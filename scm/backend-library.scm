;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2005--2015 Jan Nieuwenhuizen <janneke@gnu.org>
;;;; Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;;
;;;; LilyPond is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; LilyPond is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; backend helpers.

(use-modules (scm ps-to-png)
             (scm paper-system)
             (ice-9 optargs))

(define-public (ly:system command)
  (ly:debug (_ "Invoking `~a'...") (string-join command))
  (let ((status (apply ly:spawn command)))
    (if (> status 0)
        (begin
          (ly:warning (_ "`~a' failed (~a)\n") command status)
          ;; hmmm.  what's the best failure option?
          (throw 'ly-file-failed)))))

;; ly:system can't handle pipe and redirection.
;; This procedure can handle them by using shell.
(define-public (ly:system-with-shell command)
  (let ((s (if (eq? PLATFORM 'windows)
               ;; MinGW (except Cygwin): Use COMSPEC (cmd.exe)
               ;; FIXME: Command window is displayed briefly
               (list (or (getenv "COMSPEC")
                         "cmd.exe")
                     "/c")
               ;; POSIX (also Cygwin): Use /bin/sh
               (list "/bin/sh"
                     "-c")))
        (c (list (if (eq? PLATFORM 'windows)
                     ;; MinGW hack: Double quotes can not be used here.
                     ;; So we remove them.
                     ;; FIXME: The filename that contains space
                     ;; can't be handled.
                     (string-join (string-split command #\") "")
                     ;; Other environments (also Cygwin):
                     ;; Double quotes can be used. Pass through.
                     command
                     ))))
    (ly:system (append s c))))

(define-public (search-executable names)
  (define (helper path lst)
    (if (null? (cdr lst))
        (car lst)
        (if (search-path path (car lst)) (car lst)
            (helper path (cdr lst)))))

  (let ((path (parse-path (getenv "PATH"))))
    (helper path names)))

(define-public (search-gs)

  ;; must be sure that we don't catch stuff from old GUBs.
  (search-executable '("gs")))

(define-public (postscript->pdf paper-width paper-height
                                base-name tmp-name is-eps)
  (let* ((pdf-name (string-append base-name ".pdf"))
         (*unspecified* (if #f #f))
         (cmd
          (remove (lambda (x) (eq? x *unspecified*))
                  (list
                   (search-gs)
                   (if (ly:get-option 'verbose) *unspecified* "-q")
                   (if (or (ly:get-option 'gs-load-fonts)
                           (ly:get-option 'gs-load-lily-fonts)
                           (eq? PLATFORM 'windows))
                       "-dNOSAFER"
                       "-dSAFER")

                   (if is-eps
                       "-dEPSCrop"
                       (ly:format "-dDEVICEWIDTHPOINTS=~$" paper-width))
                   (if is-eps
                       *unspecified*
                       (ly:format "-dDEVICEHEIGHTPOINTS=~$" paper-height))
                   "-dCompatibilityLevel=1.4"
                   "-dNOPAUSE"
                   "-dBATCH"
                   "-r1200"
                   "-sDEVICE=pdfwrite"
                   "-dAutoRotatePages=/None"
                   "-dPrinted=false"
                   (string-append "-sOutputFile="
                                  (string-join
                                   (string-split pdf-name #\%)
                                   "%%"))
                   "-c.setpdfwrite"
                   (string-append "-f" tmp-name)))))

    (ly:message (_ "Converting to `~a'...\n") pdf-name)
    (ly:system cmd)))

(define-public (postscript->png resolution paper-width paper-height
                                base-name tmp-name is-eps)
  (let* ((verbose (ly:get-option 'verbose))
         (rename-page-1 #f))

    ;; Do not try to guess the name of the png file,
    ;; GS produces PNG files like BASE-page%d.png.
    (ly:message (_ "Converting to ~a...") "PNG")
    (make-ps-images base-name tmp-name is-eps
                    #:resolution resolution
                    #:page-width paper-width
                    #:page-height paper-height
                    #:rename-page-1 rename-page-1
                    #:be-verbose verbose
                    #:anti-alias-factor (ly:get-option 'anti-alias-factor)
                    #:pixmap-format (ly:get-option 'pixmap-format))
    (ly:progress "\n")))

(define-public (postscript->ps base-name tmp-name is-eps)
  (let* ((ps-name (string-append base-name
                                 (if is-eps ".eps" ".ps"))))
    (if (not (equal? ps-name tmp-name))
        (begin
          (ly:message (_ "Copying to `~a'...\n") ps-name)
          (copy-binary-file tmp-name ps-name)))))

(define-public (copy-binary-file from-name to-name)
  (if (eq? PLATFORM 'windows)
      ;; MINGW hack: MinGW Guile's copy-file is broken.
      ;; It opens files by the text mode instead of the binary mode.
      ;; (It is fixed from Guile 2.0.9.)
      ;; By the text mode, copied binary files are broken.
      ;; So, we open files by the binary mode and copy by ourselves.
      (let ((port-from (open-file from-name "rb"))
            (port-to (open-file to-name "wb")))
        (let loop((c (read-char port-from)))
          (if (eof-object? c)
              (begin (close port-from)
                     (close port-to))
              (begin (write-char c port-to)
                     (loop (read-char port-from))))))
      ;; Cygwin and other platforms:
      ;; Pass through to copy-file
      (copy-file from-name to-name)))

(define-public (make-tmpfile)
  (let* ((tmpl
          (string-append (cond
                          ;; MINGW hack: TMP / TEMP may include
                          ;; unusable characters (Unicode etc.).
                          ((eq? PLATFORM 'windows) "./tmp-")
                          ;; Cygwin can handle any characters
                          ;; including Unicode.
                          ((eq? PLATFORM 'cygwin) (string-append
                                                   (or (getenv "TMP")
                                                       (getenv "TEMP"))
                                                   "/"))
                          ;; Other platforms (POSIX platforms)
                          ;; use TMPDIR or /tmp.
                          (else (string-append
                                 (or (getenv "TMPDIR")
                                     "/tmp")
                                 "/")))
                          "lilypond-XXXXXX"))
         (port-tmp (mkstemp! tmpl)))
    (if (eq? PLATFORM 'windows)
        ;; MINGW hack: MinGW Guile's mkstemp! is broken.
        ;; It creates a file by the text mode instead of the binary mode.
        ;; (It is fixed from Guile 2.0.9.)
        ;; We need the binary mode for embeddings CFFs.
        ;; So, we re-open the same file by the binary mode.
        (let* ((filename (port-filename port-tmp))
               (port (open-file filename "r+b")))
          (close port-tmp)
          port)
        ;; Cygwin and other platforms:
        ;; Pass through the return value of mkstemp!
        port-tmp)))

(define-public (postprocess-output paper-book module formats
                                   base-name tmp-name is-eps)
  (let* ((completed (completize-formats formats is-eps)))
    (for-each (lambda (f)
                ((eval (string->symbol (format #f "convert-to-~a" f)) module)
                 paper-book base-name tmp-name is-eps)) completed)
    (if (and (ly:get-option 'delete-intermediate-files)
             (or (not is-eps)
                 (not (member "ps" completed)))
             (file-exists? tmp-name))
        (begin (ly:message (_ "Deleting `~a'...\n") tmp-name)
               (delete-file tmp-name)))))

(define-public (completize-formats formats is-eps)
  (define new-fmts '())
  (if (and is-eps (member "eps" formats))
      (set! formats (cons "ps" formats)))
  (if (not (or (member "pdf" formats)
               (member "png" formats)))
      (set! formats (cons "ps" formats)))
  (for-each (lambda (x)
              (if (member x formats) (set! new-fmts (cons x new-fmts))))
            '("ps" "pdf" "png"))
  (uniq-list (reverse new-fmts)))

(define (header-to-file file-name key value)
  (set! key (symbol->string key))
  (if (not (equal? "-" file-name))
      (set! file-name (string-append file-name "." key)))
  (ly:message (_ "Writing header field `~a' to `~a'...")
              key
              (if (equal? "-" file-name) "<stdout>" file-name))
  (if (equal? file-name "-")
      (display value)
      (let ((port (open-file file-name "w")))
        (display value port)
        (close-port port)))

  (ly:progress "\n")
  "")

(define-public (output-scopes scopes fields basename)
  (define (output-scope scope)
    (string-concatenate
     (module-map
      (lambda (sym var)
        (let ((val (if (variable-bound? var) (variable-ref var) "")))
          (if (and (memq sym fields) (string? val))
              (header-to-file basename sym val))
          ""))
      scope)))
  (string-concatenate (map output-scope scopes)))

(define-public (relevant-book-systems book)
  (let ((systems (ly:paper-book-systems book)))
    ;; skip booktitles.
    (if (and (not (ly:get-option 'include-book-title-preview))
             (pair? systems)
             (ly:prob-property (car systems) 'is-book-title #f))
        (cdr systems)
        systems)))

(define-public (relevant-dump-systems systems)
  (let ((to-dump-systems '()))
    (for-each
     (lambda (sys)
       (if (or (paper-system-title? sys)
               (not (pair? to-dump-systems))
               (paper-system-title? (car to-dump-systems)))
           (set! to-dump-systems (cons sys to-dump-systems))))
     systems)
    to-dump-systems))

(define missing-stencil-list '())

(define-public (backend-testing output-module)
  (define (missing-stencil-expression name)
    (begin
      (ly:warning (_ "missing stencil expression `~S'") name)
      ""))

  (for-each (lambda (x)
              (if (not (module-defined? output-module x))
                  (begin
                    (module-define! output-module x
                                    (lambda* (#:optional y . z)
                                             (missing-stencil-expression x)))
                    (set! missing-stencil-list (cons x missing-stencil-list)))))
            (ly:all-stencil-commands)))

(define-public (remove-stencil-warnings output-module)
  (for-each
   (lambda (x)
     (module-remove! output-module x))
   missing-stencil-list))

(define-public (font-name-split font-name)
  "Return @code{(FONT-NAME . DESIGN-SIZE)} from @var{font-name} string
or @code{#f}."
  (let ((match (regexp-exec (make-regexp "(.*)-([0-9]*)") font-name)))
    (if (regexp-match? match)
        (cons (match:substring match 1) (match:substring match 2))
        (cons font-name-designsize #f))))

;; Example of a pango-physical-font
;; ("Emmentaler-11" "/home/janneke/vc/lilypond/out/share/lilypond/current/fonts/otf/emmentaler-11.otf" 0)
(define-public (pango-pf-font-name pango-pf)
  "Return the font-name of the pango physical font @var{pango-pf}."
  (list-ref pango-pf 0))
(define-public (pango-pf-file-name pango-pf)
  "Return the file-name of the pango physical font @var{pango-pf}."
  (list-ref pango-pf 1))
(define-public (pango-pf-fontindex pango-pf)
  "Return the fontindex of the pango physical font @var{pango-pf}."
  (list-ref pango-pf 2))

(define (pango-font-name pango-font)
  (let ((pf-fonts (ly:pango-font-physical-fonts pango-font)))
    (if (pair? pf-fonts)
        (pango-pf-font-name (car pf-fonts))
        "")))

(define-public (define-fonts paper define-font define-pango-pf)
  "Return a string of all fonts used in @var{paper}, invoking the functions
@var{define-font} and @var{define-pango-pf} for producing the actual font
definition."

  (let* ((font-list (ly:paper-fonts paper))
         (pango-fonts (filter ly:pango-font? font-list))
         (other-fonts (remove ly:pango-font? font-list))
         (other-font-names (map ly:font-name other-fonts))
         (pango-only-fonts
          (remove (lambda (x)
                    (member (pango-font-name x) other-font-names))
                  pango-fonts)))

    (define (font-load-command font)
      (let* ((font-name (ly:font-name font))
             (designsize (ly:font-design-size font))
             (magnification (* (ly:font-magnification font)))
             (ops (ly:output-def-lookup paper 'output-scale))
             (scaling (* ops magnification designsize)))
        (if (equal? font-name "unknown")
            (display (list font font-name)))
        (define-font font font-name scaling)))

    (define (pango-font-load-command pango-font)
      (let* ((pf-fonts (ly:pango-font-physical-fonts pango-font))
             (pango-pf (if (pair? pf-fonts) (car pf-fonts) '("" "" 0)))
             (font-name (pango-pf-font-name pango-pf))
             (scaling (ly:output-def-lookup paper 'output-scale)))
        (if (equal? font-name "unknown")
            (display (list pango-font font-name)))
        (define-pango-pf pango-pf font-name scaling)))

    (string-append
     (string-concatenate (map font-load-command other-fonts))
     (string-concatenate (map pango-font-load-command pango-only-fonts)))))
