;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2005--2020 Jan Nieuwenhuizen <janneke@gnu.org>
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

(define-public (search-executable names)
  (define (helper path lst)
    (if (null? (cdr lst))
        (car lst)
        (if (search-path path (car lst)) (car lst)
            (helper path (cdr lst)))))

  (let ((path (parse-path (getenv "PATH"))))
    (helper path names)))

(define-public (ly:gs-cli args run-str)
  (let*
      ((tmp (make-tmpfile #f))
       (tmp-name (port-filename tmp)))
    (display run-str tmp)
    (close-port tmp)
    (set! args (append args (list tmp-name)))
    (ly:system args)
    (delete-file tmp-name)
    ))

(define-public (gs-cmd-args is-eps)
  (filter string?
          (list
           (search-executable '("gs"))
           (if (not (ly:get-option 'verbose)) "-q")
           "-dNODISPLAY"
           "-dNOSAFER"
           "-dNOPAUSE"
           "-dBATCH"
           (if is-eps "-dEPSCrop")
           "-dAutoRotatePages=/None"
           "-dPrinted=false")))

(define-public (postscript->pdf paper-width paper-height
                                base-name tmp-name is-eps)
  (let* ((pdf-name (ly:format "~a.~a.pdf" tmp-name (random 1000000)))
         (flush-name (string-append pdf-name ".flush"))
         (dest (string-append base-name ".pdf"))
         (output-file (string-join (string-split pdf-name #\%) "%%"))
         (run-strings
          (filter string?
                  (list
                   (ly:format "mark /OutputFile (~a)" output-file)
                   (if (not is-eps)
                       (ly:format "/PageSize [~$ ~$]" paper-width paper-height))
                   "(pdfwrite) finddevice putdeviceprops setdevice"
                   ;; from Resource/Init/gs_pdfwr.ps; needed here because we
                   ;; do not have the pdfwrite device initially (-dNODISPLAY).
                   "newpath fill"
                   (ly:format "(~a) run" tmp-name)))
          ))

    (ly:message (_ "Converting to `~a'...\n") dest)
    ((if (ly:get-option 'gs-api) ly:gs-api ly:gs-cli)
      (gs-cmd-args is-eps) (string-join run-strings " "))

    ;; for pdfwrite, the output is only finalized once a new output
    ;; file is opened.
    (if (ly:get-option 'gs-api)
        (begin
          (ly:gs-api (gs-cmd-args is-eps)
                     (string-join
                      (list
                       (ly:format "mark /OutputFile (~a)" flush-name)
                       "(pdfwrite) finddevice putdeviceprops setdevice"
                       ;; see above
                       "newpath fill")
                       " "))
          (delete-file flush-name)))

    (rename-file pdf-name dest)
    ))

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

(define-public (mkdir-if-not-exist path . mode)
  (catch
   'system-error
   (lambda ()
     ;; mkdir:
     ;; If the directory already exists, it raises system-error.
     (if (null? mode)
         (mkdir path)
         (mkdir path (car mode)))
     #t)
   (lambda stuff
     ;; Catch the system-error
     (if (= EEXIST (system-error-errno stuff))
         ;; If the directory already exists, avoid error and return #f.
         (begin #f)
         ;; If the cause is something else, re-throw the error.
         (throw 'system-error (cdr stuff))))))

(define-public (symlink-if-not-exist oldpath newpath)
  (catch
   'system-error
   (lambda ()
     ;; symlink:
     ;; If the file already exists, it raises system-error.
     (symlink oldpath newpath)
     #t)
   (lambda stuff
     ;; Catch the system-error
     (if (= EEXIST (system-error-errno stuff))
         ;; If the file already exists, avoid error and return #f.
         (begin #f)
         ;; If the cause is something else, re-throw the error.
         (throw 'system-error (cdr stuff))))))

(define-public (close-port-rename port name)
  (let* ((tmp (port-filename port)))
    (close-port port)
    (rename-file tmp name)))

(define-public (symlink-or-copy-if-not-exist oldpath newpath)
  (if (eq? PLATFORM 'windows)
      (let ((port (create-file-exclusive newpath)))
        (if port
            (begin
              (close port)
              (copy-binary-file oldpath newpath)
              #t)
            (begin #f)))
      (symlink-if-not-exist oldpath newpath)))

(define-public (create-file-exclusive path . mode)
  (catch
   'system-error
   (lambda ()
     ;; Exclusive file create:
     ;; If the file already exists, it raises system-error.
     (if (null? mode)
         (open path (logior O_WRONLY O_CREAT O_EXCL))
         (open path (logior O_WRONLY O_CREAT O_EXCL) (car mode))))
   (lambda stuff
     ;; Catch the system-error
     (if (= EEXIST (system-error-errno stuff))
         ;; If the file already exists, avoid error and return #f.
         (begin #f)
         ;; If the cause is something else, re-throw the error.
         (throw 'system-error (cdr stuff))))))

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
      ;; TODO: this should write destination atomically.
      (copy-file from-name to-name)))

(define-public (make-tmpfile basename)
  "Returns a temp file as port. If basename is #f, a file under $TMPDIR is created."
  (if (not basename)
      (set! basename (cond
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
                             "/lilypond")))))

  (let*
      ((name (ly:format "~a-tmp-~a" basename (random 10000000)))
       (port (create-file-exclusive name #o666))
       (bport (open-file name "wb")))
    (close-port port)
    bport))


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
        (delete-file tmp-name))))

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
