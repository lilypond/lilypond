;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2004--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

(define-module (scm framework-ps))

;;; this is still too big a mess.

(use-modules (ice-9 string-fun)
             (guile)
             (scm page)
             (scm paper-system)
             (srfi srfi-1)
             (srfi srfi-13)
             (scm clip-region)
             (scm output-ps)
             (lily))

(define format ergonomic-simple-format)

(define framework-ps-module (current-module))

;;(define pdebug stderr)
(define (pdebug . rest) #f)

(define-public (ps-font-command font)
  (let* ((name (ly:font-file-name font))
         (magnify (ly:font-magnification font)))
    (string-append
     "magfont"
     (ly:string-substitute
      " " "_"
      (ly:string-substitute
       "/" "_"
       (ly:string-substitute
        "%" "_" name)))
     "m" (string-encode-integer (inexact->exact (round (* 1000 magnify)))))))

(define (ps-define-pango-pf pango-pf font-name scaling)
  "")

(define (ps-define-font font font-name scaling)
  (if (ly:get-option 'music-font-encodings)
      (string-append
       "/" (ps-font-command font) "-N"
       " { /" font-name "-N"
       " " (ly:number->string scaling) " output-scale div selectfont }"
       " bind def\n"
       "/" (ps-font-command font) "-S"
       " { /" font-name "-S"
       " " (ly:number->string scaling) " output-scale div selectfont }"
       " bind def\n"
       "/" (ps-font-command font) "-O"
       " { /" font-name "-O"
       " " (ly:number->string scaling) " output-scale div selectfont }"
       " bind def\n")
      (string-append
       "/" (ps-font-command font)
       " { /" font-name
       " " (ly:number->string scaling) " output-scale div selectfont }"
       " bind def\n")))

;; FIXME: duplicated in other output backends
;; FIXME: silly interface name
(define (output-variables layout)
  ;; FIXME: duplicates output-layout's scope-entry->string, mostly
  (define (value->string val)
    (cond
     ((string? val) (string-append "(" val ")"))
     ((symbol? val) (symbol->string val))
     ((number? val) (number->string val))
     (else "")))

  (define (output-entry ps-key ly-key)
    (string-append
     "/" ps-key " "
     (value->string (ly:output-def-lookup layout ly-key)) " def\n"))

  (string-append
   "/lily-output-units "
   (number->string (/ (ly:bp 1))) " def %% millimeter\n"
   (output-entry "staff-line-thickness" 'line-thickness)
   (output-entry "line-width" 'line-width)
   (output-entry "paper-size" 'papersizename)
   (output-entry "staff-height" 'staff-height)  ;junkme.
   "/output-scale "
   (number->string (ly:output-def-lookup layout 'output-scale)) " def\n"
   (output-entry "page-height" 'paper-height)
   (output-entry "page-width" 'paper-width)
   (if (ly:get-option 'strokeadjust) "true setstrokeadjust\n" "")
   ))

(define (dump-page outputter page-stencil page-number page-count landscape?)
  (ly:outputter-dump-string
   outputter
   (string-append
    (format #f "%%Page: ~a ~a\n" page-number page-number)
    "%%BeginPageSetup\n"
    (if landscape?
        "page-width output-scale lily-output-units mul mul 0 translate 90 rotate\n"
        "")
    "%%EndPageSetup\n"
    "\n"
    "gsave 0 paper-height translate set-ps-scale-to-lily-scale\n"))
  (ly:outputter-dump-stencil outputter page-stencil)
  (ly:outputter-dump-string outputter "stroke grestore\nshowpage\n"))

(define (supplies-or-needs paper load-fonts?)
  (define (extract-names font)
    (if (ly:pango-font? font)
        (map car (ly:pango-font-physical-fonts font))
        (list (ly:font-name font))))

  (let* ((fonts (ly:paper-fonts paper))
         (names (append-map extract-names fonts)))
    (string-concatenate
     (map (lambda (f)
            (format #f
                    (if load-fonts?
                        "%%DocumentSuppliedResources: font ~a\n"
                        "%%DocumentNeededResources: font ~a\n")
                    f))
          (uniq-list (sort names string<?))))))

(define (eps-header paper bbox load-fonts?)
  (string-append "%!PS-Adobe-2.0 EPSF-2.0\n"
                 "%%Creator: LilyPond " (lilypond-version) "\n"
                 "%%BoundingBox: "
                 (string-join (map ly:number->string bbox) " ") "\n"
                 "%%Orientation: "
                 (if (eq? (ly:output-def-lookup paper 'landscape) #t)
                     "Landscape\n"
                     "Portrait\n")
                 (supplies-or-needs paper load-fonts?)
                 "%%EndComments\n"))

(define (ps-document-media paper)
  (let* ((w (/ (*
                (ly:output-def-lookup paper 'output-scale)
                (ly:output-def-lookup paper 'paper-width)) (ly:bp 1)))
         (h (/ (*
                (ly:output-def-lookup paper 'paper-height)
                (ly:output-def-lookup paper 'output-scale))
               (ly:bp 1)))
         (landscape? (eq? (ly:output-def-lookup paper 'landscape) #t)))
    (ly:format "%%DocumentMedia: ~a ~2f ~2f ~a ~a ~a\n"
               (ly:output-def-lookup paper 'papersizename)
               (if landscape? h w)
               (if landscape? w h)
               80   ;; weight
               "()" ;; color
               "()" ;; type
               )))

(define (file-header paper page-count load-fonts?)
  (string-append "%!PS-Adobe-3.0\n"
                 "%%Creator: LilyPond " (lilypond-version) "\n"
                 "%%Pages: " (number->string page-count) "\n"
                 "%%PageOrder: Ascend\n"
                 "%%Orientation: "
                 (if (eq? (ly:output-def-lookup paper 'landscape) #t)
                     "Landscape\n"
                     "Portrait\n")
                 (ps-document-media paper)
                 (supplies-or-needs paper load-fonts?)
                 "%%EndComments\n"))

(define (procset file-name)
  (format #f
          "%%BeginResource: procset (~a) 1 0
~a
%%EndResource
"
          file-name (cached-file-contents file-name)))

(define (embed-document file-name)
  (format #f "%%BeginDocument: ~a
~a
%%EndDocument
"
          file-name (cached-file-contents file-name)))

(define (setup-variables paper)
  (string-append
   "\n"
   (define-fonts paper ps-define-font ps-define-pango-pf)
   (output-variables paper)))

(define never-embed-font-list (list))

(define (cff-font? font)
  (let* ((cff-string (ly:otf-font-table-data font "CFF ")))
    (> (string-length cff-string) 0)))

(define-public (ps-embed-cff body font-set-name version)
  (let* ((binary-data
          (string-append
           (format #f "/~a ~s StartData " font-set-name (string-length body))
           body))
         (header
          (format #f
                  "%%BeginResource: font ~a
%!PS-Adobe-3.0 Resource-FontSet
%%DocumentNeededResources: ProcSet (FontSetInit)
%%Title: (FontSet/~a)
%%Version: ~s
%%EndComments
%%IncludeResource: ProcSet (FontSetInit)
%%BeginResource: FontSet (~a)
/FontSetInit /ProcSet findresource begin
%%BeginData: ~s Binary Bytes
"
                  font-set-name font-set-name version font-set-name
                  (string-length binary-data)))
         (footer "\n%%EndData
%%EndResource
%%EndResource\n"))
    (begin
      (set! never-embed-font-list
            (append never-embed-font-list (list font-set-name)))
      (string-append header
                     binary-data
                     footer))))

(define check-conflict-and-embed-cff
  (let ((font-list '()))
    (lambda (name file-name font-index)
      (if name
          (let* ((name-symbol (string->symbol name))
                 (args-filename-offset
                  (cons file-name (ly:get-cff-offset file-name font-index)))
                 (found-filename-offset (assq name-symbol font-list)))
            (if found-filename-offset
                (begin
                  (if (equal? args-filename-offset (cdr found-filename-offset))
                      (ly:debug
                       (_ "CFF font `~a' already embedded, skipping.")
                       name)
                      (ly:warning
                       (_ "Different CFF fonts which have the same name `~a' has been detected. The font cannot be embedded.")
                       name))
                  "")
                (begin
                  (ly:debug (_ "Embedding CFF font `~a'.") name)
                  (set! font-list
                        (acons name-symbol args-filename-offset font-list))
                  (ps-embed-cff (ly:otf->cff file-name font-index) name 0))))
          (begin
            (ly:debug (_ "Initializing embedded CFF font list."))
            (set! font-list '()))))))

(define (initialize-font-embedding)
  (check-conflict-and-embed-cff #f #f #f))

(define (is-collection-font? file-name)
  (let* ((port (open-file file-name "rb"))
         (retval
          (if (eq? (read-char port) #\t)
              (if (eq? (read-char port) #\t)
                  (if (eq? (read-char port) #\c)
                      (if (eq? (read-char port) #\f)
                          #t
                          #f)
                      #f)
                  #f)
              #f)))
    (close-port port)
    retval))

(define (link-ps-resdir-font name file-name font-index)
  (ly:debug (_ "Preparing font ~a in PostScript resource directory\
 for subfont ~a of file `~a'...")
            name font-index file-name)
  (let* ((index (if (number? font-index) font-index 0))
         (font-format (ly:get-font-format file-name index)))
    (cond
     ((and (eq? font-format 'CFF)
           (is-collection-font? file-name))
      ;; OpenType/CFF Collection (OTC)
      (let* ((newpath (if (ly:has-glyph-names? file-name index)
                          (format #f "~a/Font/~a"
                                  (ly:get-option 'font-ps-resdir) name)
                          (format #f "~a/CIDFont/~a"
                                  (ly:get-option 'font-ps-resdir) name)))
             (port (create-file-exclusive newpath)))
        (if (not port)
            (ly:debug (_ "File `~a' already exists, skipping...") newpath)
            (begin
              (close port)
              (ly:extract-subfont-from-collection file-name index newpath)))))
     ((eq? font-format 'TrueType)
      ;; TrueType fonts (TTF) and TrueType Collection (TTC)
      (ly:debug (_ "Font ~a is TrueType font, skipping...") name))
     ((or (eq? font-format (string->symbol "Type 1"))
          (and (eq? font-format 'CFF)
               (ly:has-glyph-names? file-name 0)))
      ;; Type 1 (PFA and PFB) fonts and non-CID OpenType/CFF fonts (OTF)
      (let ((newpath (format #f "~a/Font/~a"
                             (ly:get-option 'font-ps-resdir) name)))
        (if (not (symlink-or-copy-if-not-exist file-name newpath))
            (ly:debug (_ "File `~a' already exists, skipping...") newpath))))
     ((eq? font-format 'CFF)
      ;; CID OpenType/CFF fonts (OTF)
      (let ((newpath (format #f "~a/CIDFont/~a"
                             (ly:get-option 'font-ps-resdir) name)))
        (if (not (symlink-or-copy-if-not-exist file-name newpath))
            (ly:debug (_ "File `~a' already exists, skipping...") newpath))))
     (else
      (ly:warning (_ "Font ~a cannot be used in PostScript resource directory\
 because it is unknown format." name))))))

(define (write-preamble paper load-fonts? port)
  (define (internal-font? font-name-filename)
    (let* ((font (car font-name-filename))
           (file-name (caddr font-name-filename))
           (font-file-name (ly:find-file (format #f "~a.otf" file-name))))
      (and font
           (cff-font? font)
           font-file-name
           (string-contains font-file-name
                            (ly:get-option 'datadir)))))

  (define (load-font-via-GS font-name-filename)
    (define (ps-load-file file-name name)
      (if (string? file-name)
          (begin
            (if (ly:get-option 'font-ps-resdir)
                (link-ps-resdir-font name file-name 0))
            (if (string-contains file-name (ly:get-option 'datadir))
                (begin
                  (set! file-name (ly:string-substitute
                                   (ly:get-option 'datadir)
                                   "" file-name))
                  (format #f
                          "lilypond-datadir (~a) concatstrings (r) file .loadfont\n"
                          file-name))
                (format #f "(~a) (r) file .loadfont\n" file-name)))
          (format #f "% cannot find font file: ~a\n" file-name)))

    (let* ((font (car font-name-filename))
           (name (cadr font-name-filename))
           (file-name (caddr font-name-filename))
           (font-index (cadddr font-name-filename))
           (bare-file-name (ly:find-file file-name)))
      (cond
       ((and (number? font-index)
             (!= font-index 0))
        (ly:warning (_ "Font ~a cannot be loaded via Ghostscript because its font-index (~a) is not zero.")
                    name font-index)
        (load-font font-name-filename))
       ((and (string? bare-file-name)
             (eq? (ly:get-font-format bare-file-name font-index) 'CFF)
             (is-collection-font? bare-file-name))
        (ly:warning (_ "Font ~a cannot be loaded via Ghostscript because it is an OpenType/CFF Collection (OTC) font.")
                    name)
        (load-font font-name-filename))
       ((and (string? bare-file-name)
             (eq? (ly:get-font-format bare-file-name font-index) 'TrueType)
             (not (ly:has-glyph-names? bare-file-name font-index)))
        (ly:warning (_ "Font ~a cannot be used via Ghostscript because it is a TrueType font that does not have glyph names.")
                    name)
        (load-font font-name-filename))
       (else
        (begin
          (if (or (and font (cff-font? font))
                  (and (string? bare-file-name)
                       (not (eq? (ly:get-font-format
                                  bare-file-name
                                  font-index) 'TrueType))))
              (set! never-embed-font-list
                    (append never-embed-font-list (list name))))
          (cons name
                (if (mac-font? bare-file-name)
                    (handle-mac-font name bare-file-name)
                    (cond
                     ((and font (cff-font? font))
                      (ps-load-file (ly:find-file
                                     (format #f "~a.otf" file-name)) name))
                     ((string? bare-file-name)
                      (ps-load-file file-name name))
                     (else
                      (ly:warning (_ "cannot embed ~S=~S") name file-name)
                      "")))))))))

  (define (dir-join a b)
    (if (equal? a "")
        b
        (string-append a "/" b)))

  (define (dir-listing dir-name)
    (define (dir-helper dir lst)
      (let ((e (readdir dir)))
        (if (eof-object? e)
            lst
            (dir-helper dir (cons e lst)))))
    (reverse (dir-helper (opendir dir-name) '())))

  (define (handle-mac-font name file-name)
    (let* ((dir-name (tmpnam))
           (files '())
           (status 0)
           (embed #f)
           (cwd (getcwd)))
      (mkdir dir-name #o700)
      (chdir dir-name)
      (set! status (ly:system (list "fondu" "-force" file-name)))
      (chdir cwd)
      (set! files (dir-listing dir-name))
      (for-each
       (lambda (f)
         (let* ((full-name (dir-join dir-name f)))
           (if (and (not embed)
                    (equal? 'regular (stat:type (stat full-name)))
                    (equal? name (ly:ttf-ps-name full-name)))
               (set! embed (font-file-as-ps-string name full-name 0)))
           (if (or (equal? "." f)
                   (equal? ".." f))
               #t
               (delete-file full-name))))
       files)
      (rmdir dir-name)
      (if (not embed)
          (begin
            (set! embed "% failed\n")
            (ly:warning (_ "cannot extract file matching ~a from ~a")
                        name file-name)))
      embed))

  (define (font-file-as-ps-string name file-name font-index)
    (let ((font-format (ly:get-font-format file-name font-index)))
      (if (ly:get-option 'font-ps-resdir)
          (link-ps-resdir-font name file-name font-index))
      (cond
       ((eq? font-format (string->symbol "Type 1"))
        ;; Type 1 (PFA and PFB) fonts
        (begin (set! never-embed-font-list
                     (append never-embed-font-list (list name)))
               (ly:type1->pfa file-name)))
       ((eq? font-format 'TrueType)
        ;; TrueType fonts (TTF) and TrueType Collection (TTC)
        (ly:ttf->pfa file-name font-index))
       ((eq? font-format 'CFF)
        ;; OpenType/CFF fonts (OTF) and OpenType/CFF Collection (OTC)
        (check-conflict-and-embed-cff name file-name font-index))
       (else
        (ly:warning (_ "do not know how to embed ~S=~S") name file-name)
        ""))))

  (define (mac-font? bare-file-name)
    (and (eq? PLATFORM 'darwin)
         bare-file-name
         (or (string-endswith bare-file-name ".dfont")
             (= (stat:size (stat bare-file-name)) 0))))

  (define (load-font font-psname-filename-fontindex)
    (let* ((font (list-ref font-psname-filename-fontindex 0))
           (name (list-ref font-psname-filename-fontindex 1))
           (file-name (list-ref font-psname-filename-fontindex 2))
           (font-index (list-ref font-psname-filename-fontindex 3))
           (bare-file-name (ly:find-file file-name)))
      (cons name
            (cond ((mac-font? bare-file-name)
                   (handle-mac-font name bare-file-name))
                  ((and font (cff-font? font))
                   (begin
                     (if (ly:get-option 'font-ps-resdir)
                         (link-ps-resdir-font
                          name
                          (ly:find-file
                           (format #f "~a.otf" file-name))
                          font-index))
                     (ps-embed-cff (ly:otf-font-table-data font "CFF ")
                                   name
                                   0)))
                  (bare-file-name (font-file-as-ps-string
                                   name bare-file-name font-index))
                  (else
                   (ly:warning (_ "do not know how to embed font ~s ~s ~s")
                               name file-name font))))))

  (define (load-fonts paper)
    (let* ((fonts (ly:paper-fonts paper))

           ;; todo - doc format of list.
           (all-font-names
            (map
             (lambda (font)
               (cond ((string? (ly:font-file-name font))
                      (list (list font
                                  (ly:font-name font)
                                  (ly:font-file-name font)
                                  #f)))
                     ((ly:pango-font? font)
                      (map (lambda (psname-filename-fontindex)
                             (list #f
                                   (list-ref psname-filename-fontindex 0)
                                   (list-ref psname-filename-fontindex 1)
                                   (list-ref psname-filename-fontindex 2)))
                           (ly:pango-font-physical-fonts font)))
                     (else
                      (ly:font-sub-fonts font))))
             fonts))
           (font-names (uniq-list
                        (sort (concatenate all-font-names)
                              (lambda (x y) (string<? (cadr x) (cadr y))))))

           ;; slightly spaghetti-ish: deciding what to load where
           ;; is smeared out.
           (font-loader
            (lambda (name)
              (cond ((ly:get-option 'gs-load-fonts)
                     (load-font-via-GS name))
                    ((ly:get-option 'gs-load-lily-fonts)
                     (if (or (string-contains (caddr name)
                                              (ly:get-option 'datadir))
                             (internal-font? name))
                         (load-font-via-GS name)
                         (load-font name)))
                    (else
                     (load-font name)))))
           (pfas (map font-loader font-names)))
      pfas))

  (define (font-export name body)
    (let* ((filename (format #f "~a/~a.font.ps"
                             (ly:get-option 'font-export-dir)
                             name))
           (port-excl (create-file-exclusive filename)))
      (if (not port-excl)
          (begin
            (ly:debug
             (_ "Font file `~a' already exists, skipping...")
             filename)
            #f)
          ;; MinGW hack: need to have "b"inary for fonts
          (let ((port (open-file filename "wb")))
            (close port-excl)
            (ly:debug (_ "Exporting font file `~a'.") filename)
            (if (or (ly:get-option 'gs-load-fonts)
                    (ly:get-option 'gs-load-lily-fonts))
                (begin
                  (display "%%BeginProlog\n" port)
                  (format port
                          "/lilypond-datadir where {pop} {userdict /lilypond-datadir (~a) put } ifelse\n"
                          (ly:get-option 'datadir))))
            (format port "%%BeginFont: ~a\n" name)
            (display body port)
            (display "%%EndFont\n" port)
            (if (or (ly:get-option 'gs-load-fonts)
                    (ly:get-option 'gs-load-lily-fonts))
                (display "%%EndProlog\n" port))
            (close-port port)))))

  (display "%%BeginProlog\n" port)
  (format
   port
   "/lilypond-datadir where {pop} {userdict /lilypond-datadir (~a) put } ifelse"
   (ly:get-option 'datadir))
  (set! never-embed-font-list (list))
  (if (ly:get-option 'font-ps-resdir)
      (let* ((resdir (format #f "~a" (ly:get-option 'font-ps-resdir)))
             (cidfontdir (format #f "~a/CIDFont"
                                 (ly:get-option 'font-ps-resdir)))
             (fontdir (format #f "~a/Font"
                              (ly:get-option 'font-ps-resdir))))
        (ly:debug
         (_ "Making PostScript resource directory `~a'.") resdir)
        (if (not (mkdir-if-not-exist resdir))
            (ly:debug
             (_ "PostScript resource directory `~a' already exists.") resdir))
        (ly:debug
         (_ "Making CIDFont directory `~a'.") cidfontdir)
        (if (not (mkdir-if-not-exist cidfontdir))
            (ly:debug
             (_ "CIDFont directory `~a' already exists.") cidfontdir))
        (ly:debug
         (_ "Making Font directory `~a'.") fontdir)
        (if (not (mkdir-if-not-exist fontdir))
            (ly:debug
             (_ "Font directory `~a' already exists.") fontdir))))
  (if (ly:get-option 'font-export-dir)
      (let ((dirname (format #f "~a" (ly:get-option 'font-export-dir))))
        (ly:debug
         (_ "Making font export directory `~a'.") dirname)
        (if (not (mkdir-if-not-exist dirname))
            (ly:debug
             (_ "Font export directory `~a' already exists.") dirname))))
  (if load-fonts?
      (for-each (lambda (f)
                  (format port "\n%%BeginFont: ~a\n" (car f))
                  (display (cdr f) port)
                  (display "%%EndFont\n" port)
                  (if (ly:get-option 'font-export-dir)
                      (font-export (car f) (cdr f))))
                (load-fonts paper)))
  (if (ly:get-option 'gs-never-embed-fonts)
      (begin
        (display "
/currentpagedevice where {
  pop currentpagedevice /Name known {
    currentpagedevice /Name get (pdfwrite) eq {
      << /NeverEmbed [" port)
        (display (string-concatenate
                  (map (lambda (f) (string-append " /" f))
                       never-embed-font-list)) port)
        (display " ] >> setdistillerparams
    } if
  } if
} if\n" port)))
  (if (ly:get-option 'music-font-encodings)
      (display (procset "encodingdefs.ps") port))
  (display (setup-variables paper) port)

  ;; adobe note 5002: should initialize variables before loading routines.
  (display (procset "music-drawing-routines.ps") port)
  (display (procset "lilyponddefs.ps") port)
  (display "%%EndProlog\n" port)
  (display "%%BeginSetup\ninit-lilypond-parameters\n%%EndSetup\n\n" port))

(define (ps-quote str)
  (fold
   (lambda (replacement-list result)
     (string-join
      (string-split
       result
       (car replacement-list))
      (cadr replacement-list)))
   str
   '((#\\ "\\\\")
     (#\nul "\\000") (#\cr "\\015") (#\newline "\\012")
     (#\( "\\(") (#\) "\\)"))))

;;; Create DOCINFO pdfmark containing metadata
;;; header fields with pdf prefix override those without the prefix
(define (handle-metadata header port)
  (define (metadata-encode val)
    ;; First, call ly:encode-string-for-pdf to encode the string (latin1 or
    ;; utf-16be), then escape all parentheses and backslashes
    ;;
    ;; NOTE: with guile-2.0+ ly:encode-string-for-pdf is not really needed and
    ;; could be replaced with the following code:
    ;;
    ;;    (let* ((utf16be-bom #vu8(#xFE #xFF)))
    ;;      (string-append (bytevector->string utf16be-bom "ISO-8859-1")
    ;;                     (bytevector->string (string->utf16 val 'big) "ISO-8859-1")))
    ;;
    (ps-quote (ly:encode-string-for-pdf val)))
  (define (metadata-lookup-output overridevar fallbackvar field)
    (let* ((overrideval (ly:modules-lookup (list header) overridevar))
           (fallbackval (ly:modules-lookup (list header) fallbackvar))
           (val (if overrideval overrideval fallbackval)))
      (if val
          (format port "/~a (~a)\n" field (metadata-encode (markup->string val (list header)))))))

  (if (module? header)
      (begin
        (display "mark " port)
        (metadata-lookup-output 'pdfauthor 'author "Author")
        (format port "/Creator (LilyPond ~a)\n" (lilypond-version))
        (metadata-lookup-output 'pdftitle 'title "Title")
        (metadata-lookup-output 'pdfsubject 'subject "Subject")
        (metadata-lookup-output 'pdfkeywords 'keywords "Keywords")
        (metadata-lookup-output 'pdfmodDate 'modDate "ModDate")
        (metadata-lookup-output 'pdfsubtitle 'subtitle "Subtitle")
        (metadata-lookup-output 'pdfcomposer 'composer "Composer")
        (metadata-lookup-output 'pdfarranger 'arranger "Arranger")
        (metadata-lookup-output 'pdfpoet 'poet "Poet")
        (metadata-lookup-output 'pdfcopyright 'copyright "Copyright")
        (display "/DOCINFO pdfmark\n\n" port)))

  (if (ly:get-option 'outline-bookmarks)
      ;; Presenting the outline by default in PDF viewers.
      (display "mark /PageMode /UseOutlines /DOCVIEW pdfmark\n\n" port))

  (if (ly:get-option 'embed-source-code)
      (let ((source-list (delete-duplicates
                          (remove (lambda (str)
                                    (or
                                     (string-contains str
                                                      (ly:get-option 'datadir))
                                     (string=? str
                                               "<included string>")))
                                  (ly:source-files)))))
        (display "\n/pdfmark where
{pop} {userdict /pdfmark /cleartomark load put} ifelse" port)
        (for-each (lambda (fname idx)
                    (format port "\n
mark /_objdef {ly~a_stream} /type /stream   /OBJ pdfmark
mark {ly~a_stream} << /Type /EmbeddedFile>> /PUT pdfmark
mark {ly~a_stream} (~a) /PUT pdfmark
mark /Name (LilyPond source file ~a)
/FS << /Type /Filespec /F (~a) /EF << /F {ly~a_stream} >> >> /EMBED pdfmark
mark {ly~a_stream} /CLOSE pdfmark
\n"
                            idx idx idx
                            (ps-quote (ly:gulp-file fname))
                            idx fname idx idx))
                  source-list (iota (length source-list))))))

(define (dump-pdf-bookmarks toc-alist page-numbers port)
  (let* ((sorted-page-numbers
          (sort (reverse page-numbers)
                (lambda (pairA pairB)
                  (< (cdr pairA) (cdr pairB)))))
         (remaining (map car sorted-page-numbers)))
    ;; TODO -- properly handle non-linear parent-children
    ;; relationships (within the format’s limitations) -vv
    (map
     (lambda (entry)
       (let* ((id (car entry))
              (page-number (cdr entry))
              (alist (assoc-get id toc-alist)))
         (set! remaining (cdr remaining))
         (if (and (number? page-number) alist
                  (not (memq id remaining)))
             (format
              port
              "mark /Page ~a /Title (~a) /Count ~a\
 /View [/XYZ null null 0] /Subtype /Link /OUT pdfmark\n"
              page-number
              (ps-quote (markup->string (assoc-get 'text alist)))
              (length (assoc-get 'children alist))))))
     sorted-page-numbers)))

(define-public (output-framework basename book scopes fields)
  (let* ((port (make-tmpfile basename))
         (tmp-name (port-filename port))
         (outputter (ly:make-paper-outputter port stencil-dispatch-alist))
         (paper (ly:paper-book-paper book))
         (header (ly:paper-book-header book))
         (systems (ly:paper-book-systems book))
         (page-stencils (map page-stencil (ly:paper-book-pages book)))
         (landscape? (eq? (ly:output-def-lookup paper 'landscape) #t))
         (page-number (1- (ly:output-def-lookup paper 'first-page-number)))
         (page-count (length page-stencils)))
    (cond-expand
     (guile-2 (set-port-encoding! port "Latin1"))
     (else))
    (initialize-font-embedding)
    (if (ly:get-option 'clip-systems)
        (clip-system-EPSes basename book))
    (if (ly:get-option 'dump-signatures)
        (write-system-signatures basename (ly:paper-book-systems book) 1))
    (output-scopes scopes fields basename)
    (display (file-header paper page-count #t) port)
    ;; don't do BeginDefaults PageMedia: A4
    ;; not necessary and wrong
    (write-preamble paper #t port)
    (handle-metadata header port)
    (for-each
     (lambda (page)
       (set! page-number (1+ page-number))
       (dump-page outputter page page-number page-count landscape?))
     page-stencils)
    (if (ly:get-option 'outline-bookmarks)
        (dump-pdf-bookmarks
         (ly:output-def-lookup paper 'label-alist-table)
         (ly:output-def-lookup paper 'label-page-table)
         port))
    (display "%%Trailer\n%%EOF\n" port)
    (ly:outputter-close outputter)
    (postprocess-output book framework-ps-module (ly:output-formats)
                        basename tmp-name #f)))

(define-public (dump-stencil-as-EPS paper dump-me filename
                                    load-fonts)
  (let* ((xext (ly:stencil-extent dump-me X))
         (yext (ly:stencil-extent dump-me Y))
         (padding (ly:get-option 'eps-box-padding))
         (left-overshoot (if (number? padding)
                             (* -1 padding (ly:output-def-lookup paper 'mm))
                             #f))
         (bbox
          (map
           (lambda (x)
             (if (or (nan? x) (inf? x)
                     ;; FIXME: huh?
                     (equal? (format #f "~S" x) "+#.#")
                     (equal? (format #f "~S" x) "-#.#"))
                 0.0 x))

           ;; the left-overshoot is to make sure that
           ;; bar numbers stick out of margin uniformly.
           ;;
           (list
            (if (number? left-overshoot)
                (min left-overshoot (car xext))
                (car xext))
            (car yext) (cdr xext) (cdr yext)))))
    (dump-stencil-as-EPS-with-bbox paper dump-me filename load-fonts bbox)))

(define-public (dump-stencil-as-EPS-with-bbox paper dump-me filename
                                              load-fonts
                                              bbox)
  "Create an EPS file from stencil @var{dump-me} to @var{filename}.
@var{bbox} has format @code{(left-x, lower-y, right-x, upper-y)}.  If
@var{load-fonts} set, include fonts inline."
  (define (to-rounded-bp-box box)
    "Convert box to 1/72 inch with rounding to enlarge the box."
    (let* ((scale (ly:output-def-lookup paper 'output-scale))
           (strip-non-number (lambda (x)
                               (if (or (nan? x)
                                       (inf? x))
                                   0.0
                                   x)))
           (directed-round (lambda (x rounder)
                             (inexact->exact
                              (rounder (/ (* (strip-non-number x) scale)
                                          (ly:bp 1)))))))
      (list (directed-round (car box) floor)
            (directed-round (cadr box) floor)
            (directed-round (max (1+ (car box)) (caddr box)) ceiling)
            (directed-round (max (1+ (cadr box)) (cadddr box)) ceiling))))

  (let* ((dest-name  (format #f "~a.eps" filename))
         (ignore (ly:message (_  "Layout output to `~a'...") dest-name))
         (port (make-tmpfile dest-name))
         (tmp-name (port-filename port))
         (outputter (ly:make-paper-outputter port stencil-dispatch-alist))
         (port (ly:outputter-port outputter))
         (rounded-bbox (to-rounded-bp-box bbox))
         (port (ly:outputter-port outputter))
         (header (eps-header paper rounded-bbox load-fonts)))
    (initialize-font-embedding)
    (display header port)
    (write-preamble paper load-fonts port)
    (display "/mark_page_link { pop pop pop pop pop } bind def\n" port)
    (display "gsave set-ps-scale-to-lily-scale\n" port)
    (ly:outputter-dump-stencil outputter dump-me)
    (display "stroke grestore\n%%Trailer\n%%EOF\n" port)
    (ly:outputter-close outputter)
    (ly:rename-file tmp-name dest-name)
    ))

(define (clip-systems-to-region basename paper systems region do-pdf do-png)
  (let* ((extents-system-pairs
          (filtered-map (lambda (paper-system)
                          (let* ((x-ext (system-clipped-x-extent
                                         (paper-system-system-grob paper-system)
                                         region)))
                            (if x-ext
                                (cons x-ext paper-system)
                                #f)))
                        systems))
         (count 0))
    (for-each
     (lambda (ext-system-pair)
       (let* ((xext (car ext-system-pair))
              (paper-system (cdr ext-system-pair))
              (yext (paper-system-extent paper-system Y))
              (bbox (list (car xext) (car yext)
                          (cdr xext) (cdr yext)))
              (filename (if (< 0 count)
                            (format #f "~a-~a" basename count)
                            basename)))
         (set! count (1+ count))
         (dump-stencil-as-EPS-with-bbox paper
                                        (paper-system-stencil paper-system)
                                        filename
                                        (ly:get-option 'include-eps-fonts)
                                        bbox)
         (if do-pdf
             (postscript->pdf 0 0 filename (format #f "~a.eps" filename) #t))
         (if do-png
             (postscript->png (ly:get-option 'resolution) 0 0 bbox
                              filename (format #f "~a.eps" filename) #t))))
     extents-system-pairs)))

(define-public (clip-system-EPSes basename paper-book)
  (define do-pdf
    (member "pdf" (ly:output-formats)))
  (define do-png
    (member "png" (ly:output-formats)))

  (define (clip-score-systems basename systems)
    (let* ((layout (ly:grob-layout (paper-system-system-grob (car systems))))
           (regions (ly:output-def-lookup layout 'clip-regions)))
      (for-each
       (lambda (region)
         (clip-systems-to-region
          (format #f "~a-from-~a-to-~a-clip"
                  basename
                  (rhythmic-location->file-string (car region))
                  (rhythmic-location->file-string (cdr region)))
          layout systems region
          do-pdf do-png))
       regions)))

  ;; partition in system lists sharing their layout blocks
  (let* ((systems (ly:paper-book-systems paper-book))
         (count 0)
         (score-system-list '()))
    (fold
     (lambda (system last-system)
       (if (not (and last-system
                     (equal? (paper-system-layout last-system)
                             (paper-system-layout system))))
           (set! score-system-list (cons '() score-system-list)))
       (if (paper-system-layout system)
           (set-car! score-system-list (cons system (car score-system-list))))
       ;; pass value.
       system)
     #f
     systems)
    (for-each (lambda (system-list)
                ;; filter out headers and top-level markup
                (if (pair? system-list)
                    (clip-score-systems
                     (if (> count 0)
                         (format #f "~a-~a" basename count)
                         basename)
                     system-list)))
              score-system-list)))

(define-public (output-preview-framework basename book scopes fields)
  (let* ((paper (ly:paper-book-paper book))
         (systems (relevant-book-systems book))
         (to-dump-systems (relevant-dump-systems systems)))
    (dump-stencil-as-EPS paper
                         (stack-stencils Y DOWN 0.0
                                         (map paper-system-stencil
                                              (reverse to-dump-systems)))
                         (format #f "~a.preview" basename)
                         #t)
    (postprocess-output book framework-ps-module
                        (cons "png" (ly:output-formats))
                        (format #f "~a.preview" basename)
                        (format #f "~a.preview.eps" basename)
                        #t
                        )))

(define-public (output-crop-framework basename book scopes fields)
  (let* ((paper (ly:paper-book-paper book))
         (systems (relevant-book-systems book)))
    (dump-stencil-as-EPS paper
                         (stack-stencils Y DOWN 0.0
                                         (map paper-system-stencil
                                              (reverse (reverse systems))))
                         (format #f "~a.cropped" basename)
                         #t)
    (postprocess-output book framework-ps-module
                        (cons "png" (ly:output-formats))
                        (format #f "~a.cropped" basename)
                        (format #f "~a.cropped.eps" basename)
                        #t
                        )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (output-width-height defs)
  (let* ((landscape (ly:output-def-lookup defs 'landscape))
         (output-scale (ly:output-def-lookup defs 'output-scale))
         (convert (lambda (x)
                    (* x output-scale (/ (ly:bp 1)))))
         (paper-width (convert (ly:output-def-lookup defs 'paper-width)))
         (paper-height (convert (ly:output-def-lookup defs 'paper-height)))
         (w (if landscape paper-height paper-width))
         (h (if landscape paper-width paper-height)))
    (cons w h)))

(define-public (convert-to-pdf book base-name tmp-name is-eps)
  (let* ((defs (ly:paper-book-paper book))
         (width-height (output-width-height defs))
         (width (car width-height))
         (height (cdr width-height)))
    (postscript->pdf width height base-name tmp-name is-eps)))

(define-public (convert-to-png book base-name tmp-name is-eps)
  (let* ((defs (ly:paper-book-paper book))
         (width-height (output-width-height defs))
         (width (car width-height))
         (height (cdr width-height))
         (resolution (ly:get-option 'resolution))
         ;; We access the bbox from the EPS file by scanning the first
         ;; 256 bytes.
         (header (ly:gulp-file tmp-name 256))
         (bbox (get-postscript-bbox (car (string-split header #\nul)))))
    (postscript->png resolution width height bbox base-name tmp-name is-eps)))

(define-public (convert-to-ps book base-name tmp-name is-eps)
  (postscript->ps base-name tmp-name is-eps))

(define-public (output-classic-framework basename book scopes fields)
  (ly:error (_ "\nThe PostScript backend does not support the
system-by-system output.  For that, use the EPS backend instead,

  lilypond -dbackend=eps FILE

If you have cut & pasted a lilypond fragment from a webpage, be sure
to only remove anything before

  %% ****************************************************************
  %% Start cut-&-pastable-section
  %% ****************************************************************
")))
