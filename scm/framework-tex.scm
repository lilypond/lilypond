;;;; framework-tex.scm --
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;;
;;;; (c)  2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>

(define-module (scm framework-tex)
  #:export (output-framework-tex	
	    output-classic-framework-tex))

(use-modules (ice-9 regex)
	     (ice-9 string-fun)
	     (ice-9 format)
	     (guile)
	     (srfi srfi-1)
	     (srfi srfi-13)
	     (lily))

(define-public (sanitize-tex-string s)
  (if (ly:get-option 'safe)
      (regexp-substitute/global
       #f "\\\\"
       (regexp-substitute/global #f "([{}])" s 'pre  "\\" 1 'post)
       'pre "$\\backslash$" 'post)
      s))

(define (symbol->tex-key sym)
  (regexp-substitute/global
   #f "_" (sanitize-tex-string (symbol->string sym)) 'pre "X" 'post))

(define (tex-number-def prefix key number)
  (string-append
   "\\def\\" prefix (symbol->tex-key key) "{" number "}%\n"))

(define-public (digits->letters str)
  (regexp-substitute/global
   #f "[-\\._]"
   (regexp-substitute/global
    #f "([0-9])" str
    'pre
    (lambda (match)
      (make-string
       1
       (integer->char
	(+ (char->integer #\A)
	   (- (char->integer #\0))
	   (char->integer (string-ref (match:substring match 1) 0)))
	)))
    'post)
   'pre ""
   'post))

(define-public (tex-font-command-raw name magnification)
  (string-append
   "magfont"
   (digits->letters (format "~a" name))
   "m"
   (string-encode-integer
    (inexact->exact (round (* 1000 magnification))))))

(define-public (tex-font-command font)
  (tex-font-command-raw
   (ly:font-file-name font) (ly:font-magnification font)))

(define (otf-font-load-command paper font)
  (let* ((sub-fonts (ly:font-sub-fonts font)))
    (string-append
     (apply string-append
	    (map
	     (lambda (sub-name)
	       (format #f "\\font\\~a=~a scaled ~a%\n"
		       (tex-font-command-raw
			sub-name (ly:font-magnification font))
		       sub-name
		       (ly:number->string
			(inexact->exact
			 (round (* 1000
				   (ly:font-magnification font)
				   (ly:paper-outputscale paper)))))))
	     sub-fonts)))))

(define (simple-font-load-command paper font)
  (let* ((coding-alist (ly:font-encoding-alist font))
	 (font-encoding (assoc-get 'output-name coding-alist)))
    (string-append
     "\\font\\lilypond" (tex-font-command font) "="
     (if (or (equal? (ly:font-encoding font) "cork-lm")
	     ;; FIXME: encoding: FontSpecific for cork-lm
	     (string-prefix? "lm" (ly:font-file-name font)))
	 "cork-" "")
     (ly:font-file-name font)
     " scaled "
     (ly:number->string (inexact->exact
			 (round (* 1000
				   (ly:font-magnification font)
				   (ly:paper-outputscale paper)))))
     "\n"
     "\\def\\" (tex-font-command font) "{%\n"
     ;; UGH.  Should be handled via alist.
     (if (or (equal? "Extended-TeX-Font-Encoding---Latin" font-encoding)
	     (not font-encoding))
	 "  \\lilypondfontencoding{T1}"
	 "  ")
     "\\lilypond" (tex-font-command font)
     "}%\n")))

(define (font-load-command paper font)
  (if (pair? (ly:font-sub-fonts font))
      (otf-font-load-command paper font)
      (simple-font-load-command paper font)))

(define (define-fonts paper)
  (string-append
   ;; UGH. FIXME.
   "\\def\\lilypondpaperunit{mm}%\n"
   (tex-number-def "lilypondpaper" 'outputscale
		   (number->string (exact->inexact
				    (ly:paper-outputscale paper))))
   (tex-string-def "lilypondpaper" 'papersize
		   (eval 'papersizename (ly:output-def-scope paper)))
   ;; paper/layout?
   (tex-string-def "lilypondpaper" 'inputencoding
		   (eval 'inputencoding (ly:output-def-scope paper)))

   (apply string-append
	  (map (lambda (x) (font-load-command paper x))
	       (ly:paper-fonts paper)))))

(define (header-to-file fn key val)
  (set! key (symbol->string key))
  (if (not (equal? "-" fn))
      (set! fn (string-append fn "." key)))
  (display
   (format (_ "Writing header field `~a' to `~a'...")
	   key
	   (if (equal? "-" fn) "<stdout>" fn))
   (current-error-port))
  (if (equal? fn "-")
      (display val)
      (display val (open-file fn "w")))
  (newline (current-error-port))
  "")

(define (output-scopes scopes fields basename)
  (define (output-scope scope)
    (apply
     string-append
     (module-map
      (lambda (sym var)
	(let ((val (if (variable-bound? var) (variable-ref var) "")))
	  (if (and (memq sym fields) (string? val))
	      (header-to-file basename sym val))
	  ""))
      scope)))
  (apply string-append (map output-scope scopes)))

(define (tex-string-def prefix key str)
  (if (equal? "" (sans-surrounding-whitespace (sanitize-tex-string str)))
      (string-append "\\let\\" prefix (symbol->tex-key key) "\\undefined%\n")
      (string-append "\\def\\" prefix (symbol->tex-key key)
		     "{" (sanitize-tex-string str) "}%\n")))

(define (header paper page-count classic?)
  (let ((scale (ly:output-def-lookup paper 'outputscale))
	(texpaper (string-append
		   (ly:output-def-lookup paper 'papersizename)
		   "paper"))
	(landscape? (eq? #t (ly:output-def-lookup paper 'landscape))))
    (string-append
     "% Generated by LilyPond "
     (lilypond-version) "\n"
     "% at " "time-stamp,FIXME" "\n"
     (if classic?
	 (tex-string-def "lilypond" 'classic "1")
	 "")

     (if (ly:get-option 'safe)
	 "\\nofiles\n"
	 "")

     (tex-string-def
      "lilypondpaper" 'linewidth
      (ly:number->string (* scale (ly:output-def-lookup paper 'linewidth))))
     "\\def\\lilyponddocumentclassoptions{"
     (sanitize-tex-string texpaper)
     (if landscape? ",landscape" "")
     "}%\n"
     (tex-string-def
      "lilypondpaper" 'interscoreline
      (ly:number->string
       (* scale (ly:output-def-lookup paper 'interscoreline)))))))

(define (header-end)
  (string-append
   "\\def\\scaletounit{ "
   (number->string (cond
		    ((equal? (ly:unit) "mm") (/ 72.0 25.4))
		    ((equal? (ly:unit) "pt") (/ 72.0 72.27))
		    (else (error "unknown unit" (ly:unit)))))
   " mul }%\n"
   "\\ifx\\lilypondstart\\undefined\n"
   "  \\input lilyponddefs\n"
   "\\fi\n"
   "\\lilypondstart\n"
   "\\lilypondspecial\n"
   "\\lilypondpostscript\n"))

(define (dump-page putter page last? with-extents?)
  (ly:outputter-dump-string
   putter
   (format "\\lybox{~a}{~a}{%\n"
	   (if with-extents?
	       (interval-start (ly:stencil-extent page X))
	       0.0)
	   (if with-extents?
	       (- (interval-start (ly:stencil-extent page Y)))
	       0.0)))
  (ly:outputter-dump-stencil putter page)
  (ly:outputter-dump-string
   putter
   (if last?
       "}%\n\\vfill\n"
       "}%\n\\vfill\n\\lilypondpagebreak\n")))

(define-public (output-framework outputter book scopes fields basename )
  (let* ((paper (ly:paper-book-paper book))
	 (pages (ly:paper-book-pages book))
	 (last-page (car (last-pair pages)))
	 (with-extents
	  (eq? #t (ly:output-def-lookup paper 'dump-extents))))
    (for-each
     (lambda (x)
       (ly:outputter-dump-string outputter x))
     (list
      (header paper (length pages) #f)
      (define-fonts paper)
      (header-end)))
    (ly:outputter-dump-string outputter "\\lilypondnopagebreak\n")
    (for-each
     (lambda (page)
       (dump-page outputter page (eq? last-page page) with-extents))
     pages)
    (ly:outputter-dump-string outputter "\\lilypondend\n")))

(define (dump-line putter line last?)
  (ly:outputter-dump-string
   putter
   (format "\\lybox{~a}{~a}{%\n"
	   (ly:number->string
	    (max 0 (interval-end (ly:paper-system-extent line X))))
	   (ly:number->string
	    (interval-length (ly:paper-system-extent line Y)))))

  (ly:outputter-dump-stencil putter (ly:paper-system-stencil line))
  (ly:outputter-dump-string
   putter
   (if last?
       "}%\n"
       "}\\interscoreline\n")))

(define-public (output-classic-framework
		outputter book scopes fields basename)
  (let* ((paper (ly:paper-book-paper book))
	 (lines (ly:paper-book-systems book))
	 (last-line (car (last-pair lines))))
    (for-each
     (lambda (x)
       (ly:outputter-dump-string outputter x))
     (list
      ;;FIXME
      (header paper (length lines) #f)
      "\\def\\lilypondclassic{1}%\n"
      (output-scopes scopes fields basename)
      (define-fonts paper)
      (header-end)))

    (for-each
     (lambda (line) (dump-line outputter line (eq? line last-line))) lines)
    (ly:outputter-dump-string outputter "\\lilypondend\n")))

(define-public (output-preview-framework
		outputter book scopes fields basename )
  (let* ((paper (ly:paper-book-paper book))
	 (lines (ly:paper-book-systems book))
	 (first-notes-index (list-index
			     (lambda (s) (not (ly:paper-system-title? s)))
			     lines)))

    (for-each
     (lambda (x)
       (ly:outputter-dump-string outputter x))
     (list
      
      ;;FIXME
      (header paper (length lines) #f)
      "\\def\\lilypondclassic{1}%\n"
      (output-scopes scopes fields basename)
      (define-fonts paper)
      (header-end)))

    (for-each
     (lambda (lst)
       (dump-line outputter lst (not (ly:paper-system-title? lst))))
     (take lines (1+ first-notes-index)))
    (ly:outputter-dump-string outputter "\\lilypondend\n")))

(define-public (convert-to-pdf book name)
  (let* ((defs (ly:paper-book-paper book))
	 (papersizename (ly:output-def-lookup defs 'papersizename)))
    (postscript->pdf (if (string? papersizename) papersizename "a4")
		     (string-append
		      (basename name ".tex")
		      ".ps"))))

(define-public (convert-to-png book name)
  (let* ((defs (ly:paper-book-paper book))
	 (resolution (ly:output-def-lookup defs 'pngresolution)))
    (postscript->png
     (if (number? resolution)
	 resolution
	 (ly:get-option 'resolution))
     (string-append (basename name ".tex") ".ps"))))

(define-public (convert-to-ps book name)
  (let* ((paper (ly:paper-book-paper book))
	 (preview? (string-contains name ".preview"))

	 (papersizename (ly:output-def-lookup paper 'papersizename))
	 (landscape? (eq? #t (ly:output-def-lookup paper 'landscape)))
	 (base (basename name ".tex"))
	 (cmd (string-append "dvips "
			     (if preview?
				 "-E "
				 (string-append
				  "-t"
				  ;; careful: papersizename is user-set.
				  (sanitize-command-option papersizename)
				  " "))
			     (if landscape? "-t landscape " "")
			     (if (ly:kpathsea-find-file "lm.map")
				 "-u+lm.map " "")
			     (if (ly:kpathsea-find-file "ecrm10.pfa")
				 "-u+ec-mftrace.map " "")
			     "-u+lilypond.map -Ppdf " ""
			     base)))
    (let ((ps-name (string-append base ".ps")))
      (if (access? ps-name W_OK)
	  (delete-file ps-name)))
    (if (not (ly:get-option 'verbose))
	(begin
	  (format (current-error-port) (_ "Converting to `~a.ps'...") base)
	  (newline (current-error-port))))
    (ly:system cmd)))

(define-public (convert-to-dvi book name)
  (let* ((curr-extra-mem
	  (string->number
	   (regexp-substitute/global
	    #f " *%.*\n?"
	    (ly:kpathsea-expand-variable "$extra_mem_top")
	    'pre "" 'post)))
	 (base (basename name ".tex"))
	 (cmd (string-append
	       "latex \\\\nonstopmode \\\\input " name)))
    (setenv "extra_mem_top" (number->string (max curr-extra-mem 1024000)))
    (let ((dvi-name (string-append base ".dvi")))
      (if (access? dvi-name W_OK)
	  (delete-file dvi-name)))
    (if (not (ly:get-option 'verbose))
	(begin
	  (format (current-error-port) (_ "Converting to `~a.dvi'...") base)
	  (newline (current-error-port))))

    ;; fixme: set in environment?
    (if (ly:get-option 'safe)
	(set! cmd (string-append "openout_any=p " cmd)))

    (ly:system cmd)))

(define-public (convert-to-tex book name)
  #t)
