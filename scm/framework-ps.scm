;;;; framework-ps.scm --
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;;
;;;; (c)  2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>

(define-module (scm framework-ps))

(use-modules (ice-9 regex)
	     (ice-9 string-fun)
	     (ice-9 format)
	     (guile)
	     (srfi srfi-1)
	     (srfi srfi-13)
	     (lily))

(define mm-to-bigpoint
  (/ 72 25.4))

(define-public (ps-font-command font . override-coding)
  (let* ((name (ly:font-filename font))
	 (magnify (ly:font-magnification font))
	 (coding-alist (ly:font-encoding-alist font))
	 (input-encoding (assoc-get 'input-name coding-alist))
	 (font-encoding (assoc-get 'output-name coding-alist))
	 (coding-command (if (null? override-coding)
			     (if (equal? input-encoding font-encoding)
				 #f font-encoding)
			     (car override-coding))))

    ;; FIXME:  now feta stuff has feta* input-encoding (again?)
    ;;(format (current-error-port) "FONT: ~S, ~S\n" name font-encoding)
    ;;(format (current-error-port) "INPUT: ~S\n" input-encoding)
    (if (and coding-command
	     (or
	      (equal? (substring coding-command 0 4) "feta")
	      (equal? (substring coding-command 0 8) "parmesan")

	     ))
	(set! coding-command #f))

    (string-append
     "magfont" (string-encode-integer (hashq  name 1000000))
     "m" (string-encode-integer (inexact->exact (round (* 1000 magnify))))
     (if (not coding-command) "" (string-append "e" coding-command)))))

(define (tex-font? fontname)
  (or
   (equal? (substring fontname 0 2) "cm")
   (equal? (substring fontname 0 2) "ec")))

(define (load-fonts bookpaper)
  (let* ((fonts (ly:bookpaper-fonts bookpaper))
	 (font-names (uniq-list (sort (map ly:font-filename fonts) string<?)))
	 (pfas (map
		(lambda (x)
		
		(let*
		    ((aname (string-append x ".pfa"))
		     (apath (ly:kpathsea-expand-path aname))

		     (bpath (if (not apath)
				(ly:kpathsea-expand-path (string-append x ".pfb"))
				#f)))
		     
			    
		  (cond
		   (apath (ly:gulp-file apath))
		   (bpath (ly:pfb->pfa bpath))
		   (else
		    (ly:warn "Can't find PFA font ~S" x)
		    ""))))
		(filter string? font-names))))
  
    (string-join pfas "\n")))

(define (define-fonts bookpaper)

  (define font-list (ly:bookpaper-fonts bookpaper))
  (define (define-font command fontname scaling)
    (string-append
     "/" command " { /" fontname " findfont "
     (ly:number->string scaling) " output-scale div scalefont } bind def\n"))

  (define (reencode-font plain encoding command)
    (let ((coding-vector (get-coding-command encoding)))
      (string-append
       plain " " coding-vector " /" command " reencode-font\n"
       "/" command "{ /" command " findfont 1 scalefont } bind def\n")))

  (define (guess-ps-fontname basename)

    "We do not have the FontName, try to guess is from basename."
    (cond
     (#t basename)
     ((tex-font? basename)
      ;; FIXME: we need proper Fontmap for the bluesky CM*, EC* fonts.
      ;; Only the fonts that we trace in mf/ are in our own FontMap.
      (string-append basename ".pfb"))
     (else (string-append basename ".pfa"))))

  (define (font-load-command font)
    (let* ((specced-font-name (ly:font-name font))
	   (fontname (if specced-font-name
			 specced-font-name
			 (guess-ps-fontname (ly:font-filename font))))
	
	   (coding-alist (ly:font-encoding-alist font))
	   (input-encoding (assoc-get 'input-name coding-alist))
	   (font-encoding (assoc-get 'output-name coding-alist))
	   (command (ps-font-command font))
	   ;; FIXME -- see (ps-font-command )
	   (plain (ps-font-command font #f))
	   (designsize (ly:font-design-size font))
	   (magnification (* (ly:font-magnification font)))
	   (ops (ly:output-def-lookup bookpaper 'outputscale))
	   (scaling (* ops magnification designsize)))

      (string-append
       (define-font plain fontname scaling)
       (if (or (equal? input-encoding font-encoding)
	       ;; guh
	       (equal? font-encoding "fetaBraces")
	       (equal? font-encoding "fetaNumber")
	       (equal? font-encoding "fetaMusic")
	       (equal? font-encoding "parmesanMusic"))
	       ""
	   (reencode-font plain input-encoding command)))))

  (define (font-load-encoding encoding)
    (let ((filename (get-coding-filename encoding)))
      (ly:gulp-file (ly:kpathsea-expand-path filename))))

  (let* ((encoding-list (map (lambda (x)
			       (assoc-get 'input-name
					  (ly:font-encoding-alist x)))
			     font-list))
	 (encodings (uniq-list (sort-list (filter string? encoding-list)
					  string<?))))

    (string-append
     (apply string-append (map font-load-encoding encodings))
     (apply string-append
	    (map (lambda (x) (font-load-command x)) font-list)))))

;; FIXME: duplicated in other output backends
;; FIXME: silly interface name
(define (output-variables paper)
  ;; FIXME: duplicates output-paper's scope-entry->string, mostly
  (define (value->string  val)
    (cond
     ((string? val) (string-append "(" val ")"))
     ((symbol? val) (symbol->string val))
     ((number? val) (number->string val))
     (else "")))

  (define (output-entry ps-key ly-key)
    (string-append
     "/" ps-key " "
     (value->string (ly:output-def-lookup paper ly-key)) " def \n"))

  (string-append
   "/lily-output-units " (number->string mm-to-bigpoint) "  def  %% milimeter \n"
   (output-entry "staff-line-thickness" 'linethickness)
   (output-entry "line-width" 'linewidth)
   (output-entry "paper-size" 'papersize)
   (output-entry "staff-height" 'staffheight)	;junkme.
   "/output-scale "
   (number->string (ly:output-def-lookup paper 'outputscale))
   " lily-output-units mul def \n"))

(define (dump-page outputter page page-number page-count)
  (ly:outputter-dump-string outputter
   (string-append
    "%%Page: "
    (number->string page-number) " " (number->string page-count) "\n"
    "0 0 start-system { "
    "set-ps-scale-to-lily-scale "
    "\n"))
  (ly:outputter-dump-stencil outputter page)
  (ly:outputter-dump-string outputter "} stop-system \nshowpage\n"))

(define (eps-header bookpaper bbox)
  (string-append "%!PS-Adobe-2.0 EPSF-2.0\n"
		 "%%Creator: creator time-stamp\n"
		 "%%BoundingBox: " (string-join (map number->string  bbox) " ") "\n"
		 "%%EndComments\n"))

(define (page-header bookpaper page-count)
  (string-append "%!PS-Adobe-3.0\n"
		 "%%Creator: creator time-stamp\n"
		 "%%Pages: " (number->string page-count) "\n"
		 "%%PageOrder: Ascend\n"
		 "%%DocumentPaperSizes: " (ly:output-def-lookup bookpaper 'papersize) "\n"))

(define (preamble bookpaper)
  (list
   (output-variables bookpaper)
   (ly:gulp-file "music-drawing-routines.ps")
   (ly:gulp-file "lilyponddefs.ps")
   (load-fonts bookpaper)
   (define-fonts bookpaper)))

(define-public (output-framework outputter book scopes fields basename)
  (let* ((bookpaper (ly:paper-book-book-paper book))
	 (pages (ly:paper-book-pages book))
	 (page-number 0)
	 (page-count (length pages)))
  (for-each
   (lambda (x)
     (ly:outputter-dump-string outputter x))
   (cons
    (page-header bookpaper page-count)
    (preamble bookpaper)))
  
  (for-each
   (lambda (page)
     (set! page-number (1+ page-number))
     (dump-page outputter page page-number page-count))
   pages)
  (ly:outputter-dump-string outputter "%%Trailer\n%%EOF\n")))

(define-public (output-preview-framework outputter book scopes fields basename)
  (let* ((bookpaper (ly:paper-book-book-paper book))
	 (systems (ly:paper-book-lines book))
	 (scale  (ly:output-def-lookup bookpaper 'outputscale ))
	 (titles (take-while ly:paper-system-title? systems))
	 (non-title (find (lambda (x) (not (ly:paper-system-title? x))) systems))
	 (dump-me
	  (stack-stencils Y DOWN 0.0 
			  (map ly:paper-system-stencil
			       (append titles (list non-title)))))
	 (xext (ly:stencil-extent dump-me X))
	 (yext (ly:stencil-extent dump-me Y))
	 )
    
  (for-each
   (lambda (x)
     (ly:outputter-dump-string outputter x))
   (cons
    (eps-header bookpaper
		(map
		 (lambda (x)
		   (inexact->exact
		    (round (* x scale mm-to-bigpoint))))
		 (list (car xext) (car yext)
		       (cdr xext) (cdr yext))))
    (preamble bookpaper)))


  (ly:outputter-dump-string outputter
			    (string-append "0 0 start-system { "
					   "set-ps-scale-to-lily-scale "
					   "\n"))
  (ly:outputter-dump-stencil outputter dump-me)
  (ly:outputter-dump-string outputter "} stop-system\n%%Trailer\n%%EOF\n")))

(define-public (convert-to-pdf book name)
  (let*
      ((defs (ly:paper-book-book-paper book))
       (size (ly:output-def-lookup book 'papersize)))

    (postscript->pdf (if (string? size) size "a4")
		     name)))

(define-public (convert-to-png book name)
  (let*
      ((defs (ly:paper-book-book-paper book))
       (resolution (ly:output-def-lookup defs 'pngresolution)))

    (postscript->png (if (number? resolution) resolution 90)
		     name)))

(define-public (convert-to-dvi book name)
  (ly:warn "Can not generate DVI via the postscript back-end"))

(define-public (convert-to-tex book name)
  (ly:warn "Can not generate TeX via the postscript back-end"))

(define-public (convert-to-ps book name)
  #t)

