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

(define (stderr string . rest)
  (apply format (cons (current-error-port) (cons string rest)))
  (force-output (current-error-port)))

;;(define pdebug stderr)
(define (pdebug . rest) #f)

(define mm-to-bigpoint
  (/ 72 25.4))

(define-public (ps-font-command font . override-coding)
  (let* ((name (ly:font-file-name font))
	 (magnify (ly:font-magnification font)))

    (string-append
     "magfont" (string-encode-integer (hashq  name 1000000))
     "m" (string-encode-integer (inexact->exact (round (* 1000 magnify)))))))

(define (tex-font? fontname)
  (or
   (equal? (substring fontname 0 2) "cm")
   (equal? (substring fontname 0 2) "ec")))

(define (ps-embed-cff body font-set-name version)
  (let* ((binary-data 
	  (string-append
	   (format "/~a ~s StartData " font-set-name (string-length body))
	   body)))
    
    (string-append
     (format
      "%!PS-Adobe-3.0 Resource-FontSet
%%DocumentNeededResources: ProcSet (FontSetInit)
%%EndComments
%%IncludeResource: ProcSet (FontSetInit)
%%BeginResource: FontSet (~a)
%%Title: (FontSet/~a)
%%Version: ~s
/FontSetInit /ProcSet findresource begin
%%BeginData: ~s Binary Bytes
"
      font-set-name font-set-name version (string-length binary-data))
     binary-data
     "\n%%EndData
%%EndResource
%%EOF
")))


(define (load-fonts paper)
  (let* ((fonts (ly:paper-fonts paper))
	 (all-font-names
	  (map
	   (lambda (font)
	     (if (string? (ly:font-file-name font))
		 (list (ly:font-file-name font))
		 (ly:font-sub-fonts font)))

	   fonts))
	 (font-names
	  (uniq-list
	   (sort (apply append all-font-names) string<?)))
	 (pfas (map
		(lambda (x)
		  (let* ((bare-file-name (ly:find-file x))
			 (cffname (string-append x ".cff"))
			 (aname (string-append x ".pfa"))
			 (bname (string-append x ".pfb"))
			 (cff-file-name (ly:find-file cffname))
			 (a-file-name (ly:kpathsea-find-file aname))
			 (b-file-name (ly:kpathsea-find-file bname)))
		    (cond
		     (bare-file-name (if (string-match "\\.pfb" bare-file-name)
					 (ly:pfb->pfa bare-file-name)
					 (ly:gulp-file bare-file-name)))
		     (cff-file-name (ps-embed-cff (ly:gulp-file cff-file-name) x 0))
		     (a-file-name (ly:gulp-file a-file-name))
		     (b-file-name (ly:pfb->pfa b-file-name))
		     (else
		      (ly:warn "cannot find CFF/PFA/PFB font ~S" x)
		      ""))))
		(filter string? font-names))))
    
    (string-join pfas "\n")))

(define (define-fonts paper)
  
  (define font-list (ly:paper-fonts paper))
  (define (define-font command fontname scaling)
    (string-append
     "/" command " { /" fontname " findfont "
     (ly:number->string scaling) " output-scale div scalefont } bind def\n"))

  (define (standard-tex-font? x)
    (or (equal? (substring x 0 2) "ms")
	(equal? (substring x 0 2) "cm")))
  
  (define (font-load-command font)
    (let* ((specced-font-name (ly:font-name font))
	   (fontname (if specced-font-name
			 specced-font-name
			 (ly:font-file-name font)))
	   (command (ps-font-command font))
	   
	   ;; FIXME -- see (ps-font-command )
	   (plain (ps-font-command font #f))
	   (designsize (ly:font-design-size font))
	   (magnification (* (ly:font-magnification font)))
	   (ops (ly:output-def-lookup paper 'outputscale))
	   (scaling (* ops magnification designsize)))

      
      ;; Bluesky pfbs have UPCASE names (sigh.)
      ;;
      (if (standard-tex-font? fontname)
	  (set! fontname (string-upcase fontname)))
      
      
      (define-font plain fontname scaling)))

  (apply string-append
	 (map (lambda (x) (font-load-command x))
	      (filter (lambda (x) (not (ly:pango-font? x)))
		      font-list))))

;; FIXME: duplicated in other output backends
;; FIXME: silly interface name
(define (output-variables layout)
  ;; FIXME: duplicates output-layout's scope-entry->string, mostly
  (define (value->string  val)
    (cond
     ((string? val) (string-append "(" val ")"))
     ((symbol? val) (symbol->string val))
     ((number? val) (number->string val))
     (else "")))

  (define (output-entry ps-key ly-key)
    (string-append
     "/" ps-key " "
     (value->string (ly:output-def-lookup layout ly-key)) " def \n"))

  (string-append
   "/lily-output-units " (number->string mm-to-bigpoint) " def %% milimeter
lily-output-units lily-output-units scale
"
   (output-entry "staff-line-thickness" 'linethickness)
   (output-entry "line-width" 'linewidth)
   (output-entry "paper-size" 'papersizename)
   (output-entry "staff-height" 'staffheight)	;junkme.
   "/output-scale "
   (number->string (ly:output-def-lookup layout 'outputscale))
   " def \n"
   (output-entry "page-height" 'vsize)
   (output-entry "page-width" 'hsize)))

(define (dump-page outputter page page-number page-count landscape?) 
  (ly:outputter-dump-string outputter
			    (string-append
			     "%%Page: "
			     (number->string page-number) " " (number->string page-count) "\n"

			     "%%BeginPageSetup\n"
			     (if landscape?
				 "page-width output-scale mul 0 translate 90 rotate\n"
				 "")
			     "%%EndPageSetup\n"

			     "start-system { "
			     "set-ps-scale-to-lily-scale "
			     "\n"))
  (ly:outputter-dump-stencil outputter page)
  (ly:outputter-dump-string outputter "} stop-system \nshowpage\n"))

(define (eps-header paper bbox)
  (string-append "%!PS-Adobe-2.0 EPSF-2.0\n"
		 "%%Creator: creator time-stamp\n"
		 "%%BoundingBox: "
		 (string-join (map ly:number->string bbox) " ") "\n"
		 "%%Orientation: "
		 (if (eq? (ly:output-def-lookup paper 'landscape) #t)
		     "Landscape\n"
		     "Portrait\n")
		 "%%EndComments\n"))

(define (page-header paper page-count)
  (string-append "%!PS-Adobe-3.0\n"
		 "%%Creator: creator time-stamp\n"
		 "%%Pages: " (number->string page-count) "\n"
		 "%%PageOrder: Ascend\n"
		 "%%Orientation: "
		 (if (eq? (ly:output-def-lookup paper 'landscape) #t)
		     "Landscape\n"
		     "Portrait\n")
		 "%%DocumentPaperSizes: "
		 (ly:output-def-lookup paper 'papersizename) "\n"))

(define (preamble paper)
  (list
   (output-variables paper)
   (ly:gulp-file "music-drawing-routines.ps")
   (ly:gulp-file "lilyponddefs.ps")
   (load-fonts paper)
   (define-fonts paper)))

(define-public (output-framework outputter book scopes fields basename)
  (let* ((paper (ly:paper-book-paper book))
	 (pages (ly:paper-book-pages book))
	 (landscape? (eq? (ly:output-def-lookup paper 'landscape) #t))
	 (page-number (1- (ly:output-def-lookup paper 'firstpagenumber)))
	 (page-count (length pages)))
    
    (for-each
     (lambda (x)
       (ly:outputter-dump-string outputter x))
     (cons
      (page-header paper page-count)
      (preamble paper)))
    
    (for-each
     (lambda (page)
       (set! page-number (1+ page-number))
       (dump-page outputter page page-number page-count landscape?))
     pages)
    
    (ly:outputter-dump-string outputter "%%Trailer\n%%EOF\n")))

(define-public (output-preview-framework outputter book scopes fields basename)
  (let* ((paper (ly:paper-book-paper book))
	 (systems (ly:paper-book-systems book))
	 (scale  (ly:output-def-lookup paper 'outputscale ))
	 (titles (take-while ly:paper-system-title? systems))
	 (non-title (find (lambda (x)
			    (not (ly:paper-system-title? x))) systems))
	 (dump-me
	  (stack-stencils Y DOWN 0.0 
			  (map ly:paper-system-stencil
			       (append titles (list non-title)))))
	 (xext (ly:stencil-extent dump-me X))
	 (yext (ly:stencil-extent dump-me Y))
	 (bbox
	  (map
	   (lambda (x)
	     (if (or (nan? x) (inf? x))
		 0.0 x))
	   (list (car xext) (car yext)
		 (cdr xext) (cdr yext)))	       ))
    
    (for-each
     (lambda (x)
       (ly:outputter-dump-string outputter x))
     (cons
      (eps-header paper
		  (map
		   (lambda (x)
		     (inexact->exact
		      (round (* x scale mm-to-bigpoint))))
		   bbox))
      (preamble paper)))


    (ly:outputter-dump-string outputter
			      (string-append "start-system { "
					     "set-ps-scale-to-lily-scale "
					     "\n"))

    (ly:outputter-dump-stencil outputter dump-me)
    (ly:outputter-dump-string outputter "} stop-system\n%%Trailer\n%%EOF\n")))

(define-public (convert-to-pdf book name)
  (let* ((defs (ly:paper-book-paper book))
	 (papersizename (ly:output-def-lookup defs 'papersizename)))

    (if (equal? name "-")
	(ly:warn "Can't convert <stdout> to PDF")
	(postscript->pdf (if (string? papersizename) papersizename "a4")
			 name))))

(define-public (convert-to-png book name)
  (let* ((defs (ly:paper-book-paper book))
	 (resolution (ly:output-def-lookup defs 'pngresolution)))

    (postscript->png (if (number? resolution) resolution

			 (ly:get-option 'resolution))
		     name)))

(define-public (convert-to-dvi book name)
  (ly:warn "Can not generate DVI via the postscript back-end"))

(define-public (convert-to-tex book name)
  (ly:warn "Can not generate TeX via the postscript back-end"))

(define-public (convert-to-ps book name)
  #t)
