;;;; framework-ps.scm -- structure for PostScript output
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;;
;;;; (c) 2004--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>

(define-module (scm framework-ps))

;;; this is still too big a mess.

(use-modules (ice-9 string-fun)
	     (guile)
	     (scm page)
	     (scm paper-system)
	     (srfi srfi-1)
	     (srfi srfi-13)
	     (scm clip-region)
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

(define (tex-font? fontname)
  (or
   (equal? (substring fontname 0 2) "cm")
   (equal? (substring fontname 0 2) "ec")))

(define (define-fonts paper)
  (define font-list (ly:paper-fonts paper))
  (define (define-font command fontname scaling)
    (string-append
      "/" command " { /" fontname " " (ly:number->string scaling) " output-scale div selectfont } bind def\n"))

  (define (font-load-command font)
    (let* ((specced-font-name (ly:font-name font))
	   (fontname (if specced-font-name
			  specced-font-name
			 (ly:font-file-name font)))
	   (command (ps-font-command font))

	   ;; FIXME -- see (ps-font-command )
	   (plain (ps-font-command font))
	   (designsize (ly:font-design-size font))
	   (magnification (* (ly:font-magnification font)))
	   (ops (ly:output-def-lookup paper 'output-scale))
	   (scaling (* ops magnification designsize)))

      (if (equal? fontname "unknown")
	  (display (list font fontname)))
      (define-font plain fontname scaling)))

  (apply string-append
	 (map (lambda (x) (font-load-command x))
	      (filter (lambda (x) (not (ly:pango-font? x)))
		      font-list))))

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
     (number->string (/ (ly:bp 1)))
     " def %% millimeter\n"
   (output-entry "staff-line-thickness" 'line-thickness)
   (output-entry "line-width" 'line-width)
   (output-entry "paper-size" 'papersizename)
   (output-entry "staff-height" 'staff-height)	;junkme.
   "/output-scale "
     (number->string (ly:output-def-lookup layout 'output-scale))
     " def\n"
   (output-entry "page-height" 'paper-height)
   (output-entry "page-width" 'paper-width)))

(define (dump-page outputter page page-number page-count landscape?)
  (ly:outputter-dump-string
   outputter
   (string-append
    (format "%%Page: ~a ~a\n" page-number page-number)
    "%%BeginPageSetup\n"
    (if landscape?
	"page-width output-scale lily-output-units mul mul 0 translate 90 rotate\n"
	"")
    "%%EndPageSetup\n"
    
    "true setstrokeadjust\n"
    "gsave 0 paper-height translate "
    "set-ps-scale-to-lily-scale "
    "\n"))
  (ly:outputter-dump-stencil outputter page)
  (ly:outputter-dump-string outputter "stroke grestore \nshowpage\n"))

(define (supplies-or-needs paper load-fonts?)
  (define (extract-names font)
    (if (ly:pango-font? font)
	(map car (ly:pango-font-physical-fonts font))
	(list  (ly:font-name font))))

  (let* ((fonts (ly:paper-fonts paper))
	 (names (apply append (map extract-names fonts))))

    (apply string-append
	   (map (lambda (f)
		  (format
		   (if load-fonts?
		    "%%DocumentSuppliedResources: font ~a\n"
		    "%%DocumentNeededResources: font ~a\n")
		   f))
		(uniq-list (sort names string<?))))))

(define (eps-header paper bbox load-fonts?)
  (string-append "%!PS-Adobe-2.0 EPSF-2.0\n"
		 "%%Creator: LilyPond "
		 (lilypond-version)
		 "\n"
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
	     80  ;; weight
	     "()" ;; color
	     "()"  ;; type
  )))


(define (file-header paper page-count load-fonts?)
  (string-append "%!PS-Adobe-3.0\n"
		 "%%Creator: LilyPond "
		 (lilypond-version)
		 "\n"
		 
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
  (format
    "%%BeginResource: procset (~a) 1 0
~a
%%EndResource
"
    file-name (cached-file-contents file-name)))

(define (embed-document file-name)
  (format "%%BeginDocument: ~a
~a
%%EndDocument
" 
    file-name (cached-file-contents file-name)))

(define (setup-variables paper)
  (string-append
   "\n"
   (define-fonts paper)
   (output-variables paper)
   ))

(define (cff-font? font)
  (let*
      ((cff-string (ly:otf-font-table-data font "CFF ")))
    (> (string-length cff-string) 0)))

(define-public (ps-embed-cff body font-set-name version)
  (let* ((binary-data
	  (string-append
	   (format "/~a ~s StartData " font-set-name (string-length body))
	   body))

	 (header
	  (format
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

    (string-append
     header
     binary-data
     footer)))


(define (write-preamble paper load-fonts? port)
  (define (internal-font? file-name)
    (or (string-startswith file-name "Emmentaler")
	(string-startswith file-name "emmentaler")
	(string-startswith file-name "aybabtu")
	(string-startswith file-name "Aybabtu")))
  (define (load-font-via-GS font-name-filename)       
    (define (ps-load-file file-name)
      (if (string? file-name)
	  (if (string-contains file-name (ly:get-option 'datadir))
	      (begin
		(set! file-name (ly:string-substitute (ly:get-option 'datadir) "" file-name))
		(format "lilypond-datadir (~a) concatstrings (r) file .loadfont" file-name))
	      
	      (format "(~a) (r) file .loadfont\n" file-name))
	  (format "% cannot find font file: ~a\n" file-name)))

    (let* ((font (car font-name-filename))
	   (name (cadr font-name-filename))
	   (file-name (caddr font-name-filename))
	   (bare-file-name (ly:find-file file-name)))

      (cons
       name
       
       (if (mac-font? bare-file-name)
	   (handle-mac-font name bare-file-name)
	   (cond
	    ((internal-font? file-name)
	     (ps-load-file (ly:find-file
			    (format "~a.otf"  file-name))))
	    ((string? bare-file-name)
	     (ps-load-file file-name))
	    (else
	     (ly:warning (_ "cannot embed ~S=~S") name file-name)
	     "")))

	  )))

  (define (dir-join a b)
    (if (equal? a "")
	b
	(string-append a "/" b)))
    
  (define (dir-listing dir-name)
    (define (dir-helper dir lst)
      (let ((e (readdir dir)))
	(if (eof-object? e) lst (dir-helper dir (cons e lst)))))
    (reverse (dir-helper (opendir dir-name) '())))
      
  (define (handle-mac-font name filename)
    (let*
	((dir-name  (tmpnam))
	 (files '())
	 (status 0)
	 (embed #f))

      (mkdir dir-name #o700)
      (set! status (ly:system
		    (format "cd ~a && fondu -force '~a'" dir-name filename)))
      
      (set! files (dir-listing dir-name))
      
      (for-each
       (lambda (f)
	 (let*
	     ((full-name  (dir-join dir-name f)))
	   
	   (if (and (not embed)
		    (equal? 'regular (stat:type (stat full-name)))
		    (equal? name (ly:ttf-ps-name full-name)))
	       
	       (set! embed
		     (font-file-as-ps-string name full-name)))
	   
	   (if (or (equal? "." f) 
		   (equal? ".." f))
	       #t
	       (delete-file full-name))))


       files)
      (rmdir dir-name)

      (if (not embed)
	  (begin
	    (set! embed "% failed \n")
	    (ly:warning (_ "cannot extract file matching ~a from ~a") name filename)))
      embed))

    (define (font-file-as-ps-string name file-name)
      (let*
	  ((downcase-file-name (string-downcase file-name)))
	
      (cond
       ((and file-name (string-endswith downcase-file-name ".pfa"))
	(embed-document file-name))
       ((and file-name (string-endswith downcase-file-name ".pfb"))
	(ly:pfb->pfa file-name))
       ((and file-name (string-endswith downcase-file-name ".ttf"))
	(ly:ttf->pfa file-name))
       ((and file-name (string-endswith downcase-file-name ".otf"))
	(ps-embed-cff (ly:otf->cff file-name) name 0))
       (else
	(ly:warning (_ "do not know how to embed ~S=~S") name file-name)
	""))))

    (define (mac-font? bare-file-name)
      (and
       (eq? PLATFORM 'darwin)
       bare-file-name
       (or
	(string-endswith  bare-file-name ".dfont")
	(= (stat:size (stat bare-file-name)) 0))))

  (define (load-font font-name-filename)
    (let* ((font (car font-name-filename))
	   (name (cadr font-name-filename))
	   (file-name (caddr font-name-filename))
	   (bare-file-name (ly:find-file file-name)))
      
      (cons
       name
       (cond

	((mac-font? bare-file-name)
	 (handle-mac-font name bare-file-name))

	((and font (cff-font? font))
	 (ps-embed-cff (ly:otf-font-table-data font "CFF ")
		       name
		       0))

	(bare-file-name (font-file-as-ps-string name bare-file-name))
	(else
	 (ly:warning (_ "do not know how to embed font ~s ~s ~s")
		     name file-name font))))))
	

  (define (load-fonts paper)
    (let* ((fonts (ly:paper-fonts paper))

	   ;; todo - doc format of list.
	   (all-font-names
	    (map
	     (lambda (font)
	       (cond
		((string? (ly:font-file-name font))
		 (list (list
			font
			(ly:font-name font)
			(ly:font-file-name font))))
		((ly:pango-font? font)
		 (map
		  (lambda (name-psname-pair)
		    (list #f
			  (car name-psname-pair)
			  (cdr name-psname-pair)))
		  (ly:pango-font-physical-fonts font)))

		(else
		 (ly:font-sub-fonts font))))

	     fonts))
	   (font-names
	    (uniq-list
	     (sort (apply append all-font-names)
		   (lambda (x y) (string<? (cadr x) (cadr y))))))

	   ;; slightly spaghetti-ish: deciding what to load where
	   ;; is smeared out.
	   (font-loader (lambda (name)
			  (cond
			   ((ly:get-option 'gs-load-fonts) 
			    (load-font-via-GS name))
			   ((ly:get-option 'gs-load-lily-fonts)
			    (if (or
				 (string-contains (caddr name) (ly:get-option 'datadir))
				 (internal-font? (caddr name)))

				(load-font-via-GS name)
				(load-font name)))
			   (else (load-font name)))))
			 
	   (pfas (map font-loader font-names)))
      pfas))

  (display "%%BeginProlog\n" port)

  (format port
	    "/lilypond-datadir where {pop} {userdict /lilypond-datadir (~a) put } ifelse"
	    (ly:get-option 'datadir))
  
  (if load-fonts?
      (for-each
       (lambda (f)
	 (format port "\n%%BeginFont: ~a\n" (car f))
	 (display (cdr f) port)
	 (display "\n%%EndFont\n" port))
       (load-fonts paper)))

  (display (setup-variables paper) port)

  ;; adobe note 5002: should initialize variables before loading routines.
  (display (procset "music-drawing-routines.ps") port)
  (display (procset "lilyponddefs.ps") port)

  (display "%%EndProlog\n" port)
  
  (display "%%BeginSetup\ninit-lilypond-parameters\n%%EndSetup\n\n" port))

(define-public (output-framework basename book scopes fields)
  (let* ((filename (format "~a.ps" basename))
	 (outputter (ly:make-paper-outputter
		     ;; FIXME: better wrap open/open-file,
		     ;; content-mangling is always bad.
		     ;; MINGW hack: need to have "b"inary for embedding CFFs
		     (open-file filename "wb")
		     'ps))
	 (paper (ly:paper-book-paper book))
	 (systems (ly:paper-book-systems book))
	 (page-stencils (map page-stencil (ly:paper-book-pages book)))
	 
	 (landscape? (eq? (ly:output-def-lookup paper 'landscape) #t))
	 (page-number (1- (ly:output-def-lookup paper 'first-page-number)))
	 (page-count (length page-stencils))
	 (port (ly:outputter-port outputter)))


    (if (ly:get-option 'clip-systems)
	(clip-system-EPSes basename book))

    (if (ly:get-option 'dump-signatures)
	(write-system-signatures basename (ly:paper-book-systems book) 1))
  
    (output-scopes scopes fields basename)
    (display (file-header paper page-count #t) port)
    
    ;; don't do BeginDefaults PageMedia: A4 
    ;; not necessary and wrong
    
    (write-preamble paper #t port)

    (for-each
     (lambda (page)
       (set! page-number (1+ page-number))
       (dump-page outputter page page-number page-count landscape?))
     page-stencils)

    (display "%%Trailer\n%%EOF\n" port)
    (ly:outputter-close outputter)
    (postprocess-output book framework-ps-module filename
			 (ly:output-formats))))

(define-public (dump-stencil-as-EPS paper dump-me filename
				    load-fonts)
  
  (let*
      ((xext (ly:stencil-extent dump-me X))
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
	   ;; bar numbers  stick out of margin uniformly.
	   ;;
	   (list
	    
	    (if (number? left-overshoot)
		(min left-overshoot (car xext))
		(car xext))
	    (car yext) (cdr xext) (cdr yext)))))

       (dump-stencil-as-EPS-with-bbox paper dump-me filename load-fonts bbox)
       ))
	 
	   
(define-public (dump-stencil-as-EPS-with-bbox paper dump-me filename
					      load-fonts
					      bbox)
  "Create an EPS file from stencil DUMP-ME to FILENAME. BBOX has format
   (left-x, lower-y, right x, up-y).  If LOAD-FONTS set, include fonts inline." 

  (define (to-rounded-bp-box box)
    "Convert box to 1/72 inch with rounding to enlarge the box."
    (let* ((scale (ly:output-def-lookup paper 'output-scale))
	   (strip-non-number (lambda (x)
			       (if (or (nan? x) (inf? x)) 0.0 x)))
	   (directed-round (lambda (x rounder)
			     (inexact->exact
			      (rounder (/ (* (strip-non-number x) scale)
					  (ly:bp 1)))))))
      (list (directed-round (car box) floor)
	    (directed-round (cadr box) floor)
	    (directed-round (max (1+ (car box)) (caddr box)) ceiling)
	    (directed-round (max (1+ (cadr box)) (cadddr box)) ceiling)
	  )))

  (let* ((outputter (ly:make-paper-outputter
		     ;; FIXME: better wrap open/open-file,
		     ;; content-mangling is always bad.
		     ;; MINGW hack: need to have "b"inary for embedding CFFs
		     (open-file (format "~a.eps" filename) "wb")
		     'ps))

	 (port (ly:outputter-port outputter))
	 (rounded-bbox (to-rounded-bp-box bbox))
	 (port (ly:outputter-port outputter))
	 (header (eps-header paper rounded-bbox load-fonts)))

    (display header port)
    (write-preamble paper load-fonts port)
    (display "gsave set-ps-scale-to-lily-scale \n" port)
    (ly:outputter-dump-stencil outputter dump-me)
    (display "stroke grestore\n%%Trailer\n%%EOF\n" port)
    (ly:outputter-close outputter)))



(define (clip-systems-to-region
	 basename paper systems region
	 do-pdf)

  (let*
      ((extents-system-pairs
	(filtered-map
	 (lambda (paper-system)
	   (let*
	       ((x-ext (system-clipped-x-extent
			(paper-system-system-grob paper-system)
			region)))

	     (if x-ext
		 (cons x-ext paper-system)
		 #f)))
	 
	 systems))
       (count 0))
    
    (for-each
     (lambda (ext-system-pair)
       (let*
	   ((xext (car ext-system-pair))
	    (paper-system (cdr ext-system-pair))
	    (yext (paper-system-extent paper-system Y))
	    (bbox (list (car xext) (car yext)
			(cdr xext) (cdr yext)))
	    (filename (if (< 0 count)
			  (format "~a-~a" basename count)
			  basename)))

	 (set! count (1+ count))
	 (dump-stencil-as-EPS-with-bbox
	  paper
	  (paper-system-stencil paper-system)
	  filename
	  (ly:get-option 'include-eps-fonts)
	  bbox)

	 (if do-pdf
	     (postscript->pdf  0 0  (format "~a.eps" filename)))
	 ))

     extents-system-pairs)
    ))


(define-public (clip-system-EPSes basename paper-book)
  (define do-pdf (member  "pdf" (ly:output-formats)))

  (define (clip-score-systems basename systems)
    (let*
	((layout (ly:grob-layout (paper-system-system-grob (car systems))))
	 (regions (ly:output-def-lookup layout 'clip-regions)))
      
      (for-each
       (lambda (region)
	 (clip-systems-to-region
	  (format "~a-from-~a-to-~a-clip"
		  basename
		  (rhythmic-location->file-string (car region))
		  (rhythmic-location->file-string (cdr region)))
	  layout systems region
	  do-pdf))
       
       regions)))
  

  ;; partition in system lists sharing their layout blocks
  (let*
      ((systems (ly:paper-book-systems paper-book))
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

    (for-each
     (lambda (system-list)
       (clip-score-systems
	(if (> count 0)
	    (format "~a-~a" basename count)
	    basename)
	system-list))

     score-system-list)))


(define-public (output-preview-framework basename book scopes fields)
  (let* ((paper (ly:paper-book-paper book))
	 (systems (ly:paper-book-systems book))
	 (scale (ly:output-def-lookup paper 'output-scale))
	 (to-dump-systems '()))

    ;; skip booktitles.
    (if (and
	 (not (ly:get-option 'include-book-title-preview))
	 (pair? systems)
	 (ly:prob-property (car systems) 'is-book-title #f))

	(set! systems (cdr systems)))

    (for-each
     (lambda (sys)
       (if (or
	    (paper-system-title? sys)
	    (not (pair? to-dump-systems))
	    (paper-system-title? (car to-dump-systems)))
	   (set! to-dump-systems (cons sys to-dump-systems))))
     systems)

    (dump-stencil-as-EPS
     paper
     (stack-stencils Y DOWN 0.0
		     (map paper-system-stencil (reverse to-dump-systems)))
     (format "~a.preview" basename)
     #t)

    (postprocess-output book framework-ps-module
			(format "~a.preview.eps" basename)
			(cons "png" (ly:output-formats)))))

(if #f
    (define-public (output-preview-framework basename book scopes fields)

      (let* ((paper (ly:paper-book-paper book))
	     (systems (ly:paper-book-systems book))
	     (scale (ly:output-def-lookup paper 'output-scale))
	     (titles (take-while paper-system-title? systems))
	     (non-title (find (lambda (x)
				(not (paper-system-title? x))) systems))
	     (dump-me
	      (stack-stencils Y DOWN 0.0
			      (map paper-system-stencil
				   (append titles (list non-title))))))
	(output-scopes scopes fields basename)
	(dump-stencil-as-EPS paper dump-me
			     (format "~a.preview" basename)
			     #t)

	(postprocess-output book framework-ps-module
			    (format "~a.preview.eps" basename)
			     (ly:output-formats)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (convert-to-pdf book name)
  (let* ((defs (ly:paper-book-paper book))
	 (landscape (ly:output-def-lookup defs 'landscape))
	 (output-scale (ly:output-def-lookup defs 'output-scale))
	 (convert (lambda (x) (* x output-scale (/ (ly:bp 1)))))
	 
	 (paper-width (convert (ly:output-def-lookup defs 'paper-width)))
	 (paper-height (convert (ly:output-def-lookup defs 'paper-height)))

	 (w (if landscape paper-height paper-width))
	 (h (if landscape paper-width paper-height))
	 )

    (if (equal? (basename name ".ps") "-")
	(ly:warning (_ "cannot convert <stdout> to ~S" "PDF"))
	(postscript->pdf w h name))))

(define-public (convert-to-png book name)
  (let* ((defs (ly:paper-book-paper book))
	 (defs-resolution (ly:output-def-lookup defs 'pngresolution))
	 (resolution (if (number? defs-resolution)
			 defs-resolution
			 (ly:get-option 'resolution)))
	 (paper-width (ly:output-def-lookup defs 'paper-width))
	 (paper-height (ly:output-def-lookup defs 'paper-height))
	 (output-scale (ly:output-def-lookup defs 'output-scale)))

    (postscript->png resolution
		     (* paper-width output-scale (/ (ly:bp 1)))
		     (* paper-height output-scale (/ (ly:bp 1)))
		     name)))

(define-public (convert-to-dvi book name)
  (ly:warning (_ "cannot generate ~S using the postscript back-end") "DVI"))

(define-public (convert-to-tex book name)
  (ly:warning (_ "cannot generate ~S using the postscript back-end") "TeX"))

(define-public (convert-to-ps book name)
  #t)

(define-public (output-classic-framework basename book scopes fields)

  (ly:error (_ "\nThe PostScript backend does not support the system-by-system 
output. For that, use the EPS backend instead,

  lilypond -dbackend=eps FILE

If have cut & pasted a lilypond fragment from a webpage, be sure
to only remove anything before

  %% ****************************************************************
  %% Start cut-&-pastable-section
  %% ****************************************************************
")))
