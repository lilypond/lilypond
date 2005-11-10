;;;; page-layout.scm -- page breaking and page layout
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;;
;;;; (c) 2004--2005 Jan Nieuwenhuizen <janneke@gnu.org>
;;;;          Han-Wen Nienhuys <hanwen@cs.uu.nl>

(use-modules (oop goops describe)
	     (oop goops))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class <optimally-broken-page-node> ()
  (prev #:init-value '() #:accessor node-prev #:init-keyword #:prev)
  (page #:init-value 0 #:accessor node-page-number #:init-keyword #:pageno)
  (force #:init-value 0 #:accessor node-force #:init-keyword #:force)
  (penalty #:init-value 0 #:accessor node-penalty #:init-keyword #:penalty)
  (configuration #:init-value '() #:accessor node-configuration #:init-keyword #:configuration)
  (lines #:init-value 0 #:accessor node-lines #:init-keyword #:lines))

(define-method (display (node <optimally-broken-page-node>) port)
  (map (lambda (x) (display x port))
       (list
	"Page " (node-page-number node)
	" Lines: " (node-lines node)
	" Penalty " (node-penalty node)
	"\n")))

(define-method (node-system-numbers (node <optimally-broken-page-node>))
  (map (lambda (ps) (ly:paper-system-property ps 'number))
       (node-lines node)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (annotate? layout)
  (eq? #t (ly:output-def-lookup layout 'annotatespacing)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (paper-system-staff-extents ps)
  (ly:paper-system-property ps 'refpoint-Y-extent '(0 . 0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ANNOTATIONS
;;
;; annotations are arrows indicating the numerical value of
;; spacing variables 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (annotate-y-interval layout name extent is-length?)
  ;; do something sensible for 0,0 intervals. 
  (set! extent (interval-widen extent 0.001))
  (let*
      ((text-props (cons
		    '((font-size . -3)
		      (font-family . typewriter))
		    (layout-extract-page-properties layout)))
       (annotation (interpret-markup
		    layout text-props
		    (make-column-markup
		     (list
		      (make-whiteout-markup (make-simple-markup name))
		      (make-whiteout-markup
		       (make-simple-markup
			(if is-length?
			    (format "~$" (interval-length extent))
			    (format "(~$,~$)" (car extent)
				    (cdr extent)))))))))
       (arrows
	(ly:stencil-translate-axis 
	 (dimension-arrows (cons 0 (interval-length extent)))
	 (interval-start extent) Y)))

    (set! annotation
	  (ly:stencil-aligned-to annotation Y CENTER))
    
    (set! annotation (ly:stencil-translate annotation
			  (cons 0 (interval-center extent))))

    (set! annotation
	  (ly:stencil-combine-at-edge arrows X RIGHT annotation 0.5 0))

    (set! annotation
	  (ly:make-stencil (ly:stencil-expr annotation)
			   (ly:stencil-extent annotation X)
			   (cons 10000 -10000)))
    annotation))

(define (paper-system-annotate-last system layout)
  (let*
      ((bottomspace (ly:paper-system-property system 'bottom-space))
       (y-extent (paper-system-extent system Y))
       (x-extent (paper-system-extent system X))
       (stencil (ly:paper-system-property system 'stencil))
     
       (arrow (if (number? bottomspace)
	       (annotate-y-interval layout
				    "bottom-space"
				    (cons (- (car y-extent) bottomspace)
					  (car y-extent))
				    #t)
	       #f)))
    
    (if arrow
	(set! stencil
	      (ly:stencil-add stencil arrow)))

    (set! (ly:paper-system-property system 'stencil)
	  stencil)
  ))
  
(define (paper-system-annotate system layout)
  "Add arrows and texts to indicate which lengths are set."
  (let*
      ((annotations (ly:make-stencil '() (cons 0 2) (cons 0 0)))
       (append-stencil
	(lambda (a b)
	  (ly:stencil-combine-at-edge a X RIGHT b 0.5 0)))

       (annotate-property
	(lambda (name extent is-length?)
	  (set! annotations
		(append-stencil annotations
				(annotate-y-interval layout
						     name extent is-length?)))))

       (bbox-extent (paper-system-extent system Y))
       (refp-extent (ly:paper-system-property system 'refpoint-Y-extent))
       (next-space (ly:paper-system-property system 'next-space
					     (ly:output-def-lookup layout 'betweensystemspace)
					     ))
       (next-padding (ly:paper-system-property system 'next-padding
					       (ly:output-def-lookup layout 'betweensystempadding)
					       ))
		     
       )

    (if (number-pair? bbox-extent)
	(begin
	  (annotate-property  "Y-extent"
			       bbox-extent #f)
	  (annotate-property  "next-padding"
			     (interval-translate (cons (- next-padding) 0) (car bbox-extent))
			     #t)))
    
    ;; titles don't have a refpoint-Y-extent.
    (if (number-pair? refp-extent)
	(begin
	  (annotate-property "refpoint-Y-extent"
			     refp-extent #f)
	
	  (annotate-property "next-space"
			     (interval-translate (cons (- next-space) 0) (car refp-extent))
		       #t)))
	
    

    (set! (ly:paper-system-property system 'stencil)
	  (ly:stencil-add
	   (ly:paper-system-property system 'stencil)
	   (ly:make-stencil
	    (ly:stencil-expr annotations)
	    (ly:stencil-extent empty-stencil X)
	    (ly:stencil-extent empty-stencil Y)
	    )))
    
    ))

(define (annotate-page layout stencil)
  (let*
      ((topmargin (ly:output-def-lookup layout 'topmargin))
       (vsize (ly:output-def-lookup layout 'vsize))
       (bottommargin (ly:output-def-lookup layout 'bottommargin))
       (add-stencil (lambda (y)
		      (set! stencil
			    (ly:stencil-add stencil y))
		      )))

    (add-stencil
     (ly:stencil-translate-axis 
      (annotate-y-interval layout "vsize"
			   (cons (- vsize) 0)
			   #t)
      1 X))
    

    (add-stencil
     (ly:stencil-translate-axis 
      (annotate-y-interval layout "topmargin"
			   (cons (- topmargin) 0)
			   #t)
      2 X))
    
    (add-stencil
     (ly:stencil-translate-axis 
      (annotate-y-interval layout "bottommargin"
			   (cons (- vsize) (- bottommargin vsize))
			   #t)
      2 X))
    
    stencil))

(define (annotate-space-left page-stencil layout bottom-edge)
  (let*
      ((arrow (annotate-y-interval layout
				"space left"
				(cons (- bottom-edge)  (car (ly:stencil-extent page-stencil Y)))
				#t)))
    
    (set! arrow (ly:stencil-translate-axis arrow 8 X))
    (ly:stencil-add page-stencil arrow)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (page-headfoot layout scopes number
		       sym separation-symbol dir last?)
  "Create a stencil including separating space."

  (let* ((header-proc (ly:output-def-lookup layout sym))
	 (sep (ly:output-def-lookup layout separation-symbol))
	 (stencil (ly:make-stencil "" '(0 . 0) '(0 . 0)))
	 (head-stencil
	  (if (procedure? header-proc)
	      (header-proc layout scopes number last?)
	      #f))
	 )
    
    (if (and (number? sep)
	     (ly:stencil? head-stencil)
	     (not (ly:stencil-empty? head-stencil)))

	(begin
	  (set! head-stencil
		(ly:stencil-combine-at-edge
		 stencil Y dir head-stencil
		 sep 0.0))

	  
	  ;; add arrow markers 
	  (if (annotate? layout)
	      (set! head-stencil
		    (ly:stencil-add
		     (ly:stencil-translate-axis
		      (annotate-y-interval layout 
					   (symbol->string separation-symbol)
					   (cons (min 0 (* dir sep))
						 (max 0 (* dir sep)))
					   #t)
		      (/ (ly:output-def-lookup layout 'linewidth) 2)
		      X)
		     (if (= dir UP)
			 (ly:stencil-translate-axis
			  (annotate-y-interval layout
					      "pagetopspace"
					      (cons
					       (- (min 0 (* dir sep))
						  (ly:output-def-lookup layout 'pagetopspace))
					       (min 0 (* dir sep)))
					      #t)
			  (+ 7 (interval-center (ly:stencil-extent head-stencil X))) X)
			 empty-stencil
			 )
		     head-stencil
		     ))
	      )))

    head-stencil))

(define-public (default-page-music-height layout scopes number last?)
  "Printable area for music and titles; matches default-page-make-stencil."
  (let* ((h (- (ly:output-def-lookup layout 'vsize)
	       (ly:output-def-lookup layout 'topmargin)
	       (ly:output-def-lookup layout 'bottommargin)))
	 
       (head (page-headfoot layout scopes number 'make-header 'headsep UP last?))
       (foot (page-headfoot layout scopes number 'make-footer 'footsep DOWN last?))
       (available
	(- h (if (ly:stencil? head)
		 (interval-length (ly:stencil-extent head Y))
		 0)
	   (if (ly:stencil? foot)
	       (interval-length (ly:stencil-extent foot Y))
	       0))))

    ;; (display (list "\n available" available head foot))
    available))

(define-public (default-page-make-stencil
		 lines offsets layout scopes number last?)
  "Construct a stencil representing the page from LINES.

 Offsets is a list of increasing numbers. They must be negated to
create offsets.
 "

  (let* ((topmargin (ly:output-def-lookup layout 'topmargin))

       ;; TODO: naming vsize/hsize not analogous to TeX.

	 (vsize (ly:output-def-lookup layout 'vsize))
	 (hsize (ly:output-def-lookup layout 'hsize))
	 
	 (system-xoffset (ly:output-def-lookup layout 'horizontalshift 0.0))
	 (system-separator-markup (ly:output-def-lookup layout 'systemSeparatorMarkup))
	 (system-separator-stencil (if (markup? system-separator-markup)
				       (interpret-markup layout
							 (layout-extract-page-properties layout)
							 system-separator-markup)
				       #f))
	 (lmargin (ly:output-def-lookup layout 'leftmargin))
	 (leftmargin (if lmargin
			 lmargin
			 (/ (- hsize
			       (ly:output-def-lookup layout 'linewidth)) 2)))

	 (rightmargin (ly:output-def-lookup layout 'rightmargin))
	 (bottom-edge (- vsize
			 (ly:output-def-lookup layout 'bottommargin)))

	 (head (page-headfoot layout scopes number 'make-header 'headsep UP last?))
	 (foot (page-headfoot layout scopes number 'make-footer 'footsep DOWN last?))

	 (head-height (if (ly:stencil? head)
			  (interval-length (ly:stencil-extent head Y))
			  0.0))

	 (height-proc (ly:output-def-lookup layout 'page-music-height))

	 (page-stencil (ly:make-stencil '()
					(cons leftmargin hsize)
					(cons (- topmargin) 0)))
	 (last-system #f)
	 (last-y 0.0)
	 (add-to-page (lambda (stencil y)
			(set! page-stencil
			      (ly:stencil-add page-stencil
					      (ly:stencil-translate stencil
								    (cons
								     system-xoffset
								     (- 0 head-height y topmargin))

								    )))))
	 (add-system
	  (lambda (stencil-position)
	    (let* ((system (car stencil-position))
		   (stencil (paper-system-stencil system))
		   (y (cadr stencil-position))
		   (is-title (paper-system-title?
			      (car stencil-position))))
	      (add-to-page stencil y)
	      (if (and (ly:stencil? system-separator-stencil)
		       last-system
		       (not (paper-system-title? system))
		       (not (paper-system-title? last-system)))
		  (add-to-page
		   system-separator-stencil
		   (average (- last-y
			       (car (paper-system-staff-extents last-system)))
			    (- y
			       (cdr (paper-system-staff-extents system))))))
	      (set! last-system system)
	      (set! last-y y))))
	 )


    (if (annotate? layout)
	(begin
	  (for-each (lambda (sys) (paper-system-annotate sys layout))
		    lines)
	  (paper-system-annotate-last (car (last-pair lines)) layout)))
  
    
    (if #f
	(display (list
		  "leftmargin " leftmargin "rightmargin " rightmargin
		  )))

    (set! page-stencil (ly:stencil-combine-at-edge
			page-stencil Y DOWN
			(if (and
			     (ly:stencil? head)
			     (not (ly:stencil-empty? head)))
			    head
			    (ly:make-stencil "" (cons 0 0) (cons 0 0)))
			    0. 0.))

    (map add-system (zip lines offsets))

    (if (annotate? layout)
	(set!
	 page-stencil
	 (annotate-space-left page-stencil layout
			      (- bottom-edge
				 (if (ly:stencil? foot)
				     (interval-length (ly:stencil-extent foot Y))
				     0)))
	 ))

    
    (if (and (ly:stencil? foot)
	     (not (ly:stencil-empty? foot)))
	(set! page-stencil
	      (ly:stencil-add
	       page-stencil
	       (ly:stencil-translate
		foot
		(cons 0
		      (+ (- bottom-edge)
			 (- (car (ly:stencil-extent foot Y)))))))))

    (set! page-stencil
	  (ly:stencil-translate page-stencil (cons leftmargin 0)))

    ;; annotation.
    (if (annotate? layout)
	(set! page-stencil (annotate-page layout page-stencil)))
    

    page-stencil))

;;; optimal page breaking

;;; This is not optimal page breaking, this is optimal distribution of
;;; lines over pages; line breaks are a given.

;; TODO:
;;
;; - density scoring
;; - separate function for word-wrap style breaking?
;; - raggedbottom? raggedlastbottom?

(define-public (optimal-page-breaks lines paper-book)
  "Return pages as a list starting with 1st page. Each page is a list
of lines. "

  (define MAXPENALTY 1e9)
  (define paper (ly:paper-book-paper paper-book))
  (define scopes (ly:paper-book-scopes paper-book))
  (define force-equalization-factor #f)

  (define (page-height page-number last?)
    (let ((p (ly:output-def-lookup paper 'page-music-height)))

      (if (procedure? p)
	  (p paper scopes page-number last?)
	  10000)))

  (define (get-path node done)
    "Follow NODE.PREV, and return as an ascending list of pages. DONE
is what have collected so far, and has ascending page numbers."

    (if (is-a? node <optimally-broken-page-node>)
	(get-path (node-prev node) (cons node done))
	done))

  (define (combine-penalties force user best-paths)
    (let* ((prev-force (if (null? best-paths)
			   0.0
			   (node-force (car best-paths))))
	   (prev-penalty (if (null? best-paths)
			     0.0
			     (node-penalty (car best-paths))))
	 (inter-system-space (ly:output-def-lookup paper 'betweensystemspace))
	 (relative-force (/ force inter-system-space))
	 (abs-relative-force (abs relative-force)))


      (+ (* abs-relative-force (+ abs-relative-force 1))
	 prev-penalty
	 (* force-equalization-factor (/ (abs (- prev-force force))
					 inter-system-space))
	 user)))

  (define (space-systems page-height lines ragged?)
    (let* ((global-inter-system-space
	    (ly:output-def-lookup paper 'betweensystemspace))
	   (top-space
	    (ly:output-def-lookup paper 'pagetopspace))
	   (global-fixed-dist (ly:output-def-lookup paper 'betweensystempadding))
	   
	   (system-vector (list->vector
			   (append lines
				   (if (= (length lines) 1)
				       '(#f)
				       '()))))
	   (staff-extents
	    (list->vector
	     (append (map paper-system-staff-extents lines)
		     (if (= (length lines) 1)
			 '((0 . 0))
			 '()))))

	   (real-extents
	    (list->vector
	     (append
	      (map
	       (lambda (sys) (paper-system-extent sys Y)) lines)
	      (if (= (length lines) 1)
		  '((0 .  0))
		  '()))))
	   
	   (system-count (vector-length real-extents))
	   (topskip (max
		     (+
		      top-space
		      (interval-end (vector-ref staff-extents 0)))
		     (interval-end (vector-ref real-extents 0))
		     ))
	   (last-system (vector-ref system-vector (1- system-count)))
	   (bottom-space (if (ly:paper-system? last-system)
			     (ly:paper-system-property last-system 'bottom-space 0.0)
			     0.0))
	   (space-left (- page-height
			  bottom-space
			  (apply + (map interval-length
					(vector->list real-extents)))))

	   (space (- page-height
		     topskip
		     bottom-space
		     (-  (interval-start
			  (vector-ref real-extents (1- system-count))))))

	   (calc-spring
	    (lambda (idx)
	      (let* (
		     (upper-system (vector-ref system-vector idx))
		     (between-space (ly:paper-system-property upper-system 'next-space
							      global-inter-system-space))
		     (fixed-dist (ly:paper-system-property upper-system 'next-padding
							   global-fixed-dist))
		     
		     (this-system-ext (vector-ref staff-extents idx))
		     (next-system-ext (vector-ref staff-extents (1+ idx)))
		     (fixed (max 0 (- (+ (interval-end next-system-ext)
					 fixed-dist)
				      (interval-start this-system-ext))))
		     (title1? (and (vector-ref system-vector idx)
				   (paper-system-title? (vector-ref system-vector idx)
							     )))
		     (title2? (and
			       (vector-ref system-vector (1+ idx))
			       (paper-system-title? (vector-ref system-vector (1+ idx)))))
		     (ideal (+
			     (cond
			      ((and title2? title1?)
			       (ly:output-def-lookup paper 'betweentitlespace))
			      (title1?
			       (ly:output-def-lookup paper 'aftertitlespace))
			      (title2?
			       (ly:output-def-lookup paper 'beforetitlespace))
			      (else between-space))
			     fixed))
		     (hooke (/ 1 (- ideal fixed))))
		(list ideal hooke))))

	   (springs (map calc-spring (iota (1- system-count))))
	   (calc-rod
	    (lambda (idx)
	      (let* (
		     (upper-system (vector-ref system-vector idx))
		     (fixed-dist (ly:paper-system-property upper-system 'next-padding
							   global-fixed-dist))
		     (this-system-ext (vector-ref real-extents idx))
		     (next-system-ext (vector-ref real-extents (1+ idx)))
		     
		     (distance (max  (- (+ (interval-end next-system-ext)
					   fixed-dist)
					(interval-start this-system-ext)
					) 0))
		     (entry (list idx (1+ idx) distance)))
		entry)))
	   (rods (map calc-rod (iota (1- system-count))))

	   ;; we don't set ragged based on amount space left.
	   ;; raggedbottomlast = ##T is much more predictable
	   (result (ly:solve-spring-rod-problem
		    springs rods space
		    ragged?))

	   (force (car result))
	   (positions
	    (map (lambda (y)
		   (+ y topskip))
		 (cdr  result))))

      (if #f ;; debug.
	  (begin
	    (display (list "\n# systems: " system-count
			   "\nreal-ext" real-extents "\nstaff-ext" staff-extents
			   "\ninterscore" global-inter-system-space
			   "\nspace-left" space-left
			   "\nspring,rod" springs rods
			   "\ntopskip " topskip
			   " space " space
			   "\npage-height" page-height
			   "\nragged" ragged?
			   "\nforce" force
			   "\nres" (cdr result)
			   "\npositions" positions "\n"))))

      (cons force positions)))

  (define (walk-paths done-lines best-paths current-lines  last? current-best)
    "Return the best optimal-page-break-node that contains
CURRENT-LINES.  DONE-LINES.reversed ++ CURRENT-LINES is a consecutive
ascending range of lines, and BEST-PATHS contains the optimal breaks
corresponding to DONE-LINES.

CURRENT-BEST is the best result sofar, or #f."


    (let* ((this-page-num (if (null? best-paths)
                              (ly:output-def-lookup paper 'firstpagenumber)
                              (1+ (node-page-number (car best-paths)))))

	   (ragged-all? (eq? #t (ly:output-def-lookup paper 'raggedbottom)))
	   (ragged-last? (eq? #t (ly:output-def-lookup paper 'raggedlastbottom)))
	   (ragged? (or ragged-all?
			(and ragged-last?
			     last?)))
           (page-height (page-height this-page-num last?))
	   (vertical-spacing (space-systems page-height current-lines ragged?))
	   (satisfied-constraints (car vertical-spacing))
           (force (if satisfied-constraints
		      (if (and last? ragged-last?)
			  0.0
			  satisfied-constraints)
		      10000))
	   (positions (cdr vertical-spacing))
	   (get-break-penalty (lambda (sys)
				(ly:paper-system-property sys 'penalty 0.0)))
	   (user-nobreak-penalties
	    (-
	     (apply + (filter negative?
			      (map get-break-penalty
				   (cdr current-lines))))))
           (user-penalty
	    (+
	     (max (get-break-penalty (car current-lines)) 0.0)
	     user-nobreak-penalties))
	   
           (total-penalty (combine-penalties
                           force user-penalty
			   best-paths))

           (better? (or
                     (not current-best)
                     (< total-penalty (node-penalty current-best))))
           (new-best (if better?
			 (make <optimally-broken-page-node>
			   #:prev (if (null? best-paths)
				      #f
				      (car best-paths))
			   #:lines current-lines
			   #:pageno this-page-num
			   #:force force
			   #:configuration positions
			   #:penalty total-penalty)
                         current-best)))

;;      (display total-penalty) (newline)
      (if #f ;; debug
          (display
           (list
            "\nuser pen " user-penalty
	    "\nsatisfied-constraints" satisfied-constraints
	    "\nlast? " last? "ragged?" ragged?
            "\nbetter? " better? " total-penalty " total-penalty "\n"
	    "\nconfig " positions
            "\nforce " force
	    "\nlines: " current-lines "\n")))

      (if #f ; debug
	  (display (list "\nnew-best is " (node-lines new-best)
			 "\ncontinuation of "
			 (if (null? best-paths)
			     "start"
			     (node-lines (car best-paths))))))

      (if (and (pair? done-lines)
               ;; if this page is too full, adding another line won't help
               satisfied-constraints)
          (walk-paths (cdr done-lines) (cdr best-paths)
                      (cons (car done-lines) current-lines)
                      last? new-best)
	  new-best)))

  (define (walk-lines done best-paths todo)
    "Return the best page breaking as a single
<optimal-page-break-node> for optimally breaking TODO ++
DONE.reversed. BEST-PATHS is a list of break nodes corresponding to
DONE."
    
    (if (null? todo)
	(car best-paths)
	(let* ((this-line (car todo))
	       (last? (null? (cdr todo)))
	       (next (walk-paths done best-paths (list this-line) last? #f)))

	  ;; (display "\n***************")
	  (walk-lines (cons this-line done)
		      (cons next best-paths)
		      (cdr todo)))))

  (define (line-number node)
    (ly:paper-system-property (car (node-lines node)) 'number))

  (ly:message (_ "Calculating page breaks..."))
  (set! force-equalization-factor
	(ly:output-def-lookup paper 'verticalequalizationfactor 0.3))

  (let* ((best-break-node (walk-lines '() '() lines))
	 (break-nodes (get-path best-break-node '()))
	 (last-node (car (last-pair break-nodes))))

    (define (node->page-stencil node)
      (if (not (eq? node last-node))
	  (ly:progress "["))
      (let ((stencil
	     ((ly:output-def-lookup paper 'page-make-stencil)
	      (node-lines node)
	      (node-configuration node)
	      paper
	      scopes
	      (node-page-number node)
	      (eq? node best-break-node))))
	(if (not (eq? node last-node))
	    (begin
	      (ly:progress (number->string
			    (car (last-pair (node-system-numbers node)))))
	      (ly:progress "]")))
	stencil))

    (if #f; (ly:get-option 'verbose)
	(begin
	  (display (list
		    "\nbreaks: " (map line-number break-nodes))
		   "\nsystems " (map node-lines break-nodes)
		   "\npenalties " (map node-penalty break-nodes)
		   "\nconfigs " (map node-configuration break-nodes))))

    (let ((stencils (map node->page-stencil break-nodes)))
      (ly:progress "\n")
      stencils)))
