;;; page-layout.scm -- page breaking and page layout
;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;;
;;;; (c) 2004 Jan Nieuwenhuizen <janneke@gnu.org>
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define TAGLINE
  (string-append "Engraved by LilyPond (version " (lilypond-version) ")"))

;; TODO: take <optimally-broken-page-node> iso. page-number
;; for all of these functions ?

(define-public (plain-header paper scopes page-number last?)
  "Standard header for a part: page number --outside--  and instrument--centered."

  (let* ((props (page-properties paper))
         (pnum
          (if (ly:output-def-lookup paper 'printpagenumber)
              (markup #:bold (number->string page-number))
              ""))
         (instr (ly:modules-lookup scopes 'instrument))
         
         (line (list "" (if (markup? instr) instr "") pnum)))
    
    (if (even? page-number)
        (set! line (reverse line)))
    
    (if (< (ly:output-def-lookup paper 'firstpagenumber) page-number)
        (interpret-markup
         paper props (make-fill-line-markup line))
        '())))

;; TODO: add publisher ID on non-first page.
(define-public (plain-footer paper scopes page-number last?)
  "Standard footer. Empty, save for first (copyright) and last (tagline) page."
  
  (let*
      ((props (page-properties paper))
       (copyright (ly:modules-lookup scopes 'copyright))
       (tagline-var (ly:modules-lookup scopes 'tagline))
       (tagline (if (markup? tagline-var) tagline-var TAGLINE))
       (stencil #f))

    (if last?
	(set! stencil
	      (ly:stencil-combine-at-edge
	       stencil Y DOWN (interpret-markup paper props tagline)
	       0.0
	       )))

    (if (and (= 1 page-number)
	     (markup? copyright))

	(set! stencil
	      (ly:stencil-combine-at-edge
	       stencil Y DOWN (interpret-markup paper props copyright)
	       0.0
	       )))

    stencil))
  
(define (page-headfoot paper scopes number sym sepsym dir last?)
  "Create a stencil including separating space."
  (let*
      ((header-proc (ly:output-def-lookup paper sym))
       (sep (ly:output-def-lookup paper sepsym))
       (stencil (ly:make-stencil "" '(0 . 0) '(0 . 0)))
       (head-stencil
	(if (procedure? header-proc)
	    (header-proc paper scopes number last?)
	    #f)))

    (if (and (number? sep) (ly:stencil? head-stencil))
	(set! head-stencil
	      (ly:stencil-combine-at-edge
	       stencil Y  dir head-stencil
	       sep 0.0)))

    head-stencil))

(define-public (default-page-music-height paper scopes number last?)
  "Printable area for music and titles; matches default-page-make-stencil." 
  (let*
      ((h (- (ly:output-def-lookup paper 'vsize)
	     (ly:output-def-lookup paper 'topmargin)
	     (ly:output-def-lookup paper 'bottommargin)))
       (head (page-headfoot paper scopes number 'make-header 'headsep UP last?))
       (foot (page-headfoot paper scopes number 'make-footer 'footsep DOWN last?)))
    (- h (if (ly:stencil? head)
	     (interval-length (ly:stencil-extent head Y))
	     0)
       (if (ly:stencil? foot)
	   (interval-length (ly:stencil-extent foot Y))
	   0))
    ))


(define-public (default-page-make-stencil
		 lines offsets paper scopes number last? )
  "Construct a stencil representing the page from LINES.  "
  (let*
     ((topmargin  (ly:output-def-lookup paper 'topmargin))
      
      ;; TODO: naming vsize/hsize not analogous to TeX.
      
      (vsize (ly:output-def-lookup paper 'vsize))
      (hsize (ly:output-def-lookup paper 'hsize))
      
      (lmargin (ly:output-def-lookup paper 'leftmargin))
      (leftmargin (if lmargin
                      lmargin
                      (/ (- hsize
                            (ly:output-def-lookup paper 'linewidth)) 2)))

      (rightmargin (ly:output-def-lookup paper 'rightmargin))
      (bottom-edge (- vsize
		      (ly:output-def-lookup paper 'bottommargin)))
		     
      (head (page-headfoot paper scopes number 'make-header 'headsep UP last?))
      (foot (page-headfoot paper scopes number 'make-footer 'footsep DOWN last?))

      (head-height (if (ly:stencil? head)
		       (interval-length (ly:stencil-extent head Y))
		       0.0))

      (line-stencils (map ly:paper-system-stencil lines))
      (height-proc (ly:output-def-lookup paper 'page-music-height))

      (page-stencil (ly:make-stencil '()
				     (cons leftmargin hsize)
				     (cons (- topmargin) 0)))
      (was-title #t)
      (add-system (lambda (stencil-position)
		    (set! page-stencil
			  (ly:stencil-add
			   (ly:stencil-translate-axis
			    (car stencil-position)
			    (- 0
			       head-height
			       (cadr stencil-position)
			       topmargin)
			       Y)
			   page-stencil))))
      )

    (if #f
	(display (list
		  "leftmargin" leftmargin "rightmargin" rightmargin
		  )))
    
    (set! page-stencil (ly:stencil-combine-at-edge
	  page-stencil Y DOWN head 0. 0.))

    (map add-system (zip line-stencils offsets))
    (if (ly:stencil? foot)
	(set! page-stencil
	      (ly:stencil-add
	       page-stencil
	       (ly:stencil-translate
		foot
		(cons 0
		      (+ (- bottom-edge)
			 (- (car (ly:stencil-extent foot Y)))))
		))))

    (ly:stencil-translate page-stencil (cons leftmargin 0))
  ))
  



;;; optimal page breaking

;;; This is not optimal page breaking, this is optimal distribution of
;;; lines over pages; line breaks are a given.

; TODO:
;
; - density scoring
; - separate function for word-wrap style breaking?
; - raggedbottom? raggedlastbottom? 

(define-public (ly:optimal-page-breaks
		lines paper-book)
  "Return pages as a list starting with 1st page. Each page is a list
of lines. "


  (define MAXPENALTY 1e9)
  (define bookpaper (ly:paper-book-book-paper paper-book))
  (define scopes (ly:paper-book-scopes paper-book))

  (define (page-height page-number last?)
    (let
	((p (ly:output-def-lookup bookpaper 'page-music-height)))

      (if (procedure? p)
	  (p bookpaper scopes page-number last?)
	  10000)))
  
  (define (get-path node done)
    "Follow NODE.PREV, and return as an ascending list of pages. DONE
is what have collected so far, and has ascending page numbers."
    
    (if (is-a? node <optimally-broken-page-node>)
	(get-path (node-prev node) (cons node done))
	done))

  (define (combine-penalties force user best-paths)
    (let*
	((prev-force  (if (null? best-paths)
			  0.0
			  (node-force  (car best-paths))))
	 (prev-penalty (if (null? best-paths)
			   0.0
			   (node-penalty (car best-paths))))
	 (inter-system-space (ly:output-def-lookup bookpaper 'betweensystemspace))
	 (force-equalization-factor 0.3)
	 (relative-force (/ force inter-system-space))
	 (abs-relative-force (abs relative-force))
	 )
	 
	 
    (+ (* abs-relative-force (+ abs-relative-force 1))
       prev-penalty
       (* force-equalization-factor (/ (abs (- prev-force force)) inter-system-space))
       user)))

  (define (space-systems page-height lines ragged?)
    (let*
	((inter-system-space
	  (ly:output-def-lookup bookpaper 'betweensystemspace))
	 (system-vector (list->vector
	   (append lines
		   (if (= (length lines) 1)
		       '(#f)
			'()))
	   ))

	 (staff-extents
	  (list->vector
	   (append  (map
		     ly:paper-system-staff-extents
		     lines)
		    (if (= (length lines) 1)
			'((0 .  0))
			'())) 
	   ))
	 (real-extents
	  (list->vector
	   (append
	    (map
	     (lambda (sys) (ly:paper-system-extent sys Y)) lines)
		    (if (= (length lines) 1)
			'((0 .  0))
			'()) 
		    )))
	 (no-systems (vector-length real-extents))
	 (topskip (cdr (vector-ref real-extents 0)))
	 (space-left (- page-height
			(apply + (map interval-length (vector->list real-extents)))

			))
		     
	 (space (- page-height
		   topskip
		   (-  (car (vector-ref real-extents (1- no-systems))))
		   ))

	 (fixed-dist (ly:output-def-lookup bookpaper 'betweensystempadding))
	 (calc-spring
	  (lambda (idx)
	    (let*
		((this-system-ext (vector-ref staff-extents idx))
		 (next-system-ext (vector-ref staff-extents (1+ idx)))
		 (fixed (max 0  (- (+ (cdr next-system-ext)
				      fixed-dist)
				   (car this-system-ext))))
		 (title1? (and (vector-ref system-vector idx)
			       (ly:paper-system-title? (vector-ref system-vector idx))))
		 (title2? (and
			    (vector-ref system-vector (1+ idx))
			    (ly:paper-system-title? (vector-ref system-vector (1+ idx)))))
		 (ideal (+
			 (cond
			  ((and title2? title1?)
			   (ly:output-def-lookup bookpaper 'betweentitlespace))
			  (title1?
			   (ly:output-def-lookup bookpaper 'aftertitlespace))
			  (title2?
			   (ly:output-def-lookup bookpaper 'beforetitlespace))
			  (else inter-system-space))
			 fixed))
		 (hooke (/ 1 (- ideal fixed)))
		 )
	      (list ideal hooke))
	    ))

	 (springs (map calc-spring (iota (1- no-systems))))
	 (calc-rod
	  (lambda (idx)
	    (let*
		((this-system-ext (vector-ref real-extents idx))
		 (next-system-ext (vector-ref real-extents (1+ idx)))
		 (distance (max  (- (+ (cdr next-system-ext)
				 fixed-dist)
				    (car this-system-ext)
				    ) 0)) 
		 (entry (list idx (1+ idx) distance)))
	      entry)))
	 (rods (map calc-rod (iota (1- no-systems))))

	 ;; we don't set ragged based on amount space left.
	 ;; raggedbottomlast = ##T is much more predictable
	 (result (ly:solve-spring-rod-problem
		  springs rods space
		  ragged?))

	 (force (car (ly:solve-spring-rod-problem
		      springs rods space #f)))
	 (positions
	  (map (lambda (y)
		       (+ y topskip)) 
	       (cdr  result)))
	 )
      
      (if #f ;; debug.
	  (begin
	   (display (list "\n# systems: " no-systems
			  "\nreal-ext" real-extents "\nstaff-ext" staff-extents
			  "\ninterscore" inter-system-space
			  "\nspace-letf" space-left
			  "\npage empty" page-very-empty?
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
                              (ly:output-def-lookup bookpaper 'firstpagenumber)
                              (1+ (node-page-number (car best-paths)))))

	   
	   (ragged? (or (eq? #t (ly:output-def-lookup bookpaper 'raggedbottom))
			(and (eq? #t (ly:output-def-lookup bookpaper 'raggedlastbottom))
			     last?)))
           (page-height (page-height this-page-num last?))
	   (vertical-spacing (space-systems page-height current-lines ragged?))
	   (satisfied-constraints (car vertical-spacing))
           (force (if satisfied-constraints satisfied-constraints 10000))
	   (positions (cdr vertical-spacing))
           (user-penalty (ly:paper-system-break-penalty (car current-lines)))
           (total-penalty (combine-penalties
                           force user-penalty
			   best-paths))

	   
           (better? (or
                     (not current-best)
                     (< total-penalty (node-penalty current-best))))
           (new-best (if better?
			 (make <optimally-broken-page-node>
			   #:prev  (if (null? best-paths)
                                        #f
                                        (car best-paths))
			   #:lines current-lines
			   #:pageno this-page-num
			   #:force force
			   #:configuration positions
			   #:penalty total-penalty)
                         current-best)))
      
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

;	  (display "\n***************")
	  (walk-lines (cons this-line done)
		      (cons next best-paths)
		      (cdr todo)))))

  (define (line-number node)
    (ly:paper-system-number (car (node-lines node))))

  (let* ((best-break-node (walk-lines '() '() lines))
	 (break-nodes (get-path best-break-node '())))

    (if #f; (ly:get-option 'verbose)
	(begin
	  (display (list
		    "\nbreaks: " (map line-number break-nodes))
		    "\nsystems " (map node-lines break-nodes)
		    "\npenalties " (map node-penalty break-nodes)
		    "\nconfigs " (map node-configuration break-nodes))))

    
    ; create stencils.
    
    (map (lambda (node)
	   ((ly:output-def-lookup bookpaper 'page-make-stencil)
	    (node-lines node)
	    (node-configuration node)
	    bookpaper
	    scopes
	    (node-page-number node)
	    (eq? node best-break-node)))
	 break-nodes)))


