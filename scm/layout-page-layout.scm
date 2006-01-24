;;;; page-layout.scm -- page breaking and page layout
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;;
;;;; (c) 2004--2006 Jan Nieuwenhuizen <janneke@gnu.org>
;;;;          Han-Wen Nienhuys <hanwen@cs.uu.nl>

(use-modules (oop goops describe)
	     (oop goops)
	     (scm paper-system)
	     (scm page)
	     )






;;; optimal page breaking

;;; This is not optimal page breaking, this is optimal distribution of
;;; lines over pages; line breaks are a given.

;; TODO:
;;
;; - density scoring
;; - separate function for word-wrap style breaking?
;; - raggedbottom? raggedlastbottom?

(define-public (optimal-page-breaks lines paper-book)
  "Return pages as a list starting with 1st page. Each page is a 'page prob.

"

  (define MAXPENALTY 1e9)
  (define paper (ly:paper-book-paper paper-book))
  (define scopes (ly:paper-book-scopes paper-book))
  (define force-equalization-factor #f)
  (define (get-path node done)
    "Follow NODE.PREV, and return as an ascending list of pages. DONE
is what have collected so far, and has ascending page numbers."

    (if (page? node)
	(get-path (page-prev node) (cons node done))
	done))

  (define (combine-penalties force user best-paths)
    (let* ((prev-force (if (null? best-paths)
			   0.0
			   (page-force (car best-paths))))
	   (prev-penalty (if (null? best-paths)
			     0.0
			     (page-penalty (car best-paths))))
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
	   (bottom-space (if (ly:prob? last-system)
			     (ly:prob-property last-system 'bottom-space 0.0)
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
		     (between-space (ly:prob-property upper-system 'next-space
							      global-inter-system-space))
		     (fixed-dist (ly:prob-property upper-system 'next-padding
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
		     (fixed-dist (ly:prob-property upper-system 'next-padding
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
                              (1+ (page-page-number (car best-paths)))))

	   (this-page (make-page
		       'paper-book paper-book
		       'is-last last? 
		       'page-number this-page-num))
		       
	   (ragged-all? (eq? #t (ly:output-def-lookup paper 'raggedbottom)))
	   (ragged-last? (eq? #t (ly:output-def-lookup paper 'raggedlastbottom)))
	   (ragged? (or ragged-all?
			(and ragged-last?
			     last?)))
           (height (page-height  this-page))
	   (vertical-spacing (space-systems height current-lines ragged?))
	   (satisfied-constraints (car vertical-spacing))
           (force (if satisfied-constraints
		      (if (and last? ragged-last?)
			  0.0
			  satisfied-constraints)
		      10000))
	   (positions (cdr vertical-spacing))
	   (get-break-penalty (lambda (sys)
				(ly:prob-property sys 'penalty 0.0)))
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
                     (< total-penalty (page-penalty current-best))))
           (new-best (if better?
			 (begin
			   (map
			    (lambda (x)
			      (page-set-property! this-page
						  (car x)
						  (cdr x)))
			    (list
			     (cons 'prev (if (null? best-paths)
					     #f
					     (car best-paths)))
			     (cons 'lines current-lines)
			     (cons 'force force)
			     (cons 'configuration positions)
			     (cons 'penalty total-penalty)))
			   this-page)
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
	  (display (list "\nnew-best is " (page-lines new-best)
			 "\ncontinuation of "
			 (if (null? best-paths)
			     "start"
			     (page-lines (car best-paths))))))

      (if (and (pair? done-lines)
               ;; if this page is too full, adding another line won't help
               satisfied-constraints)
          (walk-paths (cdr done-lines) (cdr best-paths)
                      (cons (car done-lines) current-lines)
                      last? new-best)
	  new-best)))

  (define (walk-lines done best-paths todo)
    "Return the best page breaking as a single
page node for optimally breaking TODO ++
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
    (ly:prob-property (car (page-lines node)) 'number))
  
  (ly:message (_ "Calculating page breaks..."))
  (set! force-equalization-factor
	(ly:output-def-lookup paper 'verticalequalizationfactor 0.3))
  
  (let* ((best-break-node (walk-lines '() '() lines))
	 (break-nodes (get-path best-break-node '()))
	 )


    (set! (page-property (car (last-pair break-nodes)) 'is-last) #t)

    (if #f; (ly:get-option 'verbose)
	(begin
	  (display (list
		    "\nbreaks: " (map line-number break-nodes))
		   "\nsystems " (map page-lines break-nodes)
		   "\npenalties " (map page-penalty break-nodes)
		   "\nconfigs " (map page-configuration break-nodes))))

    (let ((stencils (map page-stencil break-nodes)))
      (ly:progress "\n")
      stencils)))
