;;
;; page.scm -- implement Page stuff.
;;
;; source file of the GNU LilyPond music typesetter
;;
;; (c) 2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
;;

(define-module (scm page)

  #:export (make-page
	    page-property
	    page-set-property!
	    page-prev
	    page-height
	    page-lines
	    page-force 
	    page-penalty
	    page-configuration
	    page-lines
	    page-page-number
	    page-system-numbers
	    page-stencil
	    page? 
	    ))

(use-modules (lily)
	     (scm paper-system)
	     (srfi srfi-1))


(define (annotate? layout)
  (eq? #t (ly:output-def-lookup layout 'annotatespacing)))


(define page-module (current-module))

(define (make-page . args)
  (apply ly:make-prob (append
		       (list 'page '())
		       args)))

(define page-property ly:prob-property)
(define (page-property? page sym)
  (eq? #t (page-property page sym)))

(define (page? x)  (ly:prob-type? x 'page))

(define page-set-property! ly:prob-set-property!)

;; define accessors. 
(for-each
 (lambda (j)
   (module-define!
    page-module
    (string->symbol (format "page-~a" j))
    (lambda (pg)
      (page-property pg j))))
 
 '(page-number prev lines force penalty configuration lines))

(define (page-system-numbers node)
  (map (lambda (ps) (ly:prob-property ps 'number))
       (page-lines node)))



(define (annotate-page stencil layout)
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

(define (page-header-or-footer page dir)
    (let*
      ((p-book (page-property page 'paper-book))
       (layout (ly:paper-book-paper p-book))
       (scopes (ly:paper-book-scopes p-book))
       (lines (page-lines page))
       (offsets (page-configuration page))
       (number (page-page-number page))
       (last? (page-property page 'is-last))

       )
       
      (page-headfoot layout scopes number
		(if (= dir UP)
		    'make-header
		    'make-footer)
		(if (= dir UP)
		    'headsep
		    'footsep)
		dir last?)))

(define (page-footer page)
  (page-header-or-footer page UP))

(define (page-header page)
  (page-header-or-footer page DOWN))

(define (make-page-stencil page)
  "Construct a stencil representing the page from LINES.

 Offsets is a list of increasing numbers. They must be negated to
create offsets.
 "

  (let*
      ((p-book (page-property page 'paper-book))
       (layout (ly:paper-book-paper p-book))
       (scopes (ly:paper-book-scopes p-book))
       (lines (page-lines page))
       (offsets (page-configuration page))
       (number (page-page-number page))
       (last? (page-property page 'is-last))

       (topmargin (ly:output-def-lookup layout 'topmargin))

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

       (head (page-header page))
       
       (foot (page-footer page))

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
              


(define (page-stencil page)
  (if (not (ly:stencil? (page-property page 'stencil)))

      ;; todo: make tweakable.
      ;; via property + callbacks.
      
      (page-set-property! page 'stencil (make-page-stencil page)))
  (page-property page 'stencil))

(define (page-height page)
  "Printable area for music and titles; matches default-page-make-stencil."
  (let*
      ((p-book (page-property page 'paper-book))
       (layout (ly:paper-book-paper p-book))
       (scopes (ly:paper-book-scopes p-book))
       (number (page-page-number page))
       (last? (page-property page 'is-last))
       (h (- (ly:output-def-lookup layout 'vsize)
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
