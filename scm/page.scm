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
	    page-printable-height
	    layout->page-init
	    page-lines
	    page-force 
	    page-penalty
	    page-configuration
	    page-lines
	    page-page-number
	    page-system-numbers
	    page-stencil
	    page-free-height
	    page? 
	    ))

(use-modules (lily)
	     (scm paper-system)
	     (srfi srfi-1))


(define (annotate? layout)
  (eq? #t (ly:output-def-lookup layout 'annotate-spacing)))


(define page-module (current-module))

(define (make-page paper-book  . args)
  (let*
      ((p (apply ly:make-prob (append
			       (list 'page (layout->page-init (ly:paper-book-paper paper-book))
				     'paper-book paper-book)
			       args))))

    (page-set-property! p 'head-stencil (page-header p))
    (page-set-property! p 'foot-stencil (page-footer p))
    
    p))
	
(define page-property ly:prob-property)
(define page-set-property! ly:prob-set-property!)
(define (page-property? page sym)
  (eq? #t (page-property page sym)))
(define (page? x)  (ly:prob-type? x 'page))


;; define accessors. 
(for-each
 (lambda (j)
   (module-define!
    page-module
    (string->symbol (format "page-~a" j))
    (lambda (pg)
      (page-property pg j))))
 
 '(page-number prev lines force penalty lines))

(define (page-system-numbers page)
  (map (lambda (ps) (ly:prob-property ps 'number))
       (page-lines page)))

(define (page-translate-systems page)
  (for-each

   (lambda (sys-off)
     (let*
	 ((sys (car sys-off))
	  (off (cadr sys-off)))

       (if (not (number? (ly:prob-property sys 'Y-offset)))
	   (ly:prob-set-property! sys 'Y-offset off))))
   
   (zip (page-property page 'lines)
	(page-property page 'configuration))))

(define (annotate-top-space first-system layout header-stencil stencil)
  (let* ((top-margin (ly:output-def-lookup layout 'top-margin))
	 (sym (if (paper-system-title? first-system)
		  'first-system-title-spacing
		  'first-system-spacing))
	 (spacing-spec (ly:output-def-lookup layout sym))
	 (X-offset (ly:prob-property first-system 'X-offset 5))
	 (header-extent (ly:stencil-extent header-stencil Y)))

    (set! stencil
	  (ly:stencil-add stencil
			  (ly:stencil-translate-axis
			   (annotate-spacing-spec layout
						  spacing-spec
						  (- top-margin)
						  (car header-extent)
						  #:base-color red)
			   X-offset X)))
    stencil))


(define (annotate-page layout stencil)
  (let ((top-margin (ly:output-def-lookup layout 'top-margin))
	(paper-height (ly:output-def-lookup layout 'paper-height))
	(bottom-margin (ly:output-def-lookup layout 'bottom-margin))
	(add-stencil (lambda (y)
		       (set! stencil
			     (ly:stencil-add stencil
					     (ly:stencil-translate-axis y 6 X))))))
    (add-stencil
     (ly:stencil-translate-axis 
      (annotate-y-interval layout "paper-height"
			   (cons (- paper-height) 0)
			   #t)
      1 X))
    (add-stencil
     (ly:stencil-translate-axis 
      (annotate-y-interval layout "top-margin"
			   (cons (- top-margin) 0)
			   #t)
      2 X))
    (add-stencil
     (ly:stencil-translate-axis 
      (annotate-y-interval layout "bottom-margin"
			   (cons (- paper-height) (- bottom-margin paper-height))
			   #t)
      2 X))
    stencil))

(define (annotate-space-left page)
  (let*
      ((paper-book (page-property page 'paper-book))
       (layout (ly:paper-book-paper paper-book))
       (arrow (annotate-y-interval layout
				   "space left"
				   (cons (- 0.0
					    (page-property page 'bottom-edge)
					    (let ((foot (page-property page 'foot-stencil)))
					      (if (and (ly:stencil? foot)
						       (not (ly:stencil-empty? foot)))
						  (car (ly:stencil-extent foot Y))
						  0.0)))
					 (page-property page  'bottom-system-edge))
				   #t)))

    (set! arrow (ly:stencil-translate-axis arrow 8 X))

    arrow))


(define (page-header-or-footer page dir)
    (let*
      ((paper-book (page-property page 'paper-book))
       (layout (ly:paper-book-paper paper-book))
       (scopes (ly:paper-book-scopes paper-book))
       (number (page-page-number page))
       (is-last-bookpart (page-property page 'is-last-bookpart))
       (is-bookpart-last-page (page-property page 'is-bookpart-last-page))
       (sym (if (= dir UP)
		'make-header
		'make-footer))
       (header-proc (ly:output-def-lookup layout sym)))

      (if (procedure? header-proc)
	  (header-proc layout scopes number is-last-bookpart is-bookpart-last-page)
	  #f)))


(define (page-header page)
  (page-header-or-footer page UP))

(define (page-footer page)
  (page-header-or-footer page DOWN))

(define (layout->page-init layout)
  "Alist of settings for page layout"
  (let*
      ((paper-height (ly:output-def-lookup layout 'paper-height))
       (paper-width (ly:output-def-lookup layout 'paper-width))
       (lmargin (ly:output-def-lookup layout 'left-margin #f))
       (left-margin (if lmargin
		       lmargin
		       (/ (- paper-width
			     (ly:output-def-lookup layout 'line-width)) 2)))
       (bottom-edge (- paper-height
		       (ly:output-def-lookup layout 'bottom-margin)) )
       (top-margin (ly:output-def-lookup layout 'top-margin))
       )
    
    `((paper-height . ,paper-height)
      (paper-width . ,paper-width)
      (left-margin . ,left-margin)
      (top-margin . ,top-margin)
      (bottom-edge . ,bottom-edge)
      )))

(define (make-page-stencil page)
  "Construct a stencil representing the page from PAGE."
  

  (page-translate-systems page)
  (let*
      ((paper-book (page-property page 'paper-book))
       (prop (lambda (sym) (page-property page sym)))
       (layout (ly:paper-book-paper paper-book))
       (scopes (ly:paper-book-scopes paper-book))
       (lines (page-lines page))
       (number (page-page-number page))

       ;; TODO: naming paper-height/paper-width not analogous to TeX.
       
       (system-xoffset (ly:output-def-lookup layout 'horizontal-shift 0.0))
       (system-separator-markup (ly:output-def-lookup layout 'system-separator-markup))
       (system-separator-stencil (if (markup? system-separator-markup)
				     (interpret-markup layout
						       (layout-extract-page-properties layout)
						       system-separator-markup)
				     #f))
       
       (page-stencil (ly:make-stencil '()))

       (last-system #f)
       (last-y 0.0)
       (add-to-page (lambda (stencil x y)
		      (set! page-stencil
			    (ly:stencil-add page-stencil
					    (ly:stencil-translate stencil
								  (cons
								   (+ system-xoffset x)
								   (- 0 y (prop 'top-margin)))
								  
								  )))))
       (add-system
	(lambda (system)
	  (let* ((stencil (paper-system-stencil system))
		 (y (ly:prob-property system 'Y-offset))
		 (is-title (paper-system-title?
			    system)))
	    (add-to-page stencil
			 (ly:prob-property system 'X-offset 0.0)
			 y)
	    (if (and (ly:stencil? system-separator-stencil)
		     last-system
		     (not (paper-system-title? system))
		     (not (paper-system-title? last-system)))
		(add-to-page
		 system-separator-stencil
		 0
		 (average (- last-y
			     (car (paper-system-staff-extents last-system)))
			  (- y
			     (cdr (paper-system-staff-extents system))))))
	    (set! last-system system)
	    (set! last-y y))))
       (head (prop 'head-stencil))
       (foot (prop 'foot-stencil))
       )

    (if (and
	 (ly:stencil? head)
	 (not (ly:stencil-empty? head)))
	(begin
	  ;; Ensure that the top of the header just touches the top margin.
	  (set! head (ly:stencil-translate-axis head
						(- 0 (cdr (ly:stencil-extent head Y)) (prop 'top-margin)) Y))
	  (set! page-stencil (ly:stencil-add page-stencil head))))

    (if (and
	 (annotate? layout)
	 (pair? lines))

	(begin
	  (set! page-stencil (annotate-top-space (car lines) layout head page-stencil))

	  (for-each (lambda (sys next-sys)
		      (paper-system-annotate sys next-sys layout))
		    lines
		    (append (cdr lines) (list #f)))
	  (paper-system-annotate-last (car (last-pair lines)) layout)))

    (map add-system lines)


    (ly:prob-set-property! page 'bottom-system-edge
			   (car (ly:stencil-extent page-stencil Y)))
    (ly:prob-set-property! page 'space-left
			   (+ (prop 'bottom-edge)
			      (prop 'bottom-system-edge)
			      (if (and (ly:stencil? foot)
				       (not (ly:stencil-empty? foot)))
				  (car (ly:stencil-extent foot Y))
				  0.0)))

    (if (annotate? layout)
	(set! page-stencil
	      (ly:stencil-add page-stencil
			      (annotate-space-left page))))
    
    (if (and (ly:stencil? foot)
	     (not (ly:stencil-empty? foot)))
	(set! page-stencil
	      (ly:stencil-add
	       page-stencil
	       (ly:stencil-translate
		foot
		(cons 0
		      (+ (- (prop 'bottom-edge))
			 (- (car (ly:stencil-extent foot Y)))))))))

    (set! page-stencil
	  (ly:stencil-translate page-stencil (cons (prop 'left-margin) 0)))

    ;; annotation.
    (if (annotate? layout)
	(set! page-stencil (annotate-page layout page-stencil)))

    page-stencil))
              

(define-public (page-stencil page)
  (if (not (ly:stencil? (page-property page 'stencil)))

      ;; todo: make tweakable.
      ;; via property + callbacks.
      
      (page-set-property! page 'stencil (make-page-stencil page)))
  (page-property page 'stencil))

(define (calc-printable-height page)
  "Printable area for music and titles; matches default-page-make-stencil."
  (let*
      ((paper-book (page-property page 'paper-book))
       (layout (ly:paper-book-paper paper-book))
       (h (- (ly:output-def-lookup layout 'paper-height)
	       (ly:output-def-lookup layout 'top-margin)
	       (ly:output-def-lookup layout 'bottom-margin)))
       
       (head (page-property page 'head-stencil))
       (foot (page-property page 'foot-stencil))
       (available
	(- h (if (ly:stencil? head)
		 (interval-length (ly:stencil-extent head Y))
		 0)
	   (if (ly:stencil? foot)
	       (interval-length (ly:stencil-extent foot Y))
	       0))))
    
    ;; (display (list "\n available" available head foot))
    available))

(define (page-printable-height page)
  (if (not (number? (page-property page 'printable-height)))
      (page-set-property! page 'printable-height (calc-printable-height page)))
  
  (page-property page 'printable-height))

