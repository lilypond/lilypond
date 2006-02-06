;;
;; paper-system.scm -- implement paper-system objects.
;;
;; source file of the GNU LilyPond music typesetter
;;
;; (c) 2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
;;

(define-module (scm paper-system))

(use-modules (lily))

(define-public (paper-system-title? system)
  (equal? #t (ly:prob-property system 'is-title)
	  ))

(define-public (paper-system-stencil system)
  (ly:prob-property system 'stencil))

(define-public (paper-system-extent system axis)
  (ly:stencil-extent (paper-system-stencil system) axis))

(define-public (paper-system-staff-extents ps)
  (ly:prob-property ps 'refpoint-Y-extent '(0 . 0)))

(define-public (paper-system-annotate-last system layout)
  (let*
      ((bottomspace (ly:prob-property system 'bottom-space))
       (y-extent (paper-system-extent system Y))
       (x-extent (paper-system-extent system X))
       (stencil (ly:prob-property system 'stencil))
     
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

    (set! (ly:prob-property system 'stencil)
	  stencil)
  ))
  
(define-public (paper-system-annotate system layout)
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
       (refp-extent (ly:prob-property system 'refpoint-Y-extent))
       (next-space (ly:prob-property system 'next-space
					     (ly:output-def-lookup layout 'between-system-space)
					     ))
       (next-padding (ly:prob-property system 'next-padding
					       (ly:output-def-lookup layout 'between-system-padding)
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
	
    

    (set! (ly:prob-property system 'stencil)
	  (ly:stencil-add
	   (ly:prob-property system 'stencil)
	   (ly:make-stencil
	    (ly:stencil-expr annotations)
	    (ly:stencil-extent empty-stencil X)
	    (ly:stencil-extent empty-stencil Y)
	    )))
    
    ))
