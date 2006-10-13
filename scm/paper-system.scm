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

(define-public (paper-system-layout system)
  (let*
      ((g (paper-system-system-grob system)))

    (if (ly:grob? g)
	(ly:grob-layout  g)
	#f)))

(define-public (paper-system-system-grob paper-system)
  (ly:prob-property paper-system 'system-grob))

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
  
(define-public (paper-system-annotate system next-system layout)
  "Add arrows and texts to indicate which lengths are set."
  (let* ((annotations (list))
	 (annotate-extent-and-space
	  (lambda (extent-accessor next-space
				   extent-name next-space-name after-space-name)
	    (let* ((extent-annotations (list))
		   (this-extent (extent-accessor system))
		   (next-extent (and next-system (extent-accessor next-system)))
		   (push-annotation (lambda (stil)
				      (set! extent-annotations
					    (cons stil extent-annotations))))
		   (color (if (paper-system-title? system) darkblue blue))
		   (space-color (if (paper-system-title? system) darkred red)))
	      (if (and (number-pair? this-extent)
		       (not (= (interval-start this-extent)
			       (interval-end this-extent))))
		  (push-annotation (annotate-y-interval
				    layout extent-name this-extent #f
				    #:color color)))
	      (if next-system
		  (push-annotation (annotate-y-interval
				    layout next-space-name
				    (interval-translate (cons (- next-space) 0)
							(if (number-pair? this-extent)
							    (interval-start this-extent)
							    0))
				    #t
				    #:color color)))
	      (if (and next-system
		       (number-pair? this-extent)
		       (number-pair? next-extent))
		  (let ((space-after
			 (- (+ (ly:prob-property next-system 'Y-offset)
			       (interval-start this-extent))
			    (ly:prob-property system 'Y-offset)
			    (interval-end next-extent)
			    next-space)))
		    (if (> space-after 0.01)
			(push-annotation (annotate-y-interval
					  layout
					  after-space-name
					  (interval-translate
					   (cons (- space-after) 0)
					   (- (interval-start this-extent)
					      next-space))
					  #t
					  #:color space-color)))))
	      (if (not (null? extent-annotations))
		  (set! annotations
			(stack-stencils X RIGHT 0.5
					(list annotations
					      (ly:make-stencil '() (cons 0 1) (cons 0 0))
					      (apply ly:stencil-add
						     extent-annotations))))))))

	 (grob (ly:prob-property system 'system-grob))
	 (estimate-extent (if (ly:grob? grob)
			      (annotate-y-interval layout
						   "extent-estimate"
						   (ly:grob-property grob 'pure-Y-extent)
						   #f)
			      #f)))
    (let ((next-space (ly:prob-property
		       system 'next-space
		       (cond ((and next-system
				   (paper-system-title? system)
				   (paper-system-title? next-system))
			      (ly:output-def-lookup layout 'between-title-space))
			     ((paper-system-title? system)
			      (ly:output-def-lookup layout 'after-title-space))
			     ((and next-system
				   (paper-system-title? next-system))
			      (ly:output-def-lookup layout 'before-title-space))
			     (else
			      (ly:output-def-lookup layout 'between-system-space)))))
	  (next-padding (ly:prob-property
			 system 'next-padding
			 (ly:output-def-lookup layout 'between-system-padding))))
      (annotate-extent-and-space (lambda (sys)
				   (paper-system-extent sys Y))
				 next-padding
				 "Y-extent" "next-padding" "space after next-padding")
      (annotate-extent-and-space paper-system-staff-extents
				 (+ next-space next-padding)
				 "refpoint-Y-extent" "next-space+padding"
				 "space after next-space+padding"))
    (if estimate-extent
	(set! annotations
	      (stack-stencils X RIGHT 0.5
			      (list annotations
				    estimate-extent))))
				
    (if (not (null? annotations))
	(set! (ly:prob-property system 'stencil)
	      (ly:stencil-add
	       (ly:prob-property system 'stencil)
	       (ly:make-stencil
		(ly:stencil-expr annotations)
		(ly:stencil-extent empty-stencil X)
		(ly:stencil-extent empty-stencil Y)))))
    (ly:prob-property system 'stencil)))
