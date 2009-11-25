;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2006--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;;
;;;; LilyPond is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; LilyPond is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.

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
  (ly:prob-property ps 'staff-refpoint-extent '(0 . 0)))

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

; TODO: annotate the spacing for every spaceable staff within the system.
(define-public (paper-system-annotate system next-system layout)
  "Add arrows and texts to indicate which lengths are set."
  (let* ((annotations (list))
	 (grob (ly:prob-property system 'system-grob))
	 (estimate-extent (if (ly:grob? grob)
			      (annotate-y-interval layout
						   "extent-estimate"
						   (ly:grob-property grob 'pure-Y-extent)
						   #f)
			      #f)))
    (let* ((spacing-spec (cond ((and next-system
				     (paper-system-title? system)
				     (paper-system-title? next-system))
				(ly:output-def-lookup layout 'between-title-spacing))
			       ((paper-system-title? system)
				(ly:output-def-lookup layout 'after-title-spacing))
			       ((and next-system
				     (paper-system-title? next-system))
				(ly:output-def-lookup layout 'before-title-spacing))
			       (else
				(ly:output-def-lookup layout 'between-system-spacing))))
	   (last-staff-Y (car (paper-system-staff-extents system))))

      (set! annotations
	    (annotate-spacing-spec layout spacing-spec last-staff-Y (car (paper-system-extent system Y)))))
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
