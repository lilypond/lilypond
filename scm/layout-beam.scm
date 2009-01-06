;;;;
;;;; beam.scm -- Beam scheme stuff
;;;;
;;;; source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 2000--2009 Jan Nieuwenhuizen <janneke@gnu.org>
;;;;

(define ((check-beam-quant posl posr) beam posns)
  "Check whether BEAM has POSL and POSR quants.  POSL are (POSITION
. QUANT) pairs, where QUANT is -1 (hang), 0 (center), 1 (sit) or -2/ 2 (inter) 

"
  (let* ((thick (ly:grob-property beam 'thickness))
	 (layout (ly:grob-layout beam))
	 (lthick (ly:output-def-lookup layout 'line-thickness))
	 (staff-thick lthick) ; fixme.
	 (quant->coord (lambda (p q)
			 (if (= 2 (abs q))
			     (+ p (/ q 4.0))
			     (+ p (- (* 0.5 q thick) (* 0.5 q lthick))))))
	 (want-l (quant->coord (car posl) (cdr posl))) 
	 (want-r (quant->coord (car posr) (cdr posr)))
	 (almost-equal (lambda (x y) (< (abs (- x y)) 1e-3))))
    
    (if (or (not (almost-equal want-l (car posns)))
	    (not (almost-equal want-r (cdr posns))))
	(begin
	  (ly:warning (_ "Error in beam quanting.  Expected (~S,~S) found ~S.")
		      want-l want-r posns)
	  (set! (ly:grob-property beam 'annotation)
		(format "(~S,~S)" want-l want-r))))
    posns
    ))


(define ((check-beam-slope-sign comparison) beam posns)
  "Check whether the slope of BEAM is correct wrt. COMPARISON."
  (let* ((slope-sign (- (cdr posns) (car posns)))
	 (correct (comparison slope-sign 0)))
    (if (not correct)
	(begin
	  (ly:warning (_ "Error in beam quanting.  Expected ~S 0, found ~S.")
		      (procedure-name comparison) slope-sign)
	  (set! (ly:grob-property beam 'annotation)
		(format "~S 0" (procedure-name comparison))))
	(set! (ly:grob-property beam 'annotation) ""))
    posns))


(define-public (check-quant-callbacks l r)
  (list ly:beam::calc-least-squares-positions
	ly:beam::slope-damping
	ly:beam::shift-region-to-valid
	ly:beam::quanting
	(check-beam-quant l r)
	))


(define-public (check-slope-callbacks comparison)
  (list ly:beam::calc-least-squares-positions
	ly:beam::slope-damping
	ly:beam::shift-region-to-valid
	ly:beam::quanting
	(check-beam-slope-sign comparison)	
	))

