;;;;
;;;; beam.scm -- Beam scheme stuff
;;;;
;;;; source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c)  2000--2004 Jan Nieuwenhuizen <janneke@gnu.org>
;;;;

;;
;; width in staff space.
;;
(define (beam-flag-width-function type)
  (cond
   ((eq? type 1) 1.98) 
   ((eq? type 1) 1.65) ;; FIXME: check what this should be and why
   (else 1.32)))

;; There are several ways to calculate the direction of a beam
;;
;; * majority: number count of up or down notes
;; * mean    : mean centre distance of all notes
;; * median  : mean centre distance weighted per note
;;
;; [Ross] states that the majority of the notes dictates the
;; direction (and not the mean of "center distance")
;;
;; But is that because it really looks better, or because he wants
;; to provide some real simple hands-on rules?
;;     
;; We have our doubts, so we simply provide all sensible alternatives.


;;
;; DOCME: what goes into this func, what comes out.
(define (dir-compare up down)
  (sign (- up down)))

;; arguments are in the form (up . down)
(define-public (beam-dir-majority count total)
  (dir-compare (car count) (cdr count)))

(define-public (beam-dir-majority-median count total)
  "First try majority. If that doesn't work, try median."
  (let ((maj (dir-compare (car count) (cdr count))))
    (if (not (= maj 0))
	maj
	(beam-dir-median count total))))

(define-public (beam-dir-mean count total)
  (dir-compare (car total) (cdr total)))

(define-public (beam-dir-median count total)
  (if (and (> (car count) 0)
	   (> (cdr count) 0))
      (dir-compare (/ (car total) (car count)) (/ (cdr total) (cdr count)))
      (dir-compare (car count) (cdr count))))

(define ((check-beam-quant posl posr) beam)
  "Check whether BEAM has POSL and POSR quants.  POSL are (POSITION
. QUANT) pairs, where QUANT is -1 (hang), 0 (center), 1 (sit) or -2/ 2 (inter) 

"
  (let* ((posns (ly:grob-property beam 'positions))
	 (thick (ly:grob-property beam 'thickness))
	 (layout (ly:grob-layout beam))
	 (lthick (ly:output-def-lookup layout 'linethickness))
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
	  (ly:warn
	   "Error in beam quanting found. Want (~S,~S) found (~S)."
	   want-l want-r posns )
	  (set! (ly:grob-property beam 'quant-score)
		(format "(~S,~S)" want-l want-r)))
	(set! (ly:grob-property beam 'quant-score) ""))))
(define ((check-beam-slope-sign comparison) beam)
  "Check whether the slope of BEAM is correct wrt. COMPARISON."
  (let* ((posns (ly:grob-property beam 'positions))
	 (slope-sign (- (cdr posns) (car posns)))
	 (correct (comparison slope-sign 0)))

    
    (if (not correct)
	(begin
	  (ly:warn "Error in beam quanting found. Want ~S 0 found ~S."
		   (procedure-name comparison) slope-sign)
	  (set! (ly:grob-property beam 'quant-score)
		(format "~S 0" (procedure-name comparison) )))
	(set! (ly:grob-property beam 'quant-score) ""))))

(define-public (check-quant-callbacks l r)
  (list Beam::least_squares
	Beam::check_concave
	Beam::slope_damping
	Beam::shift_region_to_valid
	Beam::quanting
	(check-beam-quant l r)))


(define-public (check-slope-callbacks comparison)
  (list Beam::least_squares
	Beam::check_concave
	Beam::slope_damping
	Beam::shift_region_to_valid
	Beam::quanting
	(check-beam-slope-sign comparison)))

