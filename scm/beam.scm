;;;;
;;;; beam.scm -- Beam scheme stuff
;;;;
;;;; source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 2000--2001 Jan Nieuwenhuizen <janneke@gnu.org>
;;;;

(define (default-beam-space-function multiplicity)
  (if (<= multiplicity 3) 0.816 0.844)
  )

;;
;; width in staff space.
;;
(define (default-beam-flag-width-function type)
  (cond
   ((eq? type 1) 1.98) 
   ((eq? type 1) 1.65) ;; FIXME: check what this should be and why
   (else 1.32)
   ))


;; This is a mess : global namespace pollution. We should wait
;;  till guile has proper toplevel environment support.


;; Beams should be prevented to conflict with the stafflines, 
;; especially at small slopes
;;    ----------------------------------------------------------
;;                                                   ########
;;                                        ########
;;                             ########
;;    --------------########------------------------------------
;;       ########
;;
;;       hang       straddle   sit        inter      hang

;; inter seems to be a modern quirk, we don't use that

;; two popular veritcal beam quantings
;; see params.ly: #'beam-vertical-quants


(define (default-beam-pos-quants beam multiplicity dy staff-line)
  (let* ((beam-straddle 0)
	 (thick (ly-get-grob-property beam 'thickness))
	 (beam-sit (/ (- thick staff-line) 2))
	 (beam-hang (- 1 (/ (- thick staff-line) 2)))
	 (quants (list beam-hang))
	 )
    
    (if (or (<= multiplicity 1) (>= (abs dy) (/ staff-line 2)))
	(set! quants (cons beam-sit quants)))
    (if (or (<= multiplicity 2) (>= (abs dy) (/ staff-line 2)))
	(set! quants (cons beam-straddle quants)))
    ;; period: 1 (staff-space)
    (append quants (list (+ 1 (car quants))))))

(define (default-left-beam-pos-quants beam multiplicity dir dy staff-line)
  (default-beam-pos-quants beam multiplicity 1 staff-line))
;;
(define (foo beam multiplicity dir dy staff-line)
  (let* ((beam-straddle 0)
	 (thick (ly-get-grob-property beam 'thickness))
	 (beam-sit (/ (- thick staff-line) 2))
	 (beam-hang (- 1 (/ (- thick staff-line) 2)))
	 (quants '())
	 )

    (if (or (<= multiplicity 1)
	    (and (not (equal? dir 1))
		 (not (< dy 0))))
	(set! quants (cons beam-sit quants)))
    (if (or (<= multiplicity 1)
	    (and (not (equal? dir -1))
		 (not (> dy 0))))
	(set! quants (cons beam-hang quants)))
    (if (or (<= multiplicity 2) (>= (abs dy) (/ staff-line 2)))
	(set! quants (cons beam-straddle quants)))
    ;; period: 1 (staff-space)
    (append quants (list (+ 1 (car quants))))))

(define (default-right-beam-pos-quants beam multiplicity dir dy staff-line)
  (default-beam-pos-quants beam multiplicity 1 staff-line))
;;
(define (foo beam multiplicity dir dy staff-line)
  (let* ((beam-straddle 0)
	 (thick (ly-get-grob-property beam 'thickness))
	 (beam-sit (/ (- thick staff-line) 2))
	 (beam-hang (- 1 (/ (- thick staff-line) 2)))
	 (quants '())
	 )

    
    (if (or (<= multiplicity 1)
	    (and (not (equal? dir 1))
		 (not (> dy 0))))
	(set! quants (cons beam-sit quants)))
    (if (or (<= multiplicity 1)
	    (and (not (equal? dir -1))
		 (not (< dy 0))))
	(set! quants (cons beam-hang quants)))
    (if (or (<= multiplicity 2) (>= (abs dy) (/ staff-line 2)))
	(set! quants (cons beam-straddle quants)))
    ;; period: 1 (staff-space)
    (append quants (list (+ 1 (car quants))))))

(define (beam-traditional-pos-quants beam multiplicity dy staff-line)
  (let* ((beam-straddle 0)
	(thick (ly-get-grob-property beam 'thickness))
	(beam-sit (/ (- thick staff-line) 2))
	(beam-hang (- 1 (/ (- thick staff-line) 2)))
	(quants '())
	)
    (if (>= dy (/ staff-line -2))
	(set! quants (cons beam-hang quants)))
    (if (and (<= multiplicity 1) (<= dy (/ staff-line 2)))
	(set! quants (cons beam-sit quants)))
    (if (or (<= multiplicity 2) (>= (abs dy) (/ staff-line 2)))
	(set! quants (cons beam-straddle quants)))
    ;; period: 1 (staff-space)
    (append quants (list (+ 1 (car quants))))))


;; There are several ways to calculate the direction of a beam
;;
;; * majority: number count of up or down notes
;; * mean    : mean centre distance of all notes
;; * median  : mean centre distance weighted per note

(define (dir-compare up down)
  (sign (- up down)))

;; arguments are in the form (up . down)
(define (beam-dir-majority count total)
  (dir-compare (car count) (cdr count)))

(beam-dir-majority '(0 . 0) '(0 . 0))

(define (beam-dir-mean count total)
  (dir-compare (car total) (cdr total)))

(define (beam-dir-median count total)
  (if (and (> (car count) 0)
	   (> (cdr count) 0))
      (dir-compare (/ (car total) (car count)) (/ (cdr total) (cdr count)))
      (dir-compare (car count) (cdr count))))
	    


;; [Ross] states that the majority of the notes dictates the
;; direction (and not the mean of "center distance")
;;
;; But is that because it really looks better, or because he wants
;; to provide some real simple hands-on rules?
;;     
;; We have our doubts, so we simply provide all sensible alternatives.

;; array index multiplicity, last if index>size
;; beamed stems


;; TODO
;;  - take #forced stems into account (now done in C++)?
;;  - take staff-position of chord or beam into account

