;;; paper.scm -- scm paper variables and functions
;;;
;;;  source file of the GNU LilyPond music typesetter
;;; 
;;; (c) 1999 Jan Nieuwenhuizen <janneke@gnu.org>

;;; All dimensions are measured in staff-spaces

;; TODO
;;  - make easily customisable from mudela
;;  - take #forced stems into account (now done in C++)
;;  - take y-position of chord or beam into account
(define (stem-shorten flags) 0.5)
(define (beamed-stem-shorten multiplicity) 0.5)


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

(define beam-normal-dy-quants
  '(0 (/2 (+ beam-thickness staff-line) 2) (+ beam-thickness staff-line) 1))

;; two popular veritcal beam quantings
;; see params.ly: #'beam-vertical-quants
(define (beam-normal-y-quants multiplicity dy)
  (let ((quants `(,beam-hang 1)))
    (if (or (<= multiplicity 1) (>= (abs dy) (/ staff-line 2)))
	(set! quants (cons beam-sit quants)))
    (if (or (<= multiplicity 2) (>= (abs dy) (/ staff-line 2)))
	(set! quants (cons beam-straddle quants)))
    quants))

(define (beam-traditional-y-quants multiplicity dy)
  (let ((quants '(1)))
    (if (>= dy (/ staff-line -2))
	(set! quants (cons beam-hang quants)))
    (if (and (<= multiplicity 1) (<= dy (/ staff-line 2)))
	(set! quants (cons beam-sit quants)))
    (if (or (<= multiplicity 2) (>= (abs dy) (/ staff-line 2)))
	(set! quants (cons beam-straddle quants)))
    quants))


;;; Default variables and settings

(define staff-line 0.10)
(define beam-thickness (* 0.52 (- 1 staff-line)))
(define beam-straddle 0)
(define beam-sit (/ (+ beam-thickness staff-line) 2))
(define beam-hang (- 1 (/ (- beam-thickness staff-line) 2)))

(define beam-height-quants beam-normal-dy-quants)
(define beam-vertical-position-quants beam-normal-y-quants)


