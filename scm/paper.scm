;;; paper.scm -- scm paper variables and functions
;;;
;;;  source file of the GNU LilyPond music typesetter
;;; 
;;; (c) 1999 Jan Nieuwenhuizen <janneke@gnu.org>

;;; All dimensions are measured in staff-spaces



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


;; array index flag-2 (what a name!!), last if index>size
;; unbeamed stems
(define stem-length '(3.5 3.5 3.5 4.5 5.0))
(define grace-length-factor 0.8)
(define grace-stem-length
  (map (lambda (x) (* grace-length-factor x)) stem-length))

;; array index multiplicity, last if index>size
;; beamed stems
(define beamed-stem-shorten '(0.5))
(define beamed-stem-length '(0.0 2.5 2.0 1.5))
(define beamed-stem-minimum-length '(0.0 3.0 2.5 2.0))
(define grace-beamed-stem-minimum-length
  (map (lambda (x) (* grace-length-factor x)) beamed-stem-minimum-length))

;;  Stems in unnatural (forced) direction should be shortened,
;;  according to [Roush & Gourlay].  Their suggestion to knock off
;;  a whole staffspace seems a bit drastical: we'll do half.

;; TODO
;;  - take #forced stems into account (now done in C++)?
;;  - take y-position of chord or beam into account

(define stem-shorten '(0.5))
(define grace-stem-shorten '(0.0))
