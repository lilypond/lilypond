;;; midi.scm -- scm midi variables and functions
;;;
;;;  source file of the GNU LilyPond music typesetter
;;; 
;;; (c) 2000 Jan Nieuwenhuizen <janneke@gnu.org>


;; define factor of total volume per dynamic marking
(define absolute-volume-alist '())
(set! absolute-volume-alist
      (append 
      '(
	("sf" . 1.00)
	("ffff" . 0.91)
	("fff" . 0.81)
	("ff" . 0.71)
	("f" . 0.61)
	("mf" . 0.50)
	("mp" . 0.40)
	("p" . 0.30)
	("pp" . 0.20)
	("ppp" . 0.10)
	)
      absolute-volume-alist))

(define (default-dynamic-absolute-volume s)
  (let ((entry (assoc s absolute-volume-alist)))
    (if entry
	(cdr entry))))

;; define factors of total volume of minimum and maximum volume
(define instrument-equaliser-alist '())
(set! instrument-equaliser-alist
      (append 
       '(
	 ("flute" . (0 . 0.6))
	 ("oboe" . (0 . 0.7))
	 ("clarinet" . (0 . 0.7))
	 ("bassoon" . (0 . 0.6))
	 ("french horn" . (0.1 . 0.7))
	 ("trumpet" . (0.1 . 0.8))
	 ("timpani" . (0.2 . 0.9))
	 ("violin" . (0.2 . 1.0))
	 ("viola" . (0.1 . 0.7))
	 ("cello" . (0.2 . 0.8))
	 ("contrabass" . (0.2 . 0.8))
	 )
       instrument-equaliser-alist))

(define (default-instrument-equaliser s)
  (let ((entry (assoc s instrument-equaliser-alist)))
    (if entry
	(cdr entry))))

;; 90 == 90/127 == 0.71 is supposed to be the default value
;; urg: we should set this at start of track
(define dynamic-default-volume 0.71)
