;;; midi.scm -- scm midi variables and functions
;;;
;;;  source file of the GNU LilyPond music typesetter
;;; 
;;; (c) 2000 Jan Nieuwenhuizen <janneke@gnu.org>



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

(define (dynamic-absolute-volume s)
  (let ((entry (assoc s absolute-volume-alist)))
    (if entry
	(cdr entry))))

(define instrument-equaliser-alist '())
(set! instrument-equaliser-alist
      (append 
       '(
	 ("flute" . (0 . 1))
	 ("oboe" . (0 . 1))
	 ("clarinet" . (0 . 1))
	 ("bassoon" . (0 . 1))
	 ("french horn" . (0 . 1))
	 ("trumpet" . (0 . 1))
	 ("timpani" . (0 . 1))
	 ("violin" . (0 . 1))
	 ("viola" . (0 . 1))
	 ("cello" . (0 . 1))
	 ("contrabass" . (0 . 1))
	 )
       instrument-equaliser-alist))

(define (instrument-equaliser s)
  (let ((entry (assoc s absolute-volume-alist)))
    (if entry
	(cdr entry))))

(define instrument-equaliser-alist '())

;; 90 == 90/127 == 0.71 is supposed to be the default value
;; urg: we should set this at start of track
(define dynamic-default-volume 0.71)
