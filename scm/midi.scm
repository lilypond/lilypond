;;; midi.scm -- scm midi variables and functions
;;;
;;;  source file of the GNU LilyPond music typesetter
;;; 
;;; (c) 2000 Jan Nieuwenhuizen <janneke@gnu.org>


(define absolute-volume-alist '())
(set! absolute-volume-alist
      (append 
      '(
	("sf" . 115)
	("fff" . 102)
	("ff" . 90)
	("f" . 77)
	("mf" . 64)
	("mp" . 51)
	("p" . 38)
	("pp" . 26)
	("ppp" . 13)
	)
      absolute-volume-alist))

(define (dynamic-absolute-volume s)
  (let ((entry (assoc s absolute-volume-alist)))
    (if entry
	(cdr entry))))

;; 90 is supposed to be the default value
;; urg: we should set this at start of track
(define dynamic-default-volume 90)
