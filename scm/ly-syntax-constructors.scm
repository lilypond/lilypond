;;;; define-syntax.scm -- Defines functions for syntax expressions
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 2006 Erik Sandberg <mandolaerik@gmail.com>

;; TODO: use separate module for parser.
(define define-ly-syntax define-public)

;; This shorthand adds a location parameter, and uses it to set the
;; origin. It can be used for most music functions.
(defmacro define-ly-syntax-loc (args . body)
  (primitive-eval `(define-ly-syntax ,(cons* (car args) 'location (cdr args))
		     (let ((m ((lambda ,(cdr args) . ,body) . ,(cdr args))))
		       (set! (ly:music-property m 'origin) location)
		       m))))

(define-ly-syntax-loc (sequential-music mlist)
  (make-sequential-music mlist))

(define-ly-syntax-loc (simultaneous-music mlist)
  (make-simultaneous-music mlist))

(define-ly-syntax-loc (repeat type num body alts)
  (make-repeat type num body alts))
