; Definition of backend properties (aka. element properties).

;; See documentation of Item::visibility_lambda_
(define (begin-of-line-visible d) (if (= d 1) '(#f . #f) '(#t . #t)))
(define (end-of-line-visible d) (if (= d -1) '(#f . #f) '(#t . #t)))
(define (spanbar-begin-of-line-invisible d) (if (= d -1) '(#t . #t) '(#f . #f)))
(define (all-visible d) '(#f . #f))
(define (all-invisible d) '(#t . #t))
(define (begin-of-line-invisible d) (if (= d 1) '(#t . #t) '(#f . #f)))
(define (end-of-line-invisible d) (if (= d -1) '(#t . #t) '(#f . #f)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Bar lines.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; How should a  bar line behave at a break? 
;
;; Why prepend `default-' to every scm identifier?
(define (default-break-barline glyph dir)
   (let ((result (assoc glyph 
			'((":|:" . (":|" . "|:"))
			  ("||:" . ("||" . "|:"))
			  ("|" . ("|" . ()))
			  ("||:" . ("||" . "|:"))
			  ("|s" . (() . "|"))
			  ("|:" . ("|" . "|:"))
			  ("|." . ("|." . ()))

			  ;; hmm... should we end with a barline here?
			  (".|" . ("|" . ".|"))
			  (":|" . (":|" . ()))
			  ("||" . ("||" . ()))
			  (".|." . (".|." . ()))
			  ("" . ("" . ""))
			  ("empty" . (() . ()))
			  ("brace" . (() . "brace"))
			  ("bracket" . (() . "bracket"))  
			  )
			)))

     (if (equal? result #f)
	 (ly-warn (string-append "Unknown bar glyph: `" glyph "'"))
	 (index-cell (cdr result) dir))
     )
   )
     
