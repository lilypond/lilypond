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
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  Prefatory matter: break align item.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spacing constants 
;;
;; rules for this spacing are much more complicated than this. 
;; See [Wanske] page 126 -- 134, [Ross] pg 143 -- 147
;;

;; documentme: difference between extra-space and minimum-space-pair 

;; (Measured in staff space)
(define default-break-align-space-alist
 '(
   ((none Instrument_name) . (extra-space 1.0))
   ((none Left_edge_item) . (extra-space 0.0))
   ((none Clef_item) . (minimum-space-pair  1.0))
   ((none Staff_bar) . (minimum-space-pair  0.0))
   ((none Clef_item) . (minimum-space-pair  1.0))
   ((none Key_item) . (minimum-space-pair  0.5))
   ((none Time_signature) . (extra-space 0.0))
   ((none Breathing_sign) . (minimum-space-pair  0.0))

   




   ((none begin-of-note) . (minimum-space-pair  1.5))

   )
)

