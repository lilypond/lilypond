; Definition of backend properties (aka. element properties).

;; See documentation of Item::visibility_lambda_
(define (begin-of-line-visible d) (if (= d 1) '(#f . #f) '(#t . #t)))
(define (end-of-line-visible d) (if (= d -1) '(#f . #f) '(#t . #t)))
(define (spanbar-begin-of-line-invisible d) (if (= d -1) '(#t . #t) '(#f . #f)))
(define (all-visible d) '(#f . #f))
(define (all-invisible d) '(#t . #t))
(define (begin-of-line-invisible d) (if (= d 1) '(#t . #t) '(#f . #f)))
(define (end-of-line-invisible d) (if (= d -1) '(#t . #t) '(#f . #f)))


(define mark-visibility end-of-line-invisible)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Bar lines.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; How should a  bar line behave at a break? 
;
(define (default-break-barline glyph dir)
   (let ((result (assoc glyph 
			'((":|:" . (":|" . "|:"))
			  ("|" . ("|" . ""))
			  ("|s" . (nil . "|"))
			  ("|:" . ("|" . "|:"))
			  ("|." . ("|." . nil))
			  (".|" . (nil . ".|"))
			  (":|" . (":|" . nil))
			  ("||" . ("||" . nil))
			  (".|." . (".|." . nil))
			  ("empty" . ("nil" . nil))
			  ("brace" . (nil . "brace"))
			  ("bracket" . (nil . "bracket"))  
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

;; (Measured in staff space)
(define default-break-align-space-alist
 '(
   ((Staff_bar Custos) . (minimum-space 2.0))
   ((Custos begin-of-note) . (minimum-space 0.0))
   ((none Instrument_name) . (extra-space 1.0))
   ((Instrument_name Left_edge_item) . (extra-space 1.0))
   ((Left_edge_item Clef_item) . (extra-space 1.0))
   ((Left_edge_item Key_item) . (extra-space 0.0))   
   ((Left_edge_item begin-of-note) . (extra-space 1.0))
   ((none Left_edge_item) . (extra-space 0.0))
   ((Left_edge_item Staff_bar) . (extra-space 0.0))
;   ((none Left_edge_item) . (extra-space -15.0))
;   ((none Left_edge_item) . (extra-space -15.0))
   ((none Clef_item) . (minimum-space 1.0))
   ((none Staff_bar) . (minimum-space 0.0))
   ((none Clef_item) . (minimum-space 1.0))
   ((none Key_item) . (minimum-space 0.5))
   ((none Time_signature) . (extra-space 0.0))
   ((none begin-of-note) . (minimum-space 1.5))
   ((Clef_item Key_item) . (minimum-space 4.0))
   ((Key_item Time_signature) . (extra-space 1.0))
   ((Clef_item  Time_signature) . (minimum-space 3.5))
   ((Staff_bar Clef_item) .   (minimum-space 1.0))
   ((Clef_item  Staff_bar) .  (minimum-space 3.7))
   ((Time_signature Staff_bar) .  (minimum-space 2.0))
   ((Key_item  Staff_bar) .  (extra-space 1.0))
   ((Staff_bar Time_signature) . (minimum-space 1.5)) 
   ((Time_signature begin-of-note) . (extra-space 2.0)) 
   ((Key_item begin-of-note) . (extra-space 2.5))
   ((Staff_bar begin-of-note) . (extra-space 1.0))
   ((Clef_item begin-of-note) . (minimum-space 5.0))
   ((none Breathing_sign) . (minimum-space 0.0))
   ((Breathing_sign Key_item) . (minimum-space 1.5))
   ((Breathing_sign begin-of-note) . (minimum-space 1.0))
   ((Breathing_sign Staff_bar) . (minimum-space 1.5))
   ((Breathing_sign Clef_item) . (minimum-space 2.0))
   )
)
