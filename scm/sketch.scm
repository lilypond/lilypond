
;;; sketch.scm -- implement Scheme output routines for Sketch
;;;
;;;  source file of the GNU LilyPond music typesetter
;;; 
;;; (c) 1998--2002 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Han-Wen Nienhuys <hanwen@cs.uu.nl>


;; def dispats (out,x,y,expr):
;;     (symbol, rest) = expr
;;     if symbol == 'placebox':
;; 	(dx,dy,expr) = rest
;; 	dispats (out, x + dx, y + dy, expr)
;;         # hier wordt (X+DX) dus eerder gedaan dan dispats van EXPR.
;;         # er zijn geen "globale" variabelen.
;;     elif symbol == 'char':
;;         out.write ('moveto( %f %f); char(%d)' % (x,y,rest))




;;
;; All functions have the signature 
;;
;;  NAME X Y ARGUMENTS-PASSED-BY-LILYPOND
;;

(define-module (scm sketch))
(debug-enable 'backtrace)

(define this-module (current-module))

(define-public (sketch-output-expression expr port)
  (display (dispatch expr) port))

(use-modules (ice-9 format) (guile) (lily))

;; hmm
; (define (dispatch x y expr)
;  (let ((keyword (car expr))) 
;    (cond
; ((eq? keyword 'beam x y width slope thick)
; ((eq? keyword 'bezier-bow x y l thick)
; ((eq? keyword 'bezier-sandwich x y l thick)
; ((eq? keyword 'bracket arch_angle arch_width arch_height  height arch_thick thick)
; ((eq? keyword 'char x y i)
; ((eq? keyword 'comment s)
; ((eq? keyword 'dashed-line thick on off dx dy)
; ((eq? keyword 'dashed-slur thick dash l)
; ((eq? keyword 'define-origin a b c ) "")
; ((eq? keyword 'end-output)
; ((eq? keyword 'experimental-on) "")
; ((eq? keyword 'ez-ball ch letter-col ball-col)
; ((eq? keyword 'filledbox x y breapth width depth height)
; ((eq? keyword 'font-load-command name-mag command)
; ((eq? keyword 'font-switch i)
; ((eq? keyword 'header creator generate)
; ((eq? keyword 'header-end)
; ((eq? keyword 'invoke-char s i)
; ((eq? keyword 'lily-def key val)
; ((eq? keyword 'no-origin) "")
; ((eq? keyword 'output-scale 1)
; ((eq? keyword 'placebox)
;  (dispatch (+ x (cadr expr)) (+ y (caddr expr) (cadddr expr))))
; ((eq? keyword 'repeat-slash wid slope thick)
; ((eq? keyword 'roundfilledbox x y dx dy w h b)
; ((eq? keyword 'select-font name-mag-pair)
; ((eq? keyword 'start-system width height)
; ((eq? keyword 'stem x y z w) (filledbox x y z w))
; ((eq? keyword 'stop-last-system)
; ((eq? keyword 'stop-system)
; ((eq? keyword 'text x y s)
; ((eq? keyword 'unknown)

;     )))


(define current-y 150)

(define (dispatch expr)
  (let ((keyword (car expr))) 
    (cond
     ((eq? keyword 'placebox)
      (dispatch-x-y (cadr expr) (+ current-y (caddr expr)) (cadddr expr)))
     (else
      (apply (eval keyword this-module) (cdr expr))))))

(define (dispatch-x-y x y expr)
  (apply (eval (car expr) this-module) (append (list x y) (cdr expr))))
      
(define (ascii->string i) (make-string 1 (integer->char i)))

(define (control->list x y c)
  (list (+ x (car c)) (+ y (cdr c))))

(define (control-flip-y c)
  (cons (car c) (* -1 (cdr c))))

;;; urg.
(define (sketch-numbers->string l)
  (string-append
   (number->string (car l))
   (if (null? (cdr l))
       ""
       (string-append ","  (sketch-numbers->string (cdr l))))))

;;;\def\scaletounit{ 2.83464566929134 mul }%

;;(define output-scale 2.83464566929134)

(define scale-to-unit
  (cond
   ((equal? (ly:unit) "mm") (/ 72.0  25.4))
   ((equal? (ly:unit) "pt") (/ 72.0  72.27))
   (else (error "unknown unit" (ly:unit)))
   ))

(define (mul-scale x) (* scale-to-unit output-scale x))

(define (sketch-filled-rectangle width dy dx height x y)
  (string-append
   "fp((0,0,0))\n"
   "lw(0.1)\n"
   "r("
   (sketch-numbers->string (map mul-scale (list width dy dx height x y)))
   ")\n"))


(define (sketch-bezier x y l)
  (let* ((c0 (car (list-tail l 3)))
	 (c123 (list-head l 3))
	 (start (control->list x y c0))
	 (control (apply append
			 (map (lambda (c) (control->list x y c)) c123))))
    (string-append
     "bs(" (sketch-numbers->string (map mul-scale start)) ",0)\n"
     "bc(" (sketch-numbers->string (map mul-scale control)) ",2)\n")))
  


(define (sketch-beziers x y l thick)
  (let* ((first (list-tail l 4))
	 (second (list-head l 4)))
    (string-append
     "fp((0,0,0))\n"
     "lw(0.1)\n"
     "b()\n"
     (sketch-bezier x y first)
     (sketch-bezier x y second))))
	 

;; alist containing fontname -> fontcommand assoc (both strings)
;; old scheme
;;(define font-alist '(("feta13" . ("feta13" . "13"))
;;		     ("feta20" . ("feta20" . "20"))))
(define font-alist '(("feta13" . ("LilyPond-Feta13" . "13"))
;;		     ("feta20" . ("LilyPond-Feta-20" . "20")
		     ("feta20" . ("GNU-LilyPond-feta-20" . "20")
		      )))

;;(define font "")
(define font (cdar font-alist))

(define font-count 0)
(define current-font "")

(define (fontify x y name-mag-pair exp)
  (string-append (select-font name-mag-pair)
		 (apply (eval (car exp) this-module)
			(append (list x y) (cdr exp)))))
;;		 (if (string? exp) exp "")))

(define (define-fonts x) "")

(define (font-def x)
"")


(define (cached-fontname i)
  "")


(define (roundfilledbox x y dx dy w h b)
  (sketch-filled-rectangle w 0 0 h x y))

(define (select-font name-mag-pair)
  ;; name-mag-pair: (quote ("feta20" . 0.569055118110236))"feta20"(quote ("feta20" . 0.569055118110236))
  (let ((f (assoc (caadr name-mag-pair) font-alist)))
    (if (pair? f)
	(set! font (cdr f))
	(format #t "font not found: ~s\n" (caadr name-mag-pair))))
  ;;(write font)
  "")

(define (font-load-command name-mag command)
  "")

(define (beam x y width slope thick)
  (apply sketch-filled-rectangle
	 (list width (* slope width) 0 thick x y)))

(define (comment s)
  (string-append "# " s "\n"))

(define (bracket arch_angle arch_width arch_height  height arch_thick thick)
  (string-append
   (numbers->string (list arch_angle arch_width arch_height height arch_thick thick)) " draw_bracket" ))

(define (char x y i)
  (string-append
   "fp((0,0,0))\n"
   "le()\n"
   "lw(0.1)\n"
   "Fn('" (car font) "')\n"
   "Fs(" (cdr font) ")\n"
   ;; how to get zero-left padding with ``Guile's fprintf'' ?
   ;;(format #f "txt('\\x~2x',(" i)
   ;;(format #f "txt('\\x~02x',(" i)
   ;; ugh uhg
   (if (< i 16)
       (format #f "txt('\\x0~x',(" i)
       (format #f "txt('\\x~x',(" i))
   (sketch-numbers->string (map mul-scale (list x y)))
   "))\n"))


;; what the heck is this interface ?
(define (dashed-slur thick dash l)
  (string-append 
   (apply string-append (map number-pair->string l)) 
   (ly:number->string thick) 
   " [ "
   (ly:number->string dash)
   " "
   (ly:number->string (* 10 thick))	;UGH.  10 ?
   " ] 0 draw_dashed_slur"))

(define (dashed-line thick on off dx dy)
  (string-append 
   (ly:number->string dx)
   " "
   (ly:number->string dy)
   " "
   (ly:number->string thick) 
   " [ "
   (ly:number->string on)
   " "
   (ly:number->string off)
   " ] 0 draw_dashed_line"))

(define (repeat-slash wid slope thick)
 (string-append (numbers->string (list wid slope thick))
  " draw_repeat_slash"))

(define (end-output)
  "guidelayer('Guide Lines',1,0,0,1,(0,0,1))
grid((0,0,20,20),0,(0,0,1),'Grid')\n")

(define (experimental-on) "")

(define (font-switch i)
  "")

(define (header-end)
  "")

(define output-scale 1)

(define (lily-def key val)
  (if (equal? key "lilypondpaperoutputscale")
      ;; ugr
      (set! output-scale (string->number val))
      )
  "")


(define (header creator generate)
  (string-append
   "##Sketch 1 2
document()
layout('A4',0)
layer('Layer 1',1,1,0,0,(0,0,0))
"))

(define (invoke-char s i)
  "")

;; TODO: bezier-ending, see ps.scm
(define (bezier-bow x y l thick)
  (bezier-sandwich x y l thick))

(define (bezier-sandwich x y l thick)
  (apply
   sketch-beziers (list x y (primitive-eval l) thick)))

(define (start-system width height)
  (set! current-y (- current-y height))
  "G()\n")

;;  r((520.305,0,0,98.0075,51.8863,10.089))
;;  width, 0, 0, height, x, y
(define (filledbox x y breapth width depth height)
  (apply sketch-filled-rectangle
	 (list
	  (+ breapth width) 0 0 (+ depth height) (- x breapth) (- y depth))))

(define (stem x y z w) (filledbox x y z w))


(define (stop-system)
    "G_()\n")

;; huh?
(define (stop-last-system)
   (stop-system))

(define (text x y s)
  (string-append
   "fp((0,0,0))\n"
   "le()\n"
   "lw(0.1)\n"
   "Fn('" (car font) "')\n"
   "Fs(" (cdr font) ")\n"
   ;; Hmm
   "txt('" s "',(" (sketch-numbers->string
				  (map mul-scale (list x y))) "))\n"))

(define (unknown) 
  "\n unknown\n")

(define (ez-ball ch letter-col ball-col)
  (string-append
   " (" ch ") "
   (numbers->string (list letter-col ball-col))
   " /Helvetica-Bold " ;; ugh
   " draw_ez_ball"))

(define (define-origin a b c ) "")
(define (no-origin) "")



;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;

 
