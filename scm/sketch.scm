;;; sketch.scm -- implement Scheme output routines for Sketch
;;;
;;;  source file of the GNU LilyPond music typesetter
;;; 
;;; (c) 1998--2001 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Han-Wen Nienhuys <hanwen@cs.uu.nl>


;; als in: 

;; def dispats (out,x,y,expr):
;;     (symbol, rest) = expr
;;     if symbol == 'placebox':
;; 	(dx,dy,expr) = rest
;; 	dispats (out, x + dx, y + dy, expr)
;;         # hier wordt (X+DX) dus eerder gedaan dan dispats van EXPR.
;;         # er zijn geen "globale" variabelen.
;;     elif symbol == 'char':
;;         out.write ('moveto( %f %f); char(%d)' % (x,y,rest))


;; (define (dispatch x y expr)
;;  (let ((keyword (car expr))) 
;;   (cond
;;    ((eq? keyword 'placebox)
;;    	    (dispatch (+ x (cadr expr)) (+ y (caddr expr) (cadddr expr)))

;;      [etc.]
;;    ))



(define-module (scm sketch)
  :export (sketch-output-expression)
  :no-backtrace)

(define this-module (current-module))

(define (sketch-output-expression expr port)
  (display (dispatch expr) port))

(use-modules
 (guile)
 (guile-user))

(use-modules (ice-9 format))


(define (dispatch expr)
  (let ((keyword (car expr))) 
    (cond
     ((eq? keyword 'placebox)
      (dispatch-x-y (cadr expr) (+ 150 (caddr expr)) (cadddr expr)))
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

(define font "")
(define output-scale 1.0)
(define (mul-scale x) (* output-scale x))

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
(define font-alist '())
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

(define (select-font name-mag-pair)
  (set! font (car name-mag-pair))
  "")

(define (font-load-command name-mag command)
  "")

(define (beam x y width slope thick)
  (apply sketch-filled-rectangle
	 (map mul-scale
	      (list width (* slope width) 0 thick x y))))

(define (comment s)
  (string-append "# " s))

(define (bracket arch_angle arch_width arch_height  height arch_thick thick)
  (string-append
   (numbers->string (list arch_angle arch_width arch_height height arch_thick thick)) " draw_bracket" ))

(define (char x y i)
  (string-append
   "fp((0,0,0))\n"
   "le()\n"
   "lw(0.1)\n"
   ;; "Fn('" global-font "')\n"
   ;; "Fn('Times-Roman')\n"
   "Fn('TeX-feta20')\n"
   "Fs(20)\n"
   ;; chars > 128 don't work yet
   (format #f "txt('\\~o',(" (modulo i 128))
   ;;	    "char(" ,(number->string i)  ",("
   (sketch-numbers->string (map mul-scale (list x y)))
   "))\n"))

(define (hairpin x y thick width starth endh )
  (string-append
   "#"
   (numbers->string (list width starth endh thick))
   " draw_hairpin"))

;; what the heck is this interface ?
(define (dashed-slur thick dash l)
  (string-append 
   (apply string-append (map control->string l)) 
   (ly-number->string thick) 
   " [ "
   (ly-number->string dash)
   " "
   (ly-number->string (* 10 thick))	;UGH.  10 ?
   " ] 0 draw_dashed_slur"))

(define (dashed-line thick on off dx dy)
  (string-append 
   (ly-number->string dx)
   " "
   (ly-number->string dy)
   " "
   (ly-number->string thick) 
   " [ "
   (ly-number->string on)
   " "
   (ly-number->string off)
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

(define (lily-def key val)
  (if (equal? key "lilypondpaperoutputscale")
      ;; ugr
      (set! output-scale (string->number val)))
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

(define (invoke-dim1 s d) 
  (string-append
   (ly-number->string (* d  (/ 72.27 72))) " " s ))

(define (bezier-sandwich x y l thick)
  (apply
   sketch-beziers (list x y (primitive-eval l) thick)))

; TODO: use HEIGHT argument
(define (start-line height)
   "G()\n"
   )

;;  r((520.305,0,0,98.0075,51.8863,10.089))
;;  width, 0, 0, height, x, y
(define (filledbox x y breapth width depth height)
  (apply sketch-filled-rectangle
	 (list
	  (+ breapth width) 0 0 (+ depth height) (- x breapth) (- y depth))))

(define (stem x y z w) (filledbox x y z w))


(define (stop-line)
    "G_()\n")

;; huh?
(define (stop-last-line)
   (stop-line))

(define (text x y s)
  (string-append "txt('" s "',(" (sketch-numbers->string
				  (map mul-scale (list x y))) "))\n"))


(define (volta x y h w thick vert_start vert_end)
  (string-append "#"
   (numbers->string (list h w thick (inexact->exact vert_start) (inexact->exact vert_end)))
   " draw_volta"))

(define (tuplet x y ht gap dx dy thick dir)
  (string-append "#"
   (numbers->string (list ht gap dx dy thick (inexact->exact dir)))
   " draw_tuplet"))


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



