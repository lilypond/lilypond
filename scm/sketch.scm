


(use-modules (ice-9 format))

(define (ascii->string i) (make-string 1 (integer->char i)))

(define (control->list c)
  (list (+ global-x (car c)) (+ global-y (cdr c))))

(define (control-flip-y c)
  (cons (car c) (* -1 (cdr c))))

;;; urg.
(define (sk-numbers->string l)
  (string-append
   (number->string (car l))
   (if (null? (cdr l))
       ""
       (string-append ","  (sk-numbers->string (cdr l))))))

(define global-x 0.0)
(define global-y 0.0)
(define global-list '())
(define global-font "")
(define global-s "")
(define global-scale 1.0)
(define (global-mul-scale  x) (* global-scale x))

;; hmm, global is global
(define (global-filledbox width dy dx height x y)
  (string-append
   "fp((0,0,0))\n"
   "lw(0.1)\n"
   "r("
   (sk-numbers->string
    (map global-mul-scale (list width dy dx height x y)))
   ")\n"))

(define (global-bezier l)
  (let* ((c0 (car (list-tail l 3)))
	 (c123 (list-head l 3))
	 (start (control->list c0))
	 (control (apply append (map control->list c123))))
    (string-append
     "bs(" (sk-numbers->string (map global-mul-scale start)) ",0)\n"
     "bc(" (sk-numbers->string (map global-mul-scale control)) ",2)\n")))
  

(define (global-beziers l thick)
  (let* (;;(burp (set! global-y (+ global-y (* 2 (cdar l)))))
	 (first
	  (list-tail l 4))
	 (second
	  (list-head l 4))
		 )
    (string-append
     "fp((0,0,0))\n"
     "lw(0.1)\n"
     "b()\n"
     (global-bezier first)
     (global-bezier second)
     ;;"b_()\n"
     )))
	 
		 
(define (sketch-scm action-name)
  
  ;; alist containing fontname -> fontcommand assoc (both strings)
  (define font-alist '())
  (define font-count 0)
  (define current-font "")

  (define (font-def x)
  "")

  (define (cached-fontname i)
    "")
  
  (define (select-font name-mag-pair)
    (set! global-font (car name-mag-pair))
    "")
  
  (define (font-load-command name-mag command)
    "")
    
  (define (beam width slope thick)
    (let ((s (list
	      'global-filledbox
	      width
	      (* slope width)
	      0
	      thick
	      'global-x
	      'global-y)))
      (set! global-s s))
    "\n")

  (define (comment s)
    (string-append "% " s))

  (define (bracket arch_angle arch_width arch_height  height arch_thick thick)
    (string-append
     (numbers->string (list arch_angle arch_width arch_height height arch_thick thick)) " draw_bracket" ))

  (define (char i)
    (set! global-s
;;	  `(string-append "txt(" ,(number->string i) ",("
;;			  (sk-numbers->string (list global-x global-y))
	  `(string-append
	    "fp((0,0,0))\n"
	    "le()\n"
	    "lw(0.1)\n"
;;	    "Fn('" global-font "')\n"
;;	    "Fn('Times-Roman')\n"
	    "Fn('TeX-feta20')\n"
	    "Fs(20)\n"
	    ;; chars > 128 don't work yet
	    "txt('" ,(ascii->string (modulo i 128)) "',("
;;	    "char(" ,(number->string i)  ",("
	    (sk-numbers->string (list (* global-scale global-x)
				      (* global-scale global-y)))
	    "))\n")))

  (define (hairpin thick width starth endh )
    (string-append 
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
	(set! global-scale (string->number val)))
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

  ;;  urg
  (define (placebox x y s)
;;    (format (current-error-port) "placebox: ~S, ~S, ~S\n" x y s)
    (set! global-x (+ x 0))
    (set! global-y (+ y 100))
    (let ((s (primitive-eval global-s)))
      (set! global-s "\n")
      s))

  (define (bezier-sandwich l thick)
    (let ((s (list
	      'global-beziers
	      'global-list
	      thick)))
      (set! global-s s)
      (set! global-list l))
    "\n")

; TODO: use HEIGHT argument
  (define (start-line height)
     "G()\n"
     )
  
  ;;  r((520.305,0,0,98.0075,51.8863,10.089))
  ;;  width, 0, 0, height, x, y
  (define (filledbox breapth width depth height)
    (let ((s (list
	      'global-filledbox
	      (+ breapth width)
	      0 0
	      (+ depth height)
	      `(- global-x ,breapth)
	      `(- global-y ,depth))))
;;      (format (current-error-port) "filledbox: ~S\n" s)
      (set! global-s s))
    "\n")
  
  (define (stem x y z w) (filledbox x y z w))

  
  (define (stop-line)
      "G_()\n")

  (define (text s)
    (set! global-s
	  `(string-append "txt('" ,s "',("
			  (sk-numbers->string (list global-x global-y))
			  "))\n")))


  (define (volta h w thick vert_start vert_end)
    (string-append 
     (numbers->string (list h w thick (inexact->exact vert_start) (inexact->exact vert_end)))
     " draw_volta"))

  (define (tuplet ht gap dx dy thick dir)
    (string-append 
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
  
  ;; PS
  (cond ((eq? action-name 'all-definitions)
	 `(begin
	    (define beam ,beam)
	    (define tuplet ,tuplet)
	    (define bracket ,bracket)
	    (define char ,char)
	    (define hairpin ,hairpin)
	    (define volta ,volta)
	    (define bezier-sandwich ,bezier-sandwich)
	    (define dashed-line ,dashed-line) 
	    (define dashed-slur ,dashed-slur) 
	    (define end-output ,end-output)
	    (define experimental-on ,experimental-on)
	    (define filledbox ,filledbox)
	    (define stem ,stem)	    
	    (define font-def ,font-def)
	    (define font-switch ,font-switch)
	    (define header-end ,header-end)
	    (define lily-def ,lily-def)
	    (define font-load-command ,font-load-command)
	    (define header ,header) 
	    (define invoke-char ,invoke-char) 
	    (define invoke-dim1 ,invoke-dim1)
	    (define placebox ,placebox)
	    (define select-font ,select-font)
	    (define start-line ,start-line)
	    (define stem ,stem)
	    (define stop-line ,stop-line)
	    (define stop-last-line ,stop-line)
	    (define repeat-slash ,repeat-slash)
	    (define text ,text)
	    (define no-origin ,no-origin)
	    (define define-origin ,define-origin)
	    (define ez-ball ,ez-ball)
	    ))
	((eq? action-name 'repeat-slash) repeat-slash)
	((eq? action-name 'tuplet) tuplet)
	((eq? action-name 'beam) beam)
	((eq? action-name 'bezier-sandwich) bezier-sandwich)
	((eq? action-name 'bracket) bracket)
	((eq? action-name 'char) char)
	((eq? action-name 'dashed-line) dashed-line) 
	((eq? action-name 'dashed-slur) dashed-slur) 
	((eq? action-name 'hairpin) hairpin)
	((eq? action-name 'experimental-on) experimental-on)
	((eq? action-name 'filledbox) filledbox)
	((eq? action-name 'ez-ball) ez-ball)	
	((eq? action-name 'select-font) select-font)
	((eq? action-name 'volta) volta)
	(else (error "unknown tag -- SKETCH-SCM " action-name))
	)
  )


