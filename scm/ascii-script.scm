(debug-enable 'backtrace)

; (define cmr-alist
;   '(("bold" . "as-dummy") 
;     ("brace" . "as-braces")
;     ("dynamic" . "as-dummy") 
;     ("default" . "as-dummy") 
;     ("feta" . "feta") 
;     ("feta-1" . "feta") 
;     ("feta-2" . "feta") 
;     ("finger" . "as-number") 
;     ("typewriter" . "as-dummy") 
;     ("italic" . "as-dummy") 
;     ("roman" . "as-dummy") 
;     ("script" . "as-dummy") 
;     ("large" . "as-dummy") 
;     ("Large" . "as-dummy") 
;     ("mark" . "as-number") 
;     ("number" . "as-number") 
;     ("timesig" . "as-number")
;     ("volta" . "as-number"))
; )


(define as-font-alist-alist
  '(
    (as5 .
	 (
	  (feta16 . as5)
	  (feta20 . as5)
	  (feta-nummer6 . as-number1)
	  (feta-nummer8 . as-number1)
	  (feta-braces16 . as-braces9)
	  (cmr7 . as-dummy)
	  (cmr8 . as-dummy)
	  (cmr10 . as-dummy)
	  ))
    (as9 .
	 (
	  (feta16 . as9)
	  (feta20 . as9)
	  (feta-nummer4 . as-number1)
	  (feta-nummer8 . as-number4)
	  (feta-braces16 . as-braces9)
	  (cmr7 . as-dummy)
	  (cmr8 . as-dummy)
	  (cmr10 . as-dummy)
	  (cmr12 . as-dummy)
	 ))
  ))

(define (as-properties-to-font-name size fonts properties-alist-list)
  (let* ((feta-name (properties-to-font-name fonts properties-alist-list))
	 (as-font-alist (cdr (assoc size as-font-alist-alist)))
	 (font (assoc (string->symbol feta-name) as-font-alist)))
    (if font (symbol->string (cdr font))
	(let ((e (current-error-port)))
	  (newline e)
	  (display "can't find font: " e)
	  (write feta-name e)
	  ;;(symbol->string size)
	  "as-dummy"
	  ))))

;; FIXME: making a full style-sheet is a pain, so we parasite on
;; paper16 and translate the result.
;;
(define (as-make-style-sheet size)
  (let ((sheet (make-style-sheet 'paper16)))
    (assoc-set! sheet 'properties-to-font
		(lambda (x y) (as-properties-to-font-name size x y)))
    sheet))

;;;; AsciiScript as  -- ascii art output
(define (as-scm action-name)

  (define (beam width slope thick)
	  (string-append
	   (func "set-line-char" "#")
	   (func "rline-to" width (* width slope))
	   ))

  ; simple flat slurs
  (define (bezier-sandwich l thick)
	  (let (
		(c0 (cadddr l))
		(c1 (cadr l))
		(c3 (caddr l)))
	       (let* ((x (car c0))
		      (dx (- (car c3) x))
		      (dy (- (cdr c3) (cdr c0)))
		      (rc (/ dy dx))
		      (c1-dx (- (car c1) x))
		      (c1-line-y (+ (cdr c0) (* c1-dx rc)))
		      (dir (if (< c1-line-y (cdr c1)) 1 -1))
		      (y (+ -1 (* dir (max (* dir (cdr c0)) (* dir (cdr c3)))))))
		     (string-append
		      (func "rmove-to" x y)
		      (func "put" (if (< 0 dir) "/" "\\\\"))
		      (func "rmove-to" 1 (if (< 0 dir) 1 0))
		      (func "set-line-char" "_")
		      (func "h-line" (- dx 1))
		      (func "rmove-to" (- dx 1) (if (< 0 dir) -1 0))
		      (func "put" (if (< 0 dir) "\\\\" "/"))))))


  (define (bracket arch_angle arch_width arch_height height arch_thick thick)
    ;; width now fixed?
    (let ((width 1))
	  (string-append
	   (func "rmove-to" (+ width 1) (- (/ height -2) 1))
	   (func "put" "\\\\")
	   (func "set-line-char" "|")
	   (func "rmove-to" 0 1)
	   (func "v-line" (+ height 1))
	   (func "rmove-to" 0 (+ height 1))
	   (func "put" "/")
	   )))

  (define (char i)
    (func "char" i))

  (define (define-origin a b c ) "")

  (define (end-output) 
    (func "end-output"))
  
  (define (experimental-on)
	  "")

  (define (filledbox breapth width depth height)
	  (let ((dx (+ width breapth))
		(dy (+ depth height)))
	       (string-append 
		(func "rmove-to" (* -1 breapth) (* -1 depth))
		(if (< dx dy)
		    (string-append
		     (func "set-line-char" 
			   (if (<= dx 1) "|" "#"))
		     (func "v-line" dy))
		    (string-append
		     (func "set-line-char" 
			   (if (<= dy 1) "-" "="))
		    (func "h-line" dx))))))

  (define (font-load-command name-mag command)
   ;; (display "name-mag: ")
   ;; (write name-mag)
   ;; (display "command: ")
   ;; (write command)
    (func "load-font" (car name-mag) (cdr name-mag)))

  (define (header creator generate) 
    (func "header" creator generate))

  (define (header-end) 
    (func "header-end"))

  ;; urg: this is good for half of as2text's execution time
  (define (xlily-def key val)
	  (string-append "(define " key " " (arg->string val) ")\n"))

  (define (lily-def key val)
    (if
     ;; let's not have all bloody definitions
     (or (equal? key "lilypondpaperlinewidth")
	 (equal? key "lilypondpaperstaffheight")
	 (equal? key "lilypondpaperoutputscale"))
     (string-append "(define " key " " (arg->string val) ")\n")
     ""))

  (define (no-origin) "")
  
  (define (placebox x y s) 
    (let ((ey (inexact->exact y)))
	  (string-append "(move-to " (number->string (inexact->exact x)) " "
			 (if (= 0.5 (- (abs y) (abs ey)))
			     (number->string y)
			     (number->string ey))
			 ")\n" s)))
		       
  (define (select-font name-mag-pair)
    (let* ((c (assoc name-mag-pair font-name-alist)))
      (if (eq? c #f)
	  (begin
	    (ly-warn 
	     (string-append 
	      "Programming error: No such font known " 
	      (car name-mag-pair))))
	    "")				; issue no command
	  (func "select-font" (car name-mag-pair))))

  (define (start-line height)
	  (func "start-line" height))

  (define (stop-line)
	  (func "stop-line"))

  (define (text s)
	  (func "text" s))

  (define (tuplet ht gap dx dy thick dir) "")

  (define (volta h w thick vert-start vert-end)
	  ;; urg
	  (string-append
	   (func "set-line-char" "|")
	   (func "rmove-to" 0 -4)
	   ;; definition strange-way around
	   (if (= 0 vert-start)
	      (func "v-line" h)
	       "")
	   (func "rmove-to" 1 h)
	   (func "set-line-char" "_")
	   (func "h-line" (- w 1))
	   (func "set-line-char" "|")
	   (if (= 0 vert-end)
	       (string-append
		(func "rmove-to" (- w 1) (* -1 h))
		(func "v-line" (* -1 h)))
	       "")))

(cond ((eq? action-name 'all-definitions)
	 `(begin
	    (define beam ,beam)
	    (define bracket ,bracket)
	    (define char ,char)
	    (define define-origin ,define-origin)
	    ;;(define crescendo ,crescendo)
	    (define bezier-sandwich ,bezier-sandwich)
	    ;;(define dashed-slur ,dashed-slur) 
	    ;;(define decrescendo ,decrescendo) 
	    (define end-output ,end-output)
	    (define experimental-on ,experimental-on)
	    (define filledbox ,filledbox)
	    ;;(define font-def ,font-def)
	    (define font-load-command ,font-load-command)
	    ;;(define font-switch ,font-switch)
	    (define header ,header) 
	    (define header-end ,header-end)
	    (define lily-def ,lily-def)
	    ;;(define invoke-char ,invoke-char) 
	    ;;(define invoke-dim1 ,invoke-dim1)
	    (define no-origin ,no-origin)
	    (define placebox ,placebox)
	    (define select-font ,select-font)
	    (define start-line ,start-line)
	    ;;(define stem ,stem)
	    (define stop-line ,stop-line)
	    (define stop-last-line ,stop-line)
	    (define text ,text)
	    (define tuplet ,tuplet)
	    (define volta ,volta)
	    ))
	((eq? action-name 'tuplet) tuplet)
	;;((eq? action-name 'beam) beam)
	;;((eq? action-name 'bezier-sandwich) bezier-sandwich)
	;;((eq? action-name 'bracket) bracket)
	((eq? action-name 'char) char)
	;;((eq? action-name 'crescendo) crescendo)
	;;((eq? action-name 'dashed-slur) dashed-slur) 
	;;((eq? action-name 'decrescendo) decrescendo)
	;;((eq? action-name 'experimental-on) experimental-on)
	((eq? action-name 'filledbox) filledbox)
	((eq? action-name 'select-font) select-font)
	;;((eq? action-name 'volta) volta)
	(else (error "unknown tag -- MUSA-SCM " action-name))
	)
  )

(define (scm-as-output)
  (primitive-eval (as-scm 'all-definitions)))
