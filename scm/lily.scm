; lily.scm -- implement Scheme output routines for TeX and PostScript
;
;  source file of the GNU LilyPond music typesetter
; 
; (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>


;(debug-enable 'backtrace)

;;; library funtions

(use-modules (ice-9 regex))

;; do nothing in .scm output
(define (comment s) "")

(define (mm-to-pt x)
  (* (/ 72.27 25.40) x)
  )

(define (cons-map f x)
  (cons (f (car x)) (f (cdr x))))

(define (reduce operator list)
      (if (null? (cdr list)) (car list)
	  (operator (car list) (reduce operator (cdr list)))
	  )
      )

(define (glue-2-strings a b) 
  (string-append a " " b))

(define (numbers->string l)
  (reduce glue-2-strings (map number->string l)))

(define (chop-decimal x) (if (< (abs x) 0.001) 0.0 x))

(define (number->octal-string x)
  (let* ((n (inexact->exact x))
         (n64 (quotient n 64))
         (n8 (quotient (- n (* n64 64)) 8)))
    (string-append
     (number->string n64)
     (number->string n8)
     (number->string (remainder (- n (+ (* n64 64) (* n8 8))) 8)))))

(define (inexact->string x radix)
  (let ((n (inexact->exact x)))
    (number->string n radix)))


(define
  (control->string c)
  (string-append
   (string-append (number->string (car c)) " ")
   (string-append (number->string (cdr c)) " ")))


(define (font i)
  (string-append
   "font"
   (make-string 1 (integer->char (+ (char->integer #\A) i)))
   ))



(define (scm-scm action-name)
  1)

(define security-paranoia #f)


;; See documentation of Item::visibility_lambda_
(define (postbreak-only-visibility d) (if (= d 1) '(#f . #f) '(#t . #t)))
(define (spanbar-non-postbreak-visibility d) (if (= d -1) '(#t . #t) '(#f . #f)))
(define (all-visibility d) '(#f . #f))
(define (non-postbreak-visibility d) (if (= d 1) '(#t . #t) '(#f . #f)))
(define (non-prebreak-visibility d) (if (= d -1) '(#t . #t) '(#f . #f)))


;; Score_span_bars are only visible at start of line
;; i.e. if break_dir == RIGHT == 1
(define Span_bar_engraver-visibility non-postbreak-visibility)
(define Base_span_bar_engraver-visibility non-postbreak-visibility)
(define mark-visibility non-prebreak-visibility)
(define Span_score_bar_engraver-visibility postbreak-only-visibility)
(define Piano_bar_engraver-visibility postbreak-only-visibility)
(define Staff_group_bar_engraver-visibility postbreak-only-visibility)

;; Spacing constants for prefatory matter.
;;
;; rules for this spacing are much more complicated than this. See [Wanske] page 126 -- 134, [Ross] pg 143 -- 147
;;
;;

;; (Measured in interlines? -- jcn)
(define space-alist
 '(
   (("" "Clef_item") . (minimum-space 1.0))
   (("" "Staff_bar") . (minimum-space 0.0))
   (("" "Clef_item") . (minimum-space 1.0))
   (("" "Key_item") . (minimum-space 0.5))
   (("" "Span_bar") . (extra-space 0.0))
   (("" "Time_signature") . (extra-space 0.0))
   (("" "begin-of-note") . (minimum-space 1.5))
   (("Clef_item" "Key_item") . (minimum-space 4.0))
   (("Key_item" "Time_signature") . (extra-space 1.0))
   (("Clef_item"  "Time_signature") . (minimum-space 3.5))
   (("Staff_bar" "Clef_item") .   (minimum-space 1.0))
   (("Clef_item"  "Staff_bar") .  (minimum-space 3.7))
   (("Time_signature" "Staff_bar") .  (minimum-space 2.0))
   (("Key_item"  "Staff_bar") .  (extra-space 1.0))
   (("Span_bar" "Clef_item") .   (extra-space 1.0))
   (("Clef_item"  "Span_bar") . (minimum-space 3.7))
   (("Time_signature" "Span_bar") . (minimum-space 2.0))
   (("Key_item"  "Span_bar") . (minimum-space 2.5))
   (("Staff_bar" "Time_signature") . (minimum-space 1.5)) ;double check this.
   (("Time_signature" "begin-of-note") . (extra-space 2.0)) ;double check this.
   (("Key_item" "begin-of-note") . (extra-space 2.5))
   (("Staff_bar" "begin-of-note") . (extra-space 1.0))
   (("Clef_item" "begin-of-note") . (minimum-space 5.0))
   (("" "Breathing_sign") . (minimum-space 0.0))
   (("Breathing_sign" "Key_item") . (minimum-space 1.5))
   (("Breathing_sign" "begin-of-note") . (minimum-space 1.0))
   (("Breathing_sign" "Staff_bar") . (minimum-space 1.5))
   (("Breathing_sign" "Clef_item") . (minimum-space 2.0))
   )
)
 
(define (break-align-spacer this next)
  (let ((entry (assoc `(,this ,next) space-alist)))
    (if entry
	(cdr entry)
	(begin (ly-warn (string-append "Unknown spacing pair `" this "', `" next "'"))
	       '(minimum-space 0.0)))))
  


;;;;;;;; TeX

;; this is silly, can't we use something like
;; roman-0, roman-1 roman+1 ?
(define cmr-alist 
  '(("bold" . "cmbx") 
    ("brace" . "feta-braces")
    ("dynamic" . "feta-din") 
    ("feta" . "feta") 
    ("feta-1" . "feta") 
    ("feta-2" . "feta") 
    ("finger" . "feta-nummer") 
    ("typewriter" . "cmtt") 
    ("italic" . "cmti") 
    ("roman" . "cmr") 
    ("script" . "cmr") 
    ("large" . "cmbx") 
    ("Large" . "cmbx") 
    ("mark" . "feta-nummer") 
    ("number" . "feta-nummer") 
    ("volta" . "feta-nummer"))
)

(define (string-encode-integer i)
  (cond
   ((= i  0) "o")
   ((< i 0)   (string-append "n" (string-encode-integer (- i))))
   (else (string-append
	  (make-string 1 (integer->char (+ 65 (modulo i 26))))
	  (string-encode-integer (quotient i 26))
	 )
   )
  )
  )

(define (magstep i)
  (cdr (assoc i '((-4 . 482)
		  (-3 . 579)
		  (-2 . 694)
		  (-1 . 833)
		  (0 . 1000)
		  (1 . 1200) 
		  (2 . 1440)
		  (3 . 1728)
		  (4 . 2074))
	      )
       )
  )
	     
(define script-alist '())
(define (articulation-to-scriptdef a)
  (assoc a script-alist)
  )

;; Map style names to TeX font names.  Return false if 
;; no font name found. 
(define (style-to-cmr s)
  (assoc s cmr-alist )
  )
	    


(define font-name-alist  '())
(define (font-command name-mag)
    (cons name-mag
	  (string-append  "magfont"
			  (string-encode-integer (hashq (car name-mag) 1000000))
			  "m"
			  (string-encode-integer (cdr name-mag)))

	  )
    )
(define (define-fonts names)
  (set! font-name-alist (map font-command names))
  (apply string-append
	 (map (lambda (x)
		(font-load-command (car x) (cdr x))) font-name-alist)
  ))
  

(define (tex-scm action-name)
  (define (unknown) 
    "%\n\\unknown%\n")


  (define (select-font font-name-symbol)
    (let*
	(
	 (c (assoc font-name-symbol font-name-alist))
	 )

      (if (eq? c #f)
	  (begin
	    (ly-warn (string-append
		      "Programming error: No such font known " (car font-name-symbol)))
	    "")				; issue no command
	  (string-append "\\" (cdr c)))
      
      
      ))
  
  (define (beam width slope thick)
    (embedded-ps ((ps-scm 'beam) width slope thick)))

  (define (bracket arch_angle arch_width arch_height width height arch_thick thick)
    (embedded-ps ((ps-scm 'bracket) arch_angle arch_width arch_height width height arch_thick thick)))

  (define (dashed-slur thick dash l)
    (embedded-ps ((ps-scm 'dashed-slur)  thick dash l)))

  (define (crescendo thick w h cont)
    (embedded-ps ((ps-scm 'crescendo) thick w h cont)))

  (define (char i)
    (string-append "\\char" (inexact->string i 10) " "))
  
  (define (decrescendo thick w h cont)
    (embedded-ps ((ps-scm 'decrescendo) thick w h cont)))

   ;This sets CTM so that you get to the currentpoint
  ; by executing a 0 0 moveto

  
 

  (define (font-load-command name-mag command)
    (string-append
     "\\font\\" command "="
     (symbol->string (car name-mag))
     " scaled "
     (number->string (magstep (cdr name-mag)))
     "\n"))


  (define (embedded-ps s)
    (string-append "\\embeddedps{" s "}"))

  (define (comment s)
    (string-append "% " s))
  
  (define (end-output) 
    "\n\\EndLilyPondOutput")
  
  (define (experimental-on)
    "")

  (define (font-switch i)
    (string-append
     "\\" (font i) "\n"))

  (define (font-def i s)
    (string-append
     "\\font" (font-switch i) "=" s "\n"))

  (define (header-end)
    (string-append
     "\\special{! "
     ; fixed in 1.3.4
     ;(ly-gulp-file "lily.ps")

     (regexp-substitute/global #f "\n" (ly-gulp-file "lily.ps") 'pre " %\n" 'post)
     "}"
     "\\input lilyponddefs \\turnOnPostScript"))

  (define (header creator generate) 
    (string-append
     "%created by: " creator generate "\n"))

  (define (invoke-char s i)
    (string-append 
     "\n\\" s "{" (inexact->string i 10) "}" ))

  (define (invoke-dim1 s d)
    (string-append
     "\n\\" s "{" (number->dim d) "}"))
  (define (pt->sp x)
    (* 65536 x))
  
  ;;
  ;; need to do something to make this really safe.
  ;;
  (define (output-tex-string s)
      (if security-paranoia
	  (regexp-substitute/global #f "\\\\" s 'pre "$\\backslash$" 'post)
	  s))
      
  (define (lily-def key val)
    (string-append
     "\\def\\"
     ; fixed in 1.3.4
     (regexp-substitute/global #f "_" (output-tex-string key) 'pre "X" 'post)
     ;(output-tex-string key)
     "{" (output-tex-string val) "}\n"))

  (define (number->dim x)
    (string-append 
     (number->string  (chop-decimal x)) " pt "))

  (define (placebox x y s) 
    (string-append 
     "\\placebox{"
     (number->dim y) "}{" (number->dim x) "}{" s "}\n"))

  (define (bezier-sandwich l thick)
    (embedded-ps ((ps-scm 'bezier-sandwich) l thick)))

  (define (start-line ht)
      (string-append"\\vbox to " (number->dim ht) "{\\hbox{%\n"))

  (define (stop-line) 
    "}\\vss}\\interscoreline")
  (define (stop-last-line)
    "}\\vss}")
  (define (filledbox breapth width depth height) 
    (string-append 
     "\\kern" (number->dim (- breapth))
     "\\vrule width " (number->dim (+ breapth width))
     "depth " (number->dim depth)
     "height " (number->dim height) " "))

  (define (text s)
    (string-append "\\hbox{" (output-tex-string s) "}"))
  
  (define (tuplet ht gapx dx dy thick dir)
    (embedded-ps ((ps-scm 'tuplet) ht gapx dx dy thick dir)))

  (define (volta h w thick vert_start vert_end)
    (embedded-ps ((ps-scm 'volta) h w thick vert_start vert_end)))

  ;; TeX
  ;; The procedures listed below form the public interface of TeX-scm.
  ;; (should merge the 2 lists)
  (cond ((eq? action-name 'all-definitions)
	 `(begin
	    (define font-load-command ,font-load-command)
	    (define beam ,beam)
	    (define bezier-sandwich ,bezier-sandwich)
	    (define bracket ,bracket)
	    (define char ,char)
	    (define crescendo ,crescendo)
	    (define dashed-slur ,dashed-slur) 
	    (define decrescendo ,decrescendo) 
	    (define end-output ,end-output)
	    (define experimental-on ,experimental-on)
	    (define filledbox ,filledbox)
	    (define font-def ,font-def)
	    (define font-switch ,font-switch)
	    (define header-end ,header-end)
	    (define lily-def ,lily-def)
	    (define header ,header) 
	    (define invoke-char ,invoke-char) 
	    (define invoke-dim1 ,invoke-dim1)
	    (define placebox ,placebox)
	    (define select-font ,select-font)
	    (define start-line ,start-line)
	    (define stop-line ,stop-line)
	    (define stop-last-line ,stop-last-line)
	    (define text ,text)
	    (define tuplet ,tuplet)
	    (define volta ,volta)
	    ))

	((eq? action-name 'beam) beam)
	((eq? action-name 'tuplet) tuplet)
	((eq? action-name 'bracket) bracket)
	((eq? action-name 'crescendo) crescendo)
	((eq? action-name 'dashed-slur) dashed-slur) 
	((eq? action-name 'decrescendo) decrescendo) 
	((eq? action-name 'end-output) end-output)
	((eq? action-name 'experimental-on) experimental-on)
	((eq? action-name 'font-def) font-def)
	((eq? action-name 'font-switch) font-switch)
	((eq? action-name 'header-end) header-end)
	((eq? action-name 'lily-def) lily-def)
	((eq? action-name 'header) header) 
	((eq? action-name 'invoke-char) invoke-char) 
	((eq? action-name 'invoke-dim1) invoke-dim1)
	((eq? action-name 'placebox) placebox)
	((eq? action-name 'bezier-sandwich) bezier-sandwich)
	((eq? action-name 'start-line) start-line)
	((eq? action-name 'stem) stem)
	((eq? action-name 'stop-line) stop-line)
	((eq? action-name 'stop-last-line) stop-last-line)
	((eq? action-name 'volta) volta)
	(else (error "unknown tag -- PS-TEX " action-name))
	)
  )


;;;;;;;;;;;; PS
(define (ps-scm action-name)


  ;; alist containing fontname -> fontcommand assoc (both strings)
  (define font-alist '())
  (define font-count 0)
  (define current-font "")

  
  (define (cached-fontname i)
    (string-append
     "lilyfont"
     (make-string 1 (integer->char (+ 65 i)))))
    
  (define (mag-to-size m)
    (number->string (case m 
		      (0 12)
		      (1 12)
		      (2 14) ; really: 14.400
		      (3 17) ; really: 17.280
		      (4 21) ; really: 20.736
		      (5 24) ; really: 24.888
		      (6 30) ; really: 29.856
		      )))
  
  
  (define (select-font font-name-symbol)
    (let*
	(
	 (c (assoc font-name-symbol font-name-alist))
	 )

      (if (eq? c #f)
	  (begin
	    (ly-warn (string-append
		      "Programming error: No such font known " (car font-name-symbol)))
	    "")				; issue no command
	  (string-append " " (cdr c) " "))
      
      
      ))

    (define (font-load-command name-mag command)
      (string-append
       "/" command
       " { /"
       (symbol->string (car name-mag))
       " findfont "
       (number->string (magstep (cdr name-mag)))
       " 1000 div 12 mul  scalefont setfont } bind def "
       "\n"))


  (define (beam width slope thick)
    (string-append
     (numbers->string (list width slope thick)) " draw_beam" ))

  (define (comment s)
    (string-append "% " s))

  (define (bracket arch_angle arch_width arch_height width height arch_thick thick)
    (string-append
     (numbers->string (list arch_angle arch_width arch_height width height arch_thick thick)) " draw_bracket" ))

  (define (char i)
    (invoke-char " show" i))

  (define (crescendo thick w h cont )
    (string-append 
     (numbers->string (list w h (inexact->exact cont) thick))
     " draw_crescendo"))

  ;; what the heck is this interface ?
  (define (dashed-slur thick dash l)
    (string-append 
     (apply string-append (map control->string l)) 
     (number->string thick) 
     " [ "
     (number->string dash)
     " "
     (number->string (* 10 thick))	;UGH.  10 ?
     " ] 0 draw_dashed_slur"))

  (define (decrescendo thick w h cont)
    (string-append 
     (numbers->string (list w h (inexact->exact cont) thick))
     " draw_decrescendo"))


  (define (end-output)
    "\nshowpage\n")
  
  (define (experimental-on) "")
  
  (define (filledbox breapth width depth height) 
    (string-append (numbers->string (list breapth width depth height))
		   " draw_box" ))

  ;; obsolete?
  (define (font-def i s)
    (string-append
     "\n/" (font i) " {/" 
     (substring s 0 (- (string-length s) 4))
     " findfont 12 scalefont setfont} bind def \n"))

  (define (font-switch i)
    (string-append (font i) " "))

  (define (header-end)
    (string-append
     (ly-gulp-file "lilyponddefs.ps")
     " {exch pop //systemdict /run get exec} "
     (ly-gulp-file "lily.ps")
     "{ exch pop //systemdict /run get exec } "
    ))
  
  (define (lily-def key val)

     (if (string=? (substring key 0 (min (string-length "mudelapaper") (string-length key))) "mudelapaper")
	 (string-append "/" key " {" val "} bind def\n")
	 (string-append "/" key " (" val ") def\n")
	 )
     )

  (define (header creator generate) 
    (string-append
     "%!PS-Adobe-3.0\n"
     "%%Creator: " creator generate "\n"))
  
  (define (invoke-char s i)
    (string-append 
     "(\\" (inexact->string i 8) ") " s " " ))
  
  (define (invoke-dim1 s d) 
    (string-append
     (number->string (* d  (/ 72.27 72))) " " s ))

  (define (placebox x y s) 
    (string-append 
     (number->string x) " " (number->string y) " {" s "} placebox "))

  (define (bezier-sandwich l thick)
    (string-append 
     (apply string-append (map control->string l))
     (number->string  thick)
     " draw_bezier_sandwich"))

  (define (start-line height)
	  "\nstart_line {\n")
  
  (define (stem breapth width depth height) 
    (string-append (numbers->string (list breapth width depth height))
		   " draw_box" ))

  (define (stop-line)
      "}\nstop_line\n")

  (define (text s)
    (string-append "(" s ") show  "))


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


  ;; PS
  (cond ((eq? action-name 'all-definitions)
	 `(begin
	    (define beam ,beam)
	    (define tuplet ,tuplet)
	    (define bracket ,bracket)
	    (define char ,char)
	    (define crescendo ,crescendo)
	    (define volta ,volta)
	    (define bezier-sandwich ,bezier-sandwich)
	    (define dashed-slur ,dashed-slur) 
	    (define decrescendo ,decrescendo) 
	    (define end-output ,end-output)
	    (define experimental-on ,experimental-on)
	    (define filledbox ,filledbox)
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
	    (define text ,text)
	    ))
	((eq? action-name 'tuplet) tuplet)
	((eq? action-name 'beam) beam)
	((eq? action-name 'bezier-sandwich) bezier-sandwich)
	((eq? action-name 'bracket) bracket)
	((eq? action-name 'char) char)
	((eq? action-name 'crescendo) crescendo)
	((eq? action-name 'dashed-slur) dashed-slur) 
	((eq? action-name 'decrescendo) decrescendo)
	((eq? action-name 'experimental-on) experimental-on)
	((eq? action-name 'filledbox) filledbox)
	((eq? action-name 'select-font) select-font)
	((eq? action-name 'volta) volta)
	(else (error "unknown tag -- PS-SCM " action-name))
	)
  )


(define (arg->string arg)
  (cond ((number? arg) (inexact->string arg 10))
	((string? arg) (string-append "\"" arg "\""))
	((symbol? arg) (string-append "\"" (symbol->string arg) "\""))))

(define (func name . args)
  (string-append 
   "(" name 
   (if (null? args) 
       ""
       (apply string-append 
	      (map (lambda (x) (string-append " " (arg->string x))) args)))
   ")\n"))


;;;; AsciiScript as
(define (as-scm action-name)

  (define (beam width slope thick)
	  (string-append
	   (func "set-line-char" "#")
	   (func "rline-to" width (* width slope))
	   ))

  (define (bracket arch_angle arch_width arch_height width height arch_thick thick)
	  (string-append
	   (func "rmove-to" (+ width 1) (- (/ height -2) 1))
	   (func "put" "\\\\")
	   (func "set-line-char" "|")
	   (func "rmove-to" 0 1)
	   (func "v-line" (+ height 1))
	   (func "rmove-to" 0 (+ height 1))
	   (func "put" "/")
	   ))

  (define (char i)
    (func "char" i))

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
    (func "load-font" (car name-mag) (magstep (cdr name-mag))))

  (define (header creator generate) 
    (func "header" creator generate))

  (define (header-end) 
    (func "header-end"))

  (define (lily-def key val)
	  (string-append "(define " key " " (arg->string val) ")\n"))

  (define (placebox x y s) 
    (string-append (func "move-to" x y) s))

  (define (select-font font-name-symbol)
    (let* ((c (assoc font-name-symbol font-name-alist)))
      (if (eq? c #f)
	  (begin
	    (ly-warn 
	     (string-append 
	      "Programming error: No such font known " 
	      (car font-name-symbol)))
	    "")				; issue no command
	  (func "select-font" (car font-name-symbol)))))

  (define (start-line height)
	  (func "start-line" height))

  (define (stop-line)
	  (func "stop-line"))

  (define (text s)
	  (func "text" s))

;  (define (volta h w thick vert_start vert_end)
;     (func "draw-volta" h w thick vert_start vert_end))

  (define (volta h w thick vert-start vert-end)
	  (string-append
	   (if #t  ;(= 0 vert-start)
	       ""
	      (func "v-line" h))
	   ""
	   ;(func "rmove-to" 0 h)
	   ;(func "h-line" w)
	   (if #t ;(= 0 vert-end)
	       ""
	       (string-append
		(func "rmove-to" w 0)
		(func "v-line" (* -1 h))))))

  (cond ((eq? action-name 'all-definitions)
	 `(begin
	    (define beam ,beam)
	    (define bracket ,bracket)
	    (define char ,char)
	    ;;(define crescendo ,crescendo)
	    ;(define bezier-sandwich ,bezier-sandwich)
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
	    (define placebox ,placebox)
	    (define select-font ,select-font)
	    (define start-line ,start-line)
	    ;;(define stem ,stem)
	    (define stop-line ,stop-line)
	    (define stop-last-line ,stop-line)
	    (define text ,text)
	    ;;(define tuplet ,tuplet)
	    (define volta ,volta)
	    ))
	;;((eq? action-name 'tuplet) tuplet)
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


(define (gulp-file name)
  (let* ((port (open-file name "r"))
	 (content (let loop ((text ""))
		       (let ((line (read-line port)))
			    (if (or (eof-object? line)
				    (not line)) 
				text
				(loop (string-append text line "\n")))))))
	(close port)
	content))

(define (scm-gulp-file name)
  (set! %load-path 
	(cons (string-append 
	       (getenv 'LILYPONDPREFIX) "/ps") %load-path))
  (let ((path (%search-load-path name)))
       (if path
	   (gulp-file path)
	   (gulp-file name))))

(define (scm-tex-output)
  (eval (tex-scm 'all-definitions)))
				
(define (scm-ps-output)
  (eval (ps-scm 'all-definitions)))

(define (scm-as-output)
  (eval (as-scm 'all-definitions)))
				
; Russ McManus, <mcmanus@IDT.NET>  
; 
; I use the following, which should definitely be provided somewhere
; in guile, but isn't, AFAIK:
; 
; 

(define (hash-table-for-each fn ht)
  (do ((i 0 (+ 1 i)))
      ((= i (vector-length ht)))
    (do ((alist (vector-ref ht i) (cdr alist)))
	((null? alist) #t)
      (fn (car (car alist)) (cdr (car alist))))))

(define (hash-table-map fn ht)
  (do ((i 0 (+ 1 i))
       (ret-ls '()))
      ((= i (vector-length ht)) (reverse ret-ls))
    (do ((alist (vector-ref ht i) (cdr alist)))
	((null? alist) #t)
      (set! ret-ls (cons (fn (car (car alist)) (cdr (car alist))) ret-ls)))))



(define (index-cell cell dir)
  (if (equal? dir 1)
      (cdr cell)
      (car cell)))

;
; How should a  bar line behave at a break? 
;
(define (break-barline glyph dir)
   (let ((result (assoc glyph 
			'((":|:" . (":|" . "|:"))
			  ("|" . ("|" . ""))
			  ("|s" . (nil . "|"))
			  ("|:" . ("|" . "|:"))
			  ("|." . ("|." . nil))
			  (":|" . (":|" . nil))
			  ("||" . ("||" . nil))
			  (".|." . (".|." . nil))
			  ("scorebar" . (nil . "scorepostbreak"))
			  ("brace" . (nil . "brace"))
			  ("bracket" . (nil . "bracket"))  
			  )
			)))

     (if (equal? result #f)
	 (ly-warn (string-append "Unknown bar glyph: `" glyph "'"))
	 (index-cell (cdr result) dir))
     )
   )
     

(define (slur-ugly ind ht)
  (if (and
;       (< ht 4.0)
       (< ht (* 4 ind))
       (> ht (* 0.4 ind))
       (> ht (+ (* 2 ind) -4))
       (< ht (+ (* -2 ind) 8)))
      #f
      (cons ind  ht)
  ))
