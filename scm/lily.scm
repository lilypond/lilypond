; lily.scm -- implement Scheme output routines for TeX and PostScript
;
;  source file of the GNU LilyPond music typesetter
; 
; (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>


;
; This file contains various routines in Scheme that are easier to 
; do here than in C++.  At present it is an unorganised mess. Sorry. 
;


; We should repartition the entire scm side of lily in a
; more sane way, using namesspaces/modules?

(debug-enable 'backtrace)

;;; library funtions

(use-modules (ice-9 regex))

(define (number-pair?  x)
  (and (pair? x) (number? (car x)) (number? (cdr x))))

(define (type-name  predicate)
  (cond
   ((eq? predicate dir?) "direction")
   ((eq? predicate number-pair?) "pair of numbers")
   ((eq? predicate ly-input-location?) "input location")   
   ((eq? predicate ly-element?) "graphic element")
   ((eq? predicate pair?) "pair")
   ((eq? predicate integer?) "integer")
   ((eq? predicate list?) "list")
   ((eq? predicate symbol?) "symbol")
   ((eq? predicate string?) "string")
   ((eq? predicate boolean?) "boolean")
   ((eq? predicate moment?) "moment")
   ((eq? predicate number?) "number")
   ((eq? predicate char?) "char")
   ((eq? predicate input-port?) "input port")
   ((eq? predicate output-port?) "output port")   
   ((eq? predicate vector?) "vector")
   ((eq? predicate procedure?) "procedure") 
   (else "unknown type")
  ))


;; The regex module may not be available, or may be broken.
(define use-regex
  (let ((os (string-downcase (vector-ref (uname) 0))))
    (not (equal? "cygwin" (substring os 0 (min 6 (string-length os)))))))

;; If you have trouble with regex, define #f
(define use-regex #t)
;;(define use-regex #f)

;; do nothing in .scm output
(define (comment s) "")

;; URG guile-1.3/1.4 compatibility
(define (ly-eval x) (eval2 x #f))

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


(define (numbers->string l)
  (apply string-append (map ly-number->string l)))

; (define (chop-decimal x) (if (< (abs x) 0.001) 0.0 x))

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


(define (control->string c)
  (string-append (number->string (car c)) " "
		 (number->string (cdr c)) " "))

(define (font i)
  (string-append
   "font"
   (make-string 1 (integer->char (+ (char->integer #\A) i)))
   ))

(define (scm-scm action-name)
  1)

(define security-paranoia #f)


;; silly, use alist? 
(define (find-notehead-symbol duration style)
  (case style
   ((cross) "2cross")
   ((harmonic) "0mensural")
   ((baroque) 
    (string-append (number->string duration)
		   (if (< duration 0) "mensural" "")))
   ((default) (number->string duration))
   (else
    (string-append (number->string duration) (symbol->string style)))))


;;;;;;;; TeX

;; this is silly, can't we use something like
;; roman-0, roman-1 roman+1 ?
(define cmr-alist 
  '(("bold" . "cmbx") 
    ("brace" . "feta-braces")
    ("default" . "cmr10")
    ("dynamic" . "feta-din") 
    ("feta" . "feta") 
    ("feta-1" . "feta") 
    ("feta-2" . "feta") 
    ("typewriter" . "cmtt") 
    ("italic" . "cmti") 
    ("msam" . "msam") 
    ("roman" . "cmr") 
    ("script" . "cmr") 
    ("large" . "cmbx") 
    ("Large" . "cmbx") 
    ("mark" . "feta-nummer") 
    ("finger" . "feta-nummer")
    ("timesig" . "feta-nummer")
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
	     
(define default-script-alist '())

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

(define (fontify name exp)
  (string-append (select-font name)
		 exp)
  )

;;;;;;;;;;;;;;;;;;;;


; Make a function that checks score element for being of a specific type. 
(define (make-type-checker symbol)
  (lambda (elt)
    ;;(display  symbol)
    ;;(eq? #t (ly-get-elt-property elt symbol))
    (not (eq? #f (memq symbol (ly-get-elt-property elt 'interfaces))))
    ))

;;;;;;;;;;;;;;;;;;; TeX output
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
  
  (define (dashed-line thick on off dx dy)
    (embedded-ps ((ps-scm 'dashed-line) thick on off dx dy)))

  (define (decrescendo thick w h cont)
    (embedded-ps ((ps-scm 'decrescendo) thick w h cont)))

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
	(begin
; uncomment for some stats about lily memory	  
;		(display (gc-stats))
    (string-append "\n\\EndLilyPondOutput"
		   ; Put GC stats here.
		   )))
  
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

     ;; URG: ly-gulp-file: now we can't use scm output without Lily
     (if use-regex
	 ;; fixed in 1.3.4 for powerpc -- broken on Windows
	 (regexp-substitute/global #f "\n"
				   (ly-gulp-file "lily.ps") 'pre " %\n" 'post)
	 (ly-gulp-file "lily.ps"))
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
	  (if use-regex
	      (regexp-substitute/global #f "\\\\" s 'pre "$\\backslash$" 'post)
	      (begin (display "warning: not paranoid") (newline) s))
	  s))
      
  (define (lily-def key val)
    (string-append
     "\\def\\"
     (if use-regex
	 ;; fixed in 1.3.4 for powerpc -- broken on Windows
	 (regexp-substitute/global #f "_"
				   (output-tex-string key) 'pre "X" 'post)
	 (output-tex-string key))
     "{" (output-tex-string val) "}\n"))

  (define (number->dim x)
    (string-append 
     (ly-number->string x) " pt "))

  (define (placebox x y s) 
    (string-append 
     "\\placebox{"
     (number->dim y) "}{" (number->dim x) "}{" s "}\n"))

  (define (bezier-sandwich l thick)
    (embedded-ps ((ps-scm 'bezier-sandwich) l thick)))

  (define (start-line ht)
      (string-append"\\vbox to " (number->dim ht) "{\\hbox{%\n"))

  (define (stop-line) 
    "}\\vss}\\interscoreline\n")
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

  (define (define-origin file line col)
    ; use this for column positions
     (string-append "\\special{src:" (number->string line) ":"
        (number->string col) " " file "}"
	 ;; arg, the clueless take over the mailing list...
;	 "\\special{-****-These-warnings-are-harmless-***}"
;	 "\\special{-****-PLEASE-read-http://appel.lilypond.org/wiki/index.php3?PostProcessing-****}"
	)

     ; line numbers only:
    ;(string-append "\\special{src:" (number->string line) " " file "}")
)
    ; no origin  info: return empty string
    ; ""
  ; no-origin not yet supported by Xdvi
  (define (no-origin) "")
  
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
	    (define dashed-line ,dashed-line) 
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
	    (define define-origin ,define-origin)
	    (define no-origin ,no-origin)
	    ))

	((eq? action-name 'beam) beam)
	((eq? action-name 'tuplet) tuplet)
	((eq? action-name 'bracket) bracket)
	((eq? action-name 'crescendo) crescendo)
	((eq? action-name 'dashed-line) dashed-line) 
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

  (define (dashed-line thick on off dx dy)
    (string-append 
     (number->string dx)
     " "
     (number->string dy)
     " "
     (number->string thick) 
     " [ "
     (number->string on)
     " "
     (number->string off)
     " ] 0 draw_dashed_line"))

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
     ;; URG: now we can't use scm output without Lily
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


  (define (define-origin a b c ) "")
  (define (no-origin) "")
  
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
	    (define dashed-line ,dashed-line) 
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
	    (define no-origin ,no-origin)
	    (define define-origin ,define-origin)
	    ))
	((eq? action-name 'tuplet) tuplet)
	((eq? action-name 'beam) beam)
	((eq? action-name 'bezier-sandwich) bezier-sandwich)
	((eq? action-name 'bracket) bracket)
	((eq? action-name 'char) char)
	((eq? action-name 'crescendo) crescendo)
	((eq? action-name 'dashed-line) dashed-line) 
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

(define (sign x)
  (if (= x 0)
      1
      (if (< x 0) -1 1)))

;;;; AsciiScript as
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
    (func "load-font" (car name-mag) (magstep (cdr name-mag))))

  (define (header creator generate) 
    (func "header" creator generate))

  (define (header-end) 
    (func "header-end"))

  ;; urg: this is good for half of as2text's execution time
  (define (xlily-def key val)
	  (string-append "(define " key " " (arg->string val) ")\n"))

  (define (lily-def key val)
	  (if 
	   (or (equal? key "mudelapaperlinewidth")
	       (equal? key "mudelapaperstaffheight"))
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

;; urg: Use when standalone, do:
;; (define ly-gulp-file scm-gulp-file)
(define (scm-gulp-file name)
  (set! %load-path 
	(cons (string-append (getenv 'LILYPONDPREFIX) "/ly")
	      (cons (string-append (getenv 'LILYPONDPREFIX) "/ps")
		    %load-path)))
  (let ((path (%search-load-path name)))
       (if path
	   (gulp-file path)
	   (gulp-file name))))

(define (scm-tex-output)
  (ly-eval (tex-scm 'all-definitions)))
				
(define (scm-ps-output)
  (ly-eval (ps-scm 'all-definitions)))

(define (scm-as-output)
  (ly-eval (as-scm 'all-definitions)))
	
(define (index-cell cell dir)
  (if (equal? dir 1)
      (cdr cell)
      (car cell)))

(define major-scale
  '(
    (0 . 0)
    (1 . 0)
    (2 . 0)
    (3 . 0)
    (4 . 0)
    (5 . 0)
    (6 . 0)
    )
  )

(begin
  (eval-string (ly-gulp-file "interface.scm"))
  (eval-string (ly-gulp-file "slur.scm"))
  (eval-string (ly-gulp-file "font.scm"))  
  (eval-string (ly-gulp-file "generic-property.scm"))
  (eval-string (ly-gulp-file "basic-properties.scm"))
  (eval-string (ly-gulp-file "chord-names.scm"))
  (eval-string (ly-gulp-file "element-descriptions.scm"))  
 )
