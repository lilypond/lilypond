;;; lily.scm -- implement Scheme output routines for TeX and PostScript
;;;
;;;  source file of the GNU LilyPond music typesetter
;;; 
;;; (c) 1998--2000 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Han-Wen Nienhuys <hanwen@cs.uu.nl>


;;;
;;; This file contains various routines in Scheme that are easier to 
;;; do here than in C++.  At present it is an unorganised mess. Sorry. 


;;; We should repartition the entire scm side of lily in a
;;; more sane way, using namesspaces/modules?

(debug-enable 'backtrace)


(define point-and-click #f)

;;; library funtions

(use-modules (ice-9 regex))

(define (number-pair?  x)
  (and (pair? x) (number? (car x)) (number? (cdr x))))
(define (boolean-or-symbol? x) (or boolean? x) (or symbol? x))
(define (number-or-string? x) (or (number? x) (string? x)))
(define markup?
  (lambda (x) (or (string? x) (list? x))))



;; ugh: code dup ; merge.
(define (object-type obj)
  (cond
   ((dir? obj) "direction")
   ((number-pair? obj) "pair of numbers")
   ((ly-input-location? obj) "input location")   
   ((ly-grob? obj) "graphic element")
   ((pair? obj) "pair")
   ((integer? obj) "integer")
   ((list? obj) "list")
   ((symbol? obj) "symbol")
   ((string? obj) "string")
   ((boolean? obj) "boolean")
   ((moment? obj) "moment")
   ((number? obj) "number")
   ((char? obj) "char")
   ((input-port? obj) "input port")
   ((output-port? obj) "output port")   
   ((vector? obj) "vector")
   ((procedure? obj) "procedure") 
   (else "unknown type")
  ))


(define (type-name  predicate)
  (cond
   ((eq? predicate dir?) "direction")
   ((eq? predicate number-pair?) "pair of numbers")
   ((eq? predicate ly-input-location?) "input location")   
   ((eq? predicate ly-grob?) "graphic element")
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


(define (uniqued-alist  alist acc)
  (if (null? alist) acc
      (if (assoc (caar alist) acc)
	  (uniqued-alist (cdr alist) acc)
	  (uniqued-alist (cdr alist) (cons (car alist) acc)
  ))))


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
    (string-append (number->string duration) (symbol->string style))))
  )


;;;;;;;; TeX

(define (string-encode-integer i)
  (cond
   ((= i  0) "o")
   ((< i 0)   (string-append "n" (string-encode-integer (- i))))
   (else (string-append
	  (make-string 1 (integer->char (+ 65 (modulo i 26))))
	  (string-encode-integer (quotient i 26))
	  ))
   )
  )

(define default-script-alist '())

(define font-name-alist  '())
(define (tex-encoded-fontswitch name-mag)
  (let* (
	 (iname-mag (car name-mag))
	 (ename-mag (cdr name-mag))
	 )
    (cons iname-mag
	  (cons ename-mag
		(string-append  "magfont"
			  (string-encode-integer
			   (hashq (car ename-mag) 1000000))
			  "m"
			  (string-encode-integer
			   (inexact->exact (* 1000 (cdr ename-mag))))

			  )
		)
    )))

(define (define-fonts internal-external-name-mag-pairs)
  (set! font-name-alist (map tex-encoded-fontswitch
			     internal-external-name-mag-pairs))
  (apply string-append
	 (map (lambda (x)
		(font-load-command (car x) (cdr x)))
	      (map cdr font-name-alist)  

  )))

(define (fontify name-mag-pair exp)
  (string-append (select-font name-mag-pair)
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


  (define (select-font name-mag-pair)
    (let*
	(
	 (c (assoc name-mag-pair font-name-alist))
	 )

      (if (eq? c #f)
	  (begin
	    (display "FAILED\n")
	    (display (object-type (car name-mag-pair)))
	    (display (object-type (caaar font-name-alist)))

	    (ly-warn (string-append
		      "Programming error: No such font known "
		      (car name-mag-pair) " "
		      (number->string (cdr name-mag-pair))
		      ))
	    "") ; issue no command
	  (string-append "\\" (cddr c)))
      
      
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
     (car name-mag)
     " scaled "
     (number->string (inexact->exact (* 1000  (cdr name-mag))))
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
     "\\input lilyponddefs\\newdimen\\outputscale \\outputscale=\\lilypondpaperoutputscale pt\\turnOnPostScript"))

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
     (ly-number->string x) " \\outputscale "))

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
    (if point-and-click
     (string-append "\\special{src:" (number->string line) ":"
        (number->string col) " " file "}"
	 ;; arg, the clueless take over the mailing list...
;	 "\\special{-****-These-warnings-are-harmless-***}"
;	 "\\special{-****-PLEASE-read-http://appel.lilypond.org/wiki/index.php3?PostProcessing-****}"
	)
     "")

     ; line numbers only:
    ;(string-append "\\special{src:" (number->string line) " " file "}")
)

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
    

  (define (select-font name-mag-pair)
    (let*
	(
	 (c (assoc name-mag-pair font-name-alist))
	 )

      (if (eq? c #f)
	  (begin
	    (display name-mag-pair)
	    (display font-name-alist)
	    (ly-warn (string-append
		      "Programming error: No such font known " (car name-mag-pair))
		     (number->string (cdr name-mag-pair))
		     )
	    
	    "")				; issue no command	  
	  (string-append " " (cdr c) " "))
      ))

    (define (font-load-command name-mag command)
      (string-append
       "/" command
       " { /"
       (symbol->string (car name-mag))
       " findfont "
       (number->string (cdr name-mag))
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

     (if (string=? (substring key 0 (min (string-length "lilypondpaper") (string-length key))) "lilypondpaper")
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

; ugh: naming.
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

(define (gulp-file name)
  (let* ((file (open-input-file name))
	 (text (read-delimited "" file)))
    (close file)
    text))

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



;;
;; (name . (glyph clef-position octavation))
;; -- the name clefOctavation is misleading the value 7 is 1 octave not 7 Octaves.
;;
(define supported-clefs '(
	  ("treble" . ("clefs-G" -2 0))
	  ("violin" . ("clefs-G" -2 0))
	  ("G" . ("clefs-G" -2 0))
	  ("G2" . ("clefs-G" -2 0))
	  ("french" . ("clefs-G" -4  0))
	  ("soprano" . ("clefs-C" -4  0))
	  ("mezzosoprano" . ("clefs-C" -2  0))
	  ("alto" . ("clefs-C" 0 0))
	  ("tenor" . ("clefs-C" 2 0))
	  ("baritone" . ("clefs-C" 4  0))
	  ("varbaritone"  . ("clefs-F" 0 0))
	  ("bass" . ("clefs-F" 2  0))
	  ("F" . ( "clefs-F" 2 0))
	  ("subbass" . ("clefs-F" 4 0))

	  ;; should move mensural stuff to separate file? 
	  ("vaticana_do1" . ("clefs-vaticana_do" -1 0))
	  ("vaticana_do2" . ("clefs-vaticana_do" 1 0))
	  ("vaticana_do3" . ("clefs-vaticana_do" 3 0))
	  ("vaticana_fa1" . ("clefs-vaticana_fa" -1 0))
	  ("vaticana_fa2" . ("clefs-vaticana_fa" 1 0))
	  ("medicaea_do1" . ("clefs-medicaea_do" -1 0))
	  ("medicaea_do2" . ("clefs-medicaea_do" 1 0))
	  ("medicaea_do3" . ("clefs-medicaea_do" 3 0))
	  ("medicaea_fa1" . ("clefs-medicaea_fa" -1 0))
	  ("medicaea_fa2" . ("clefs-medicaea_fa" 1 0))
	  ("hufnagel_do1" . ("clefs-hufnagel_do" -1 0))
	  ("hufnagel_do2" . ("clefs-hufnagel_do" 1 0))
	  ("hufnagel_do3" . ("clefs-hufnagel_do" 3 0))
	  ("hufnagel_fa1" . ("clefs-hufnagel_fa" -1 0))
	  ("hufnagel_fa2" . ("clefs-hufnagel_fa" 1 0))
	  ("hufnagel" . ("clefs-hufnagel_do_fa" 4 0))
	  ("mensural1_c1" . ("clefs-mensural1_c" -4 0))
	  ("mensural1_c2" . ("clefs-mensural1_c" -2 0))
	  ("mensural1_c3" . ("clefs-mensural1_c" 0 0))
	  ("mensural1_c4" . ("clefs-mensural1_c" 2 0))
	  ("mensural2_c1" . ("clefs-mensural2_c" -4 0))
	  ("mensural2_c2" . ("clefs-mensural2_c" -2 0))
	  ("mensural2_c3" . ("clefs-mensural2_c" 0 0))
	  ("mensural2_c4" . ("clefs-mensural2_c" 2 0))
	  ("mensural2_c5" . ("clefs-mensural2_c" 4 0))
	  ("mensural3_c1" . ("clefs-mensural3_c" -2 0))
	  ("mensural3_c2" . ("clefs-mensural3_c" 0 0))
	  ("mensural3_c3" . ("clefs-mensural3_c" 2 0))
	  ("mensural3_c4" . ("clefs-mensural3_c" 4 0))
	  ("mensural_f" . ("clefs-mensural_f" 2 0))
	)
)

(define (clef-name-to-properties cl)
  (let ((e '())
	(oct 0)
	(l (string-length cl))
	)

    ;; ugh. cleanme
    (if (equal? "8" (substring cl (- l 1) l))
	(begin
	(if (equal? "^" (substring cl (- l 2) (- l 1)))
	    (set! oct 7)
	    (set! oct -7))
	
	(set! cl (substring cl 0 (- l 2)))))


    (set! e  (assoc cl supported-clefs))
    (if (pair? e)
	`(((symbol . clefGlyph)
	   (iterator-ctor . ,Property_iterator::constructor)
	   (value . ,(cadr e))
	   )
	  ((symbol . clefPosition)
	   (iterator-ctor . ,Property_iterator::constructor)
	   (value . ,(caddr e))
	   )
	  ,(if (not (equal? oct 0))
	       `((symbol . clefOctavation)
		 (iterator-ctor . ,Property_iterator::constructor)
		 (value . ,oct)
	       ))
	  )
	(begin
	  (ly-warn (string-append "Unknown clef type `" cl "'\nSee scm/lily.scm for supported clefs"))
	  '())
    )))



(define (repeat-name-to-ctor name)
  (let*
      ((supported-reps
	`(("volta" . ((iterator-ctor . ,Volta_repeat_iterator::constructor)
		      (length . ,Repeated_music::volta_music_length)
		      ))
	  ("unfold" . ((iterator-ctor . ,Unfolded_repeat_iterator::constructor)
		       (length . ,Repeated_music::unfolded_music_length)
		       ))
	  ("fold" . ((iterator-ctor  . ,Folded_repeat_iterator::constructor)
		      (length . ,Repeated_music::folded_music_length)
		      ))
	  ("tremolo" . ((iterator-ctor . ,Chord_tremolo_iterator::constructor)
			(length . ,Repeated_music::unfolded_music_length)
			))
	  ))
	  
       (handle (assoc name supported-reps))
       )

    (if (pair? handle)
	(cdr handle)
	(begin
	  (ly-warn
	   (string-append "Unknown repeat type `" name "'\nSee scm/lily.scm for supported repeats")
	   )
	  '(type . 'repeated-music))
	)
  ))


(map (lambda (x)   (eval-string (ly-gulp-file x)))
     
   '("backend-property.scm"
 "translator-properties.scm"
 "interface.scm"
 "beam.scm"
 "slur.scm"
 "font.scm"
 "auto-beam.scm"
 "generic-property.scm"
 "basic-properties.scm"
 "chord-name.scm"
 "element-descriptions.scm"
 ))
