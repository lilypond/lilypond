; lily.scm -- implement Scheme output routines for TeX and PostScript
;
;  source file of the GNU LilyPond music typesetter
; 
; (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>

; TODO
;   - naming
;   - ready ps code (draw_bracket) vs tex/ps macros/calls (pianobrace),
;     all preparations from ps,tex to scm

;;; library funtions
(define
  (xnumbers->string l)
  (string-append 
   (map (lambda (n) (string-append (number->string n ) " ")) l)))

(define
  (numbers->string l)
  (apply string-append 
  (map (lambda (n) (string-append (number->string n) " ")) l)))

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
    (string-append (number->string (cadr c)) " ")))

;;;;;;;; TeX
;(define (tex action)

(define (beam-tex width slope thick)
  (embedded-ps-tex ((ps-scm 'beam) width slope thick)))

(define (bracket-tex h)
  (embedded-ps-tex ((ps-scm 'bracket) h)))

(define (dashed-slur-tex thick dash l)
  (embedded-ps-tex ((ps-scm 'dashed-slur)  thick dash l)))

(define (crescendo-tex w h cont)
  (embedded-ps-tex ((ps-scm 'crescendo) w h cont)))

(define (decrescendo-tex w h cont)
  (embedded-ps-tex ((ps-scm 'decrescendo) w h cont)))

(define (embedded-ps-tex s)
  (string-append "\\embeddedps{" s "}"))


(define (end-output-tex) 
  "\n\\EndLilyPondOutput")

(define (empty-tex) 
  "%\n\\empty%\n")

(define (experimental-on-tex) "\\turnOnExperimentalFeatures")

(define (extender o h)
  ((invoke-output o "invoke-dim1") "extender" h))

(define (font-switch-tex i)
  (string-append
   "\\" (font i) "\n"))

(define (font-def-tex i s)
  (string-append
   "\\font" (font-switch-tex i) "=" s "\n"))

(define (generalmeter-tex num den)
  (string-append 
   "\\generalmeter{" (number->string (inexact->exact num)) "}{" (number->string (inexact->exact den)) "}"))

(define (header-end-tex) "\\turnOnPostScript")

(define (header-tex creator generate) 
  (string-append
   "%created by: " creator generate "\n"))

(define (invoke-char-tex s i)
  (string-append 
   "\n\\" s "{" (inexact->string i 10) "}" ))

(define (invoke-dim1-tex s d)
  (string-append
   "\n\\" s "{" (number->dim-tex d) "}"))

(define (lily-def-tex key val)
  (string-append
   "\\def\\" key "{" val "}\n"))

(define (number->dim-tex x)
  (string-append 
   (number->string (chop-decimal x)) "pt "))

(define (placebox-tex x y s) 
  (string-append 
   "\\placebox{"
   (number->dim-tex y) "}{" (number->dim-tex x) "}{" s "}"))

(define (rulesym-tex h w) 
  (string-append 
   "\\vrule height " (number->dim-tex (/ h 2))
   " depth " (number->dim-tex (/ h 2))
   " width " (number->dim-tex w)
   )
  )

(define (slur-tex l)
  (embedded-ps-tex ((ps-scm 'slur) l)))

(define (start-line-tex) 
  (string-append 
   "\\hbox{%\n")
  )

(define (stem-tex kern width height depth) 
  (string-append 
   "\\kern" (number->dim-tex kern)
   "\\vrule width " (number->dim-tex width)
   "depth " (number->dim-tex depth)
   "height " (number->dim-tex height) " "))

(define (stop-line-tex) 
  "}\\interscoreline")

(define (text-tex f s)
  (string-append "\\set" f "{" s "}"))

(define (tuplet-tex dx dy dir)
  (embedded-ps-tex ((ps-scm 'tuplet) dx dy dir)))

(define (volta-tex w last)
  (embedded-ps-tex ((ps-scm 'volta)  w last)))

;;;;;;;;;;;; PS
(define (ps-scm action-name)
  (define (beam width slope thick)
    (string-append
     (numbers->string (list width slope thick)) " draw_beam " ))

  (define (bracket h)
    (invoke-dim1 "draw_bracket" h))

  (define (crescendo w h cont)
    (string-append 
     (numbers->string (list w h (inexact->exact cont)))
     "draw_crescendo"))

  (define (dashed-slur thick dash l)
    (string-append 
     (apply string-append (map control->string l)) 
     (number->string thick) 
     " [ "
     (if (> 1 dash) (number->string (- (* thick dash) thick)) "0") " "
     (number->string (* 2 thick))
     " ] 0 draw_dashed_slur"))

  (define (decrescendo w h cont)
    (string-append 
     (numbers->string (list w h (inexact->exact cont)))
     "draw_decrescendo"))

  (define (empty) 
    "\n empty\n")

  (define (end-output)
    "\nshowpage\n")

  (define (experimental-on) "")

  (define (font-def i s)
    (string-append
     "\n/" (font i) " {/" 
     (substring s 0 (- (string-length s) 4))
     " findfont 12 scalefont setfont} bind def\n"))

  (define (font-switch i)
    (string-append (font i) " "))

  (define (generalmeter num den)
    (string-append (number->string (inexact->exact num)) " " (number->string (inexact->exact den)) " generalmeter "))

  (define (header-end) "")
  (define (lily-def key val)
    (string-append
     "/" key " {" val "} bind def\n"))

  (define (header creator generate) 
    (string-append
     "%!PS-Adobe-3.0\n"
     "%%Creator: " creator generate "\n"))

  (define (invoke-char s i)
    (string-append 
     "(\\" (inexact->string i 8) ") " s " " ))

  (define (invoke-dim1 s d) 
    (string-append
     (number->string d) " " s ))

  (define (placebox x y s) 
    (string-append 
     (number->string x) " " (number->string y) " {" s "} placebox "))

  (define (rulesym x y) 
    (string-append 
     (number->string x) " "
     (number->string y) " "
     "rulesym"))

  (define (slur l)
    (string-append 
     (apply string-append (map control->string l)) 
     " draw_slur"))

  (define (start-line) 
    "\nstart_line {\n")

  (define (stem kern width height depth) 
    (string-append (numbers->string (list kern width height depth))
		   "draw_stem" ))

  (define (stop-line) 
    "}\nstop_line\n")

  (define (text f s)
    (string-append "(" s ") set" f " "))

(define 
  (unknown-tex) 
  "%\n\\unknown%\n")

  (define (volta w last)
    (string-append 
     (numbers->string (list w (inexact->exact last)))
     "draw_volta"))
  (define   (tuplet dx dy dir)
    (string-append 
     (numbers->string (list dx dy (inexact->exact dir)))
     "draw_tuplet"))



  ; dispatch on action-name
  (cond ((eq? action-name 'all-definitions)
	`(eval
	  (define beam ,beam)
	  (define tuplet ,tuplet)
	  (define bracket ,bracket)
	  (define crescendo ,crescendo)
	  (define volta ,volta)
	  (define slur ,slur)
	  (define dashed-slur ,dashed-slur) 
	  (define decrescendo ,decrescendo) 
	  (define empty ,empty)
	  (define end-output ,end-output)
	  (define font-def ,font-def)
	  (define font-switch ,font-switch)
	  (define generalmeter ,generalmeter)
	  (define header-end ,header-end)
	  (define lily-def ,lily-def)
	  (define header ,header) 
	  (define invoke-char ,invoke-char) 
	  (define invoke-dim1 ,invoke-dim1)
	  (define placebox ,placebox)
	  (define rulesym ,rulesym)
	  (define start-line ,start-line)
	  (define stem ,stem)
	  (define stop-line ,stop-line)
	  (define text ,text)
	  ))
	((eq? action-name 'tuplet) tuplet)
	((eq? action-name 'beam) beam)
	((eq? action-name 'bracket) bracket)
	((eq? action-name 'crescendo) crescendo)
	((eq? action-name 'volta) volta)
	((eq? action-name 'slur) slur)
	((eq? action-name 'dashed-slur) dashed-slur) 
	((eq? action-name 'decrescendo) decrescendo)
	(else (error "unknown tag -- PS-SCM " action-name))
	)
)


(define 
  (unknown-ps) 
  "\n unknown\n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; output definitions

(define 
  (beam o width slope thick) 
  ((invoke-output o "beam") width slope thick))

(define 
  (bracket o h)
  ((invoke-output o "bracket") h))

(define 
  (char o n) 
  ((invoke-output o "invoke-char") "show" n))

(define 
  (crescendo o w h cont)
  ((invoke-output o "crescendo") w h cont))

(define 
  (dashed-slur o thick dash l) 
  ((invoke-output o "dashed-slur") thick dash l))

(define 
  (decrescendo o w h cont)
  ((invoke-output o "decrescendo") w h cont))

(define 
  (doublebar o h)
  ((invoke-output o "invoke-dim1") "doublebar" h))

(define 
  (empty o) 
  ((invoke-output o "empty")))

(define 
  (emptybar o h) (empty o))

(define 
  (end-output o) 
  ((invoke-output o "end-output")))

(define 
  (experimental-on o) 
  ((invoke-output o "experimental-on")))

(define
  (fatdoublebar o h)
  ((invoke-output o "invoke-dim1") "fatdoublebar" h))

(define
  (finishbar o h)
  ((invoke-output o "invoke-dim1") "finishbar" h))

(define
  (font i)
  (string-append
   "font"
   (make-string 1 (integer->char (+ (char->integer #\A) i)))
   ))

(define 
  (font-def o i s) 
  ((invoke-output o "font-def") i s))

(define 
  (font-switch o i) 
  ((invoke-output o "font-switch") i))

(define 
  (generalmeter o num den)
   ((invoke-output o "generalmeter") num den))

(define 
  (header o creator generate) 
  ((invoke-output o "header") creator generate))

(define 
  (header-end o) 
  ((invoke-output o "header-end")))

(define
  (invoke-output o s)
   (eval-string (string-append s "-" o)))

(define
  (lily-def o key val)
  ((invoke-output o "lily-def") key val))

(define 
  (maatstreep o h) 
  ((invoke-output o "invoke-dim1") "maatstreep" h))

(define 
  (pianobrace o i)
  ((invoke-output o "invoke-char") "pianobrace" i))

(define 
  (placebox o x y b) 
  ((invoke-output o "placebox") x y (b o)))

(define
  (repeatbar o h)
  ((invoke-output o "invoke-dim1") "repeatbar" h))

(define
  (repeatbarstartrepeat o h)
  ((invoke-output o "invoke-dim1") "repeatbarstartrepeat" h))

(define 
  (rulesym o x y) 
  ((invoke-output o "rulesym") x y))

(define 
  (setbold o s) 
  ((invoke-output o "text") "bold" s))

(define
  (setdynamic o s) (empty o))

(define 
  (setfinger o s) 
  ((invoke-output o "text") "finger" s))

(define 
  (sethuge o s) 
  ((invoke-output o "text") "huge" s))

(define 
  (setitalic o s) 
  ((invoke-output o "text") "italic" s))

(define 
  (setlarge o s) 
  ((invoke-output o "text") "large" s))

(define 
  (setLarge o s) 
  ((invoke-output o "text") "Large" s))

(define 
  (setnumber o s) 
  ((invoke-output o "text") "number" s))

; urg, howto do all these sizes;
; what about: fontjj fontj font fonti fontii
(define 
  (setnumber-1 o s) 
  ((invoke-output o "text") "numberj" s))

(define 
  (settext o s) 
  ((invoke-output o "text") "text" s))

(define 
  (settypewriter o s) 
  ((invoke-output o "text") "typewriter" s))

(define 
  (slur o l) 
  ((invoke-output o "slur") l))

(define 
  (tuplet o dx dy dir)
  ((invoke-output o "tuplet") dx dy dir))

(define 
  (stem o kern width height depth) 
  ((invoke-output o "stem") kern width height depth))



(define 
  (start-line o) 
  ((invoke-output o "start-line")))

(define
  (startbar o h)
  ((invoke-output o "invoke-dim1") "startbar" h))

(define
  (startrepeat o h)
  ((invoke-output o "invoke-dim1") "startrepeat" h))

(define 
  (stem o kern width height depth) 
  ((invoke-output o "stem") kern width height depth))

(define 
  (stop-line o) 
  ((invoke-output o "stop-line")))

(define
  (stoprepeat o h)
  ((invoke-output o "invoke-dim1") "stoprepeat" h))

(define 
  (tuplet-ps dx dy dir)
  (string-append 
   (numbers->string (list dx dy (inexact->exact dir)))
   "draw_tuplet"))

(define 
  (unknown o) 
  ((invoke-output o "unknown")))

(define 
  (volta o w last)
  ((invoke-output o "volta") w last))

