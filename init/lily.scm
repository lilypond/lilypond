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

(define  
  (beam-tex width slope thick)
  (embedded-ps-tex (beam-ps width slope thick)))

(define 
  (bracket-tex h)
  (embedded-ps-tex (bracket-ps h)))

(define 
  (dashed-slur-tex thick dash l)
  (embedded-ps-tex (dashed-slur-ps thick dash l)))

(define 
  (crescendo-tex w h cont)
  (embedded-ps-tex (crescendo-ps w h cont)))

(define 
  (decrescendo-tex w h cont)
  (embedded-ps-tex (decrescendo-ps w h cont)))

(define 
  (embedded-ps-tex s)
  (string-append "\\embeddedps{" s "}"))


(define 
  (end-output-tex) 
  "\n\\EndLilyPondOutput")

(define 
  (empty-tex) 
  "%\n\\empty%\n")

(define
  (experimental-on-tex) "\\turnOnExperimentalFeatures")

(define 
  (extender o h)
  ((invoke-output o "invoke-dim1") "extender" h))

(define 
  (font-switch-tex i)
  (string-append
   "\\" (font i) "\n"))

(define 
  (font-def-tex i s)
  (string-append
   "\\font" (font-switch-tex i) "=" s "\n"))

(define 
  (generalmeter-tex num den)
  (string-append 
   "\\generalmeter{" (number->string (inexact->exact num)) "}{" (number->string (inexact->exact den)) "}"))

(define
  (header-end-tex) "\\turnOnPostScript")

(define 
  (header-tex creator generate) 
  (string-append
   "%created by: " creator generate "\n"))

(define 
  (invoke-char-tex s i)
  (string-append 
   "\n\\" s "{" (inexact->string i 10) "}" ))

(define 
  (invoke-dim1-tex s d)
  (string-append
   "\n\\" s "{" (number->dim-tex d) "}"))

(define
  (lily-def-tex key val)
  (string-append
   "\\def\\" key "{" val "}\n"))

(define 
  (number->dim-tex x)
  (string-append 
   (number->string (chop-decimal x)) "pt "))

(define 
  (placebox-tex x y s) 
  (string-append 
   "\\placebox{"
   (number->dim-tex y) "}{" (number->dim-tex x) "}{" s "}"))

(define 
  (rulesym-tex h w) 
  (string-append 
   "\\vrule height " (number->dim-tex (/ h 2))
   " depth " (number->dim-tex (/ h 2))
   " width " (number->dim-tex w)
   )
  )

(define 
  (slur-tex l)
  (embedded-ps-tex (slur-ps l)))

(define 
  (start-line-tex) 
  (string-append 
   "\\hbox{%\n")
  )

(define 
  (stem-tex kern width height depth) 
  (string-append 
   "\\kern" (number->dim-tex kern)
   "\\vrule width " (number->dim-tex width)
   "depth " (number->dim-tex depth)
   "height " (number->dim-tex height) " "))

(define 
  (stop-line-tex) 
  "}\\interscoreline")

(define
  (text-tex f s)
  (string-append "\\set" f "{" s "}"))

(define 
  (tuplet-tex dx dy dir)
  (embedded-ps-tex (tuplet-ps dx dy dir)))


(define 
  (volta-tex w last)
  (embedded-ps-tex (volta-ps w last)))

;;;;;;;;;;;; PS

(define 
  (beam-ps width slope thick)
  (string-append
   (numbers->string (list width slope thick)) " draw_beam " ))

(define 
  (bracket-ps h)
  (invoke-dim1-ps "draw_bracket" h))

(define 
  (crescendo-ps w h cont)
  (string-append 
   (numbers->string (list w h (inexact->exact cont)))
   "draw_crescendo"))

(define 
  (dashed-slur-ps thick dash l)
  (string-append 
    (apply string-append (map control->string l)) 
    (number->string thick) 
   " [ "
   (if (> 1 dash) (number->string (- (* thick dash) thick)) "0") " "
   (number->string (* 2 thick))
   " ] 0 draw_dashed_slur"))

(define 
  (decrescendo-ps w h cont)
  (string-append 
   (numbers->string (list w h (inexact->exact cont)))
   "draw_decrescendo"))

(define 
  (empty-ps) 
  "\n empty\n")

(define 
  (end-output-ps)
  "\nshowpage\n")

(define
  (experimental-on-ps) "")

(define 
  (font-def-ps i s)
  (string-append
   "\n/" (font i) " {/" 
   (substring s 0 (- (string-length s) 4))
   " findfont 12 scalefont setfont} bind def\n"))

(define 
  (font-switch-ps i)
  (string-append (font i) " "))

(define 
  (generalmeter-ps num den)
  (string-append (number->string (inexact->exact num)) " " (number->string (inexact->exact den)) " generalmeter "))

(define
  (header-end-ps) "")
(define
  (lily-def-ps key val)
  (string-append
   "/" key " {" val "} bind def\n"))

(define 
  (header-ps creator generate) 
  (string-append
   "%!PS-Adobe-3.0\n"
   "%%Creator: " creator generate "\n"))

(define 
  (invoke-char-ps s i)
  (string-append 
   "(\\" (inexact->string i 8) ") " s " " ))

(define 
  (invoke-dim1-ps s d) 
  (string-append
   (number->string d) " " s ))

(define 
  (placebox-ps x y s) 
  (string-append 
   (number->string x) " " (number->string y) " {" s "} placebox "))

(define 
  (rulesym-ps x y) 
  (string-append 
   (number->string x) " "
   (number->string y) " "
   "rulesym"))

(define 
  (slur-ps l)
  (string-append 
   (apply string-append (map control->string l)) 
   " draw_slur"))

(define 
  (start-line-ps) 
   "\nstart_line {\n")

(define 
  (stem-ps kern width height depth) 
  (string-append (numbers->string (list kern width height depth))
		 "draw_stem" ))

(define 
  (stop-line-ps) 
  "}\nstop_line\n")

(define
  (text-ps f s)
  (string-append "(" s ") set" f " "))


(define 
  (volta-ps w last)
  (string-append 
   (numbers->string (list w (inexact->exact last)))
   "draw_volta"))

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
  (tuplet-ps dx dy dir)
  (string-append 
   (numbers->string (list dx dy (inexact->exact dir)))
   "draw_tuplet"))

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
  (stop-line o) 
  ((invoke-output o "stop-line")))

(define
  (stoprepeat o h)
  ((invoke-output o "invoke-dim1") "stoprepeat" h))

(define 
  (volta o w last)
  ((invoke-output o "volta") w last))

