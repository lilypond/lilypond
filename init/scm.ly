% scm.ly -- implement Scheme output routines for TeX and PostScript
%
%  source file of the GNU LilyPond music typesetter
% 
% (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>

\scm "

;;; graphical lisp element
(define (add-column p) (display \"adding column (in guile): \") (display p) (newline))

;;; library funtions
(define
  (numbers->string l)
  (apply string-append 
  (map (lambda (n) (string-append (number->string n) \" \")) l)))

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
  (number->dim-tex x)
  (string-append 
   (number->string x) \"pt \"))

(define
  (control->string c)
  (string-append
    (string-append (number->string (car c)) \" \")
    (string-append (number->string (cadr c)) \" \")))

(define
  (invoke-output o s)
   (eval-string (string-append s \"-\" o)))

;;; output definitions
(define 
  (char o n) 
  ((invoke-output o \"char\") n))

(define 
  (char-ps n) 
  (string-append 
   \"(\\\\\" (inexact->string n 8) \") show\"))

(define 
  (char-tex n) 
  (string-append 
   \"\\\\char\" (inexact->string n 10)))

(define 
  (dashed-slur o thick dash l) 
  ((invoke-output o \"dashed-slur\") thick dash l))

(define 
  (dashed-slur-ps thick dash l)
  (string-append 
    (apply string-append (map control->string l)) 
    (number->string thick) 
   \" [ \"
   (if (> 1 dash) (number->string (- (* thick dash) thick)) \"0\") \" \"
   (number->string (* 2 thick))
   \" ] 0 draw_dashed_slur\"))

(define 
  (dashed-slur-tex thick dash l)
  (string-append 
    \"\\\\embeddedps{\"
    (dashed-slur-ps thick dash l)
   \"}\"))

(define 
  (empty o) 
  ((invoke-output o \"empty\")))

(define 
  (empty-ps) 
  \"\n empty\n\")

(define 
  (empty-tex) 
  \"%\n\\\\empty%\n\")

(define 
  (end-output o) 
  ((invoke-output o \"end-output\")))

(define 
  (end-output-ps)
  \"\nshowpage\n\")

(define 
  (end-output-tex) 
  \"\n\\\\EndLilyPondOutput\")

(define 
  (experimental-on o) 
  ((invoke-output o \"experimental-on\")))

(define
  (experimental-on-ps) \"\")

(define
  (experimental-on-tex) \"\\\\turnOnExperimentalFeatures\")

(define
  (finishbar o h) (empty o))

(define
  (font i)
  (string-append
   \"font\"
   (make-string 1 (integer->char (+ (char->integer #\\A) i)))
   ))

(define 
  (font-def o i s) 
  (empty o))
;  ((invoke-output o \"font-def\") i s))

(define 
  (font-def-ps i s)
  (string-append
   \"\n/\" (font i) \" {/\" 
   (substring s 0 (- (string-length s) 3))
   \" findfont 12 scalefont setfont} bind def\n\"))

(define 
  (font-def-tex i s)
  (string-append
   \"\\\\font\" (font-switch-tex i) \"=\" s \"\n\"))

(define 
  (font-switch o i) 
  ((invoke-output o \"font-switch\") i))

(define 
  (font-switch-ps i)
  (string-append (font i) \" \"))

(define 
  (font-switch-tex i)
  (string-append
   \"\\\\\" (font i) \"\n\"))

(define 
  (generalmeter o num den)
   ((invoke-output o \"generalmeter\") num den))

(define 
  (generalmeter-ps num den)
  (string-append num \" \" den \" generalmeter \"))

(define 
  (generalmeter-tex num den)
  (string-append 
   \"\\\\generalmeter{\" num \"}{\" den \"}\"))

(define 
  (header o creator generate) 
  ((invoke-output o \"header\") creator generate))

(define 
  (header-ps creator generate) 
  (string-append
   \"%!PS-Adobe-3.0\n\"
   \"%%Creator: \" creator generate \"\n\"))

(define 
  (header-tex creator generate) 
  (string-append
   \"%created by: \" creator generate \"\n\"))

(define 
  (header-end o) 
  ((invoke-output o \"header-end\")))

(define
  (header-end-ps) \"\")

(define
  (header-end-tex) \"\\\\turnOnPostScript\")

(define
  (lily-def o key val)
  ((invoke-output o \"lily-def\") key val))

(define
  (lily-def-ps key val)
  (string-append
   \"/\" key \" {\" val \"} bind def\n\"))

(define
  (lily-def-tex key val)
  (string-append
   \"\\\\def\\\\\" key \"{\" val \"}\n\"))

(define 
  (maatstreep o h) 
  ((invoke-output o \"maatstreep\") h))

(define 
  (maatstreep-ps h)
  (string-append
   (number->string h) \" maatstreep \" ))

(define 
  (maatstreep-tex h)
  (string-append
   \"\n\\\\maatstreep{\" (number->dim-tex h) \"}\"))

(define 
  (pianobrace o h) (empty o))

(define 
  (placebox o x y b) 
  ((invoke-output o \"placebox\") x y (b o)))

(define 
  (placebox-ps x y s) 
  (string-append 
   (number->string x) \" \" (number->string y) \" {\" s \"} placebox \"))

(define 
  (placebox-tex x y s) 
  (string-append 
   \"\\\\placebox{\"
   (number->dim-tex y) \"}{\" (number->dim-tex x) \"}{\" s \"}\"))

(define
  (repeatbar o h) (empty o))

(define 
  (rulesym o x y) 
  ((invoke-output o \"rulesym\") x y))

(define 
  (rulesym-ps x y) 
  (string-append 
   (number->string x) \" \"
   (number->string y) \" \"
   \"rulesym\"))

(define 
  (rulesym-tex x y) 
  (string-append 
   \"\\\\rulesym{\" (number->dim-tex x) \"}{\" (number->dim-tex y) \"}\"))

(define 
  (setitalic o s) (empty o))

(define 
  (settext o s) (empty o))

(define 
  (slur o l) 
  ((invoke-output o \"slur\") l))

(define 
  (slur-ps l)
  (string-append 
   (apply string-append (map control->string l)) 
   \" draw_slur\"))

(define 
  (slur-tex l)
  (string-append 
   \"\\\\embeddedps{\"
   (slur-ps l)
   \"}\"))

(define 
  (stem o kern width height depth) 
  ((invoke-output o \"stem\") kern width height depth))

(define 
  (stem-ps kern width height depth) 
  (string-append (numbers->string (list kern width height depth))
		 \"draw_stem\" ))

(define 
  (stem-tex kern width height depth) 
  (string-append 
   \"\\\\kern\" (number->dim-tex kern)
   \"\\\\vrule width \" (number->dim-tex width)
   \"depth \" (number->dim-tex depth)
   \"height \" (number->dim-tex height) \" \"))

(define 
  (start-line o) 
  ((invoke-output o \"start-line\")))

(define 
  (start-line-ps) 
  (string-append
   (urg-fix-font-ps)
   \"\nstart_line {\n\"))

(define 
  (start-line-tex) 
  (string-append 
   (urg-fix-font-tex)
   \"\\\\hbox{%\n\"))

(define 
  (stop-line o) 
  ((invoke-output o \"stop-line\")))

(define 
  (stop-line-ps) 
  \"}\nstop_line\n\")

(define 
  (stop-line-tex) 
  \"}\\\\interscoreline\")

(define
  (urg-fix-font-ps)
  \"/fontA { /feta20 findfont 12 scalefont setfont} bind def fontA\n\")

(define
  (urg-fix-font-tex)
  \"\\\\font\\\\fontA=feta20.afm\\\\fontA\n\")

(define 
  (urg-font-switch-ps i)
  \"\n/feta20 findfont 12 scalefont setfont \n\")

";


