;;; ps.scm -- implement Scheme output routines for PostScript
;;;
;;;  source file of the GNU LilyPond music typesetter
;;; 
;;; (c) 1998--2001 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Han-Wen Nienhuys <hanwen@cs.uu.nl>



(define-module (scm ps)
  )

(define this-module (current-module))

(debug-enable 'backtrace)

(if (or (equal? (minor-version) "4")
	(equal? (minor-version) "3.4"))
    (define-public (ps-output-expression expr port)
      (display (eval-in-module expr this-module) port))

    (define-public (ps-output-expression expr port)
      (display (eval expr this-module) port)))

 
(use-modules
 (guile)
)



;;;;;;;;
;;;;;;;; DOCUMENT ME!
;;;;;;;; 
(define (tex-encoded-fontswitch name-mag)
  (let* ((iname-mag (car name-mag))
	 (ename-mag (cdr name-mag)))
    (cons iname-mag
	  (cons ename-mag
		(string-append  "magfont"
			  (string-encode-integer
			   (hashq (car ename-mag) 1000000))
			  "m"
			  (string-encode-integer
			   (inexact->exact (* 1000 (cdr ename-mag)))))))))

(define (fontify name-mag-pair exp)
  (string-append (select-font name-mag-pair)
		 exp))


(define (define-fonts internal-external-name-mag-pairs)
  (set! font-name-alist (map tex-encoded-fontswitch
			     internal-external-name-mag-pairs))
  (apply string-append
	 (map (lambda (x)
		(font-load-command (car x) (cdr x)))
	      (map cdr font-name-alist))))



;; alist containing fontname -> fontcommand assoc (both strings)
(define font-alist '())
(define font-count 0)
(define current-font "")

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
		    (ly-number->string (cdr name-mag-pair))
		    ))
	  
	  "") ; issue no command	  
	(string-append " " (cddr c) " "))
    ))

(define (font-load-command name-mag command)
  (string-append
   "/" command
   " { /"
   (capitalize-font-name (car name-mag))
   " findfont "
   "20 " (ly-number->string (cdr name-mag)) " mul "
   "output-scale div scalefont setfont } bind def "
   "\n"))

;; Ugh, the Bluesky type1 fonts for computer modern use capitalized 
;; postscript font names.
(define (capitalize-font-name name)
  (if (equal? (substring name 0 2) "cm")
      (string-upcase name)
      name))

(define (beam width slope thick)
  (string-append
   (numbers->string (list slope width thick)) " draw_beam" ))

(define (comment s)
  (string-append "% " s "\n"))

(define (bracket arch_angle arch_width arch_height  height arch_thick thick)
  (string-append
   (numbers->string (list arch_angle arch_width arch_height height arch_thick thick)) " draw_bracket" ))

(define (char i)
  (invoke-char " show" i))



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

(define (draw-line thick x1 y1 x2 y2)

  (string-append 
  "	1 setlinecap
	1 setlinejoin "
  (ly-number->string thick)
	" setlinewidth "
   (ly-number->string x1)
   " "
   (ly-number->string y1)
   " moveto "
   (ly-number->string x2)
   " "
   (ly-number->string y2)
   " lineto stroke"

  ))

(define (repeat-slash wid slope thick)
  (string-append (numbers->string (list wid slope thick))
		 " draw_repeat_slash"))

(define (end-output)
  "\nend-lilypond-output\n")

(define (experimental-on) "")

(define (filledbox breapth width depth height) 
  (string-append (numbers->string (list breapth width depth height))
		 " draw_box" ))

(define (roundfilledbox x width y height blotdiam)
   (string-append " "
      (numbers->string
         (list x width y height blotdiam)) " draw_round_box"))

(define (dot x y radius)
    (string-append " "
     (numbers->string
      (list x y radius)) " draw_dot"))

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
   (ly-gulp-file "music-drawing-routines.ps")
   "{ exch pop //systemdict /run get exec } "
;; ps-testing is broken: global module
   (if (defined? 'ps-testing) "\n /testing true def" "")
;;   "\n /testing true def"
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


(define (placebox x y s) 
  (string-append 
   (ly-number->string x) " " (ly-number->string y) " {" s "} place-box\n"))

(define (bezier-sandwich l thick)
  (string-append 
   (apply string-append (map control->string l))
   (ly-number->string thick)
   " draw_bezier_sandwich "
   (bezier-ending (list-ref l 3) (list-ref l 0) (list-ref l 5))
   (bezier-ending (list-ref l 7) (list-ref l 0) (list-ref l 5))))

(define (bezier-ending z0 z1 z2)
  (let ((x0 (car z0))
	(y0 (cdr z0))
	(x1 (car z1))
	(y1 (cdr z1))
	(x2 (car z2))
	(y2 (cdr z2)))
    (string-append " "
     (numbers->string
      (list x0 y0
	    (/ (sqrt (+ (* (- x1 x2) (- x1 x2)) (* (- y1 y2) (- y1 y2)))) 2)))
     " draw_dot")))

					; TODO: use HEIGHT argument

  (define (start-system height)
  (string-append
   "\n"
   (ly-number->string height)
   " start-system {
set-ps-scale-to-lily-scale

"))

(define (stem breapth width depth height) 
  (string-append (numbers->string (list breapth width depth height))
		 " draw_box" ))

(define (stop-system)
  "}\nstop-system\n")

(define (stop-last-system)
  "}\nstop-system\n")

(define (text s)
  (string-append "(" s ") show  "))


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
  
  
