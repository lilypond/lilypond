;;; pdf.scm -- implement Scheme output routines for PDF.
;;;
;;;  source file of the GNU LilyPond music typesetter
;;; 
;;; (c) 2001 Stephen Peters <portnoy@portnoy.org>

; currently no font commands; this is a helper for pdftex.scm.

(define (pdf-scm action-name)
  ; simple commands to store and update currentpoint.  This makes the
  ; other procedures simple rewrites of the PostScript code.
  (define currentpoint (cons 0 0))
  (define (showcp) 
    (string-append (ly-number->string (car currentpoint)) " " 
		   (ly-number->string (cdr currentpoint)) " "))
  (define (moveto x y)
    (set! currentpoint (cons x y))
    (string-append (showcp) "m "))
  (define (moveto-pair pair)
    (moveto (car pair) (cdr pair)))
  (define (rmoveto x y)
    (moveto (+ x (car currentpoint)) (+ y (cdr currentpoint))))
  (define (lineto x y)
    (set! currentpoint (cons x y))
    (string-append (showcp) "l "))
  (define (lineto-pair pair)
    (lineto (car pair) (cdr pair)))
  (define (rlineto x y)
    (lineto (+ x (car currentpoint)) (+ y (cdr currentpoint))))
  (define (curveto x1 y1 x2 y2 x y)
    (set! currentpoint (cons x y))
    (string-append (ly-number->string x1) (ly-number->string y1)
		   (ly-number->string x2) (ly-number->string y2)
		   (ly-number->string x) (ly-number->string y) "c "))
  (define (curveto-pairs pt1 pt2 pt)
    (curveto (car pt1) (cdr pt1) (car pt2) (cdr pt2) (car pt) (cdr pt)))
  (define (closefill) "h f ")
  (define (closestroke) "S ")
  (define (setlinewidth w) (string-append (ly-number->string w) "w "))
  (define (setgray g) (string-append (ly-number->string g) "g "))
  (define (setlineparams) "1 j 1 J ")
  
  (define (beam width slope thick)
    (let ((ht (* slope width)))
      (string-append (moveto 0 (- (/ thick 2)))
		     (rlineto width ht)
		     (rlineto 0 thick)
		     (lineto 0 (/ thick 2))
		     (closefill))))

  (define (comment s) 
    (string-append "% " s "\n"))

  (define (brack-traject pair ds alpha)
    (let ((alpha-rad (* alpha (/ 3.141592654 180))))
      (cons (+ (car pair) (* (cos alpha-rad) ds))
	    (+ (cdr pair) (* (sin alpha-rad) ds)))))
    
  (define (bracket arch_angle arch_width arch_height height arch_thick thick)
    (let* ((halfht (+ (/ height 2) thick))
	   (farpt (cons (+ thick arch_height) 
			(+ (- halfht arch_thick) arch_width)))
	   (halfbrack 
	    (string-append (moveto 0 0)
			   (lineto thick 0)
			   (lineto thick (- halfht arch_thick))
			   (curveto-pairs
			    (brack-traject (cons thick 
						 (- halfht arch_thick))
					   (* 0.4 arch_height) 0)
			    (brack-traject farpt 
					   (* -0.25 arch_height) 
					   arch_angle)
			    farpt)
			   (curveto-pairs 
			    (brack-traject farpt
					   (* -0.15 arch_height)
					   arch_angle)
			    (brack-traject (cons (/ thick 2) halfht)
					   (/ arch_height 2) 0)
			    (cons 0 halfht))
			   (lineto 0 0)
			   (closefill))))
      (string-append (setlinewidth (/ thick 2))
		     (setlineparams)
		     "q 1 0 0 -1 0 0 cm " ; flip coords
		     halfbrack
		     "Q " ; grestore
		     halfbrack)))
  
  (define (char i)
    (invoke-char " show" i))

  (define (hairpin thick width starth endh )
    (string-append (setlinewidth thick)
		   (moveto 0 starth)
		   (lineto width endh)
		   (moveto 0 (- starth))
		   (lineto width (- endh))
		   (closestroke)))

  (define (dashed-slur thick dash l)
    (string-append (setlineparams)
		   "[ " (ly-number->string dash) " "
		   (ly-number->string (* 10 thick)) " ] 0 d "
		   (setlinewidth thick)
		   (moveto-pair (car l))
		   (apply curveto (cdr l))
		   (closestroke)))
		   
  (define (dashed-line thick on off dx dy)
    (string-append (setlineparams)
		   "[ " (ly-number->string on) " "
		   (ly-number->string off) " ] 0 d "
		   (setlinewidth thick)
		   (moveto 0 0)
		   (lineto dx dy)
		   (closestroke)))

  (define (repeat-slash width slope beamthick)
    (let* ((height (/ beamthick slope))
	   (xwid (sqrt (+ (* beamthick beamthick) (* height height)))))
      (string-append (moveto 0 0)
		     (rlineto xwid 0)
		     (rlineto width (* slope width))
		     (rlineto (- xwid) 0)
		     (closefill))))

  (define (end-output) "")
  
  (define (experimental-on) "")
  
  (define (filledbox breadth width depth height) 
    (string-append (ly-number->string (- breadth))
		   (ly-number->string (- depth))
		   (ly-number->string (+ breadth width))
		   (ly-number->string (+ depth height))
		   " re f "))

  (define (font-def i s) "")

  (define (font-switch i) "")

  (define (header-end) "")
  
  (define (lily-def key val) "")

  (define (header creator generate) "")
  
  (define (invoke-char s i)
    (string-append 
     "(\\" (inexact->string i 8) ") " s " " ))
  
  (define (invoke-dim1 s d) 
    (string-append
     (ly-number->string (* d  (/ 72.27 72))) " " s ))

  (define (placebox x y s) "")

  (define (bezier-sandwich l thick)
    (string-append (setlinewidth thick)
		   (moveto-pair (list-ref l 7))
		   (curveto-pairs (list-ref l 4)
				  (list-ref l 5)
				  (list-ref l 6))
		   (lineto-pair (list-ref l 3))
		   (curveto-pairs (list-ref l 0)
				  (list-ref l 1)
				  (list-ref l 2))
		   "B "))

  (define (start-line height) "")
  
  (define (stem breadth width depth height) 
    (filledbox breadth width depth height))

  (define (stop-line) "")

  (define (text s) "")

  (define (volta h w thick vert_start vert_end)
    (string-append (setlinewidth thick)
		   (setlineparams)
		   (if (= vert_start 0) 
		       (string-append (moveto 0 0)
				      (lineto 0 h))
		       (moveto 0 h))
		   (lineto w h)
		   (if (= vert_end 0) (lineto w 0) "")
		   (closestroke)))

  (define (tuplet ht gap dx dy thick dir)
    (let ((gapy (* (/ dy dx) gap)))
      (string-append (setlinewidth thick)
		     (setlineparams)
		     (moveto 0 (- (* ht dir)))
		     (lineto 0 0)
		     (lineto (/ (- dx gap) 2)
			     (/ (- dy gapy) 2))
		     (moveto (/ (+ dx gap) 2)
			     (/ (+ dy gapy) 2))
		     (lineto dx dy)
		     (lineto dx (- dy (* ht dir)))
		     (closestroke))))

  (define (unknown) "\n unknown\n")

  ; Problem here -- we're using /F18 for the font, but we don't know
  ; for sure that that will exist.
  (define (ez-ball ch letter-col ball-col)
    (let ((origin (cons 0.45 0)))
      (string-append (setgray 0)
		     (setlinewidth 1.1)
		     (moveto-pair origin) (lineto-pair origin)
		     (closestroke)
		     (setgray ball-col)
		     (setlinewidth 0.9)
		     (moveto-pair origin) (lineto-pair origin)
		     (closestroke)
		     (setgray letter-col)
		     (moveto-pair origin)
		     "BT "
		     "/F18 0.85 Tf "
		     "-0.28 -0.30 Td " ; move for text block
		     "[(" ch ")] TJ ET ")))

  (define (define-origin a b c ) "")
  (define (no-origin) "")
  
  ;; PS
  (cond ((eq? action-name 'all-definitions)
	 `(begin
	    (define beam ,beam)
	    (define tuplet ,tuplet)
	    (define bracket ,bracket)
	    (define char ,char)
	    (define volta ,volta)
	    (define bezier-sandwich ,bezier-sandwich)
	    (define dashed-line ,dashed-line) 
	    (define dashed-slur ,dashed-slur) 
	    (define hairpin ,hairpin) 
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
	    (define repeat-slash ,repeat-slash) 
	    (define select-font ,select-font)
	    (define start-line ,start-line)
	    (define stem ,stem)
	    (define stop-line ,stop-line)
	    (define stop-last-line ,stop-line)
	    (define text ,text)
	    (define no-origin ,no-origin)
	    (define define-origin ,define-origin)
	    (define ez-ball ,ez-ball)
	    ))
	((eq? action-name 'tuplet) tuplet)
	((eq? action-name 'beam) beam)
	((eq? action-name 'bezier-sandwich) bezier-sandwich)
	((eq? action-name 'bracket) bracket)
	((eq? action-name 'char) char)
	((eq? action-name 'dashed-line) dashed-line) 
	((eq? action-name 'dashed-slur) dashed-slur) 
	((eq? action-name 'hairpin) hairpin)
	((eq? action-name 'experimental-on) experimental-on)
	((eq? action-name 'ez-ball) ez-ball)	
	((eq? action-name 'filledbox) filledbox)
	((eq? action-name 'repeat-slash) repeat-slash)
	((eq? action-name 'select-font) select-font)
	((eq? action-name 'volta) volta)
	(else (error "unknown tag -- PDF-SCM " action-name))
	)
  )

(define (scm-pdf-output)
  (primitive-eval (pdf-scm 'all-definitions)))

; Local Variables:
; scheme-program-name: "guile"
; End: