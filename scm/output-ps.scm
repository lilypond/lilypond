;;;; ps.scm -- implement Scheme output routines for PostScript
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c)  1998--2004 Jan Nieuwenhuizen <janneke@gnu.org>
;;;; Han-Wen Nienhuys <hanwen@cs.uu.nl>


(debug-enable 'backtrace)

(define-module (scm output-ps))
(define this-module (current-module))

(use-modules
 (guile)
 (ice-9 regex)
 (lily))



;;; Lily output interface --- cleanup and docme

;; TODO: fucks up if outputting strings with parentheses.

;; Module entry
(define-public (ps-output-expression expr port)
  (display (expression->string expr) port))

(define (expression->string expr)
  (eval expr this-module))

;; Global vars

;; alist containing fontname -> fontcommand assoc (both strings)
(define font-name-alist '())


;; Interface functions
(define (beam width slope thick blot)
  (string-append
   (numbers->string (list slope width thick blot)) " draw_beam" ))

;; two beziers
(define (bezier-sandwich l thick)
  (string-append 
   (apply string-append (map number-pair->string l))
   (ly:number->string thick)
   " draw_bezier_sandwich "))

(define (bracket arch_angle arch_width arch_height  height arch_thick thick)
  (string-append
   (numbers->string
    (list arch_angle arch_width arch_height height arch_thick thick))
   " draw_bracket"))

(define (symmetric-x-triangle thick w h)
  (string-append
   (numbers->string (list h w thick))
   " draw_symmetric_x_triangle"))


(define (char i)
  (string-append 
   "(\\" (inexact->string i 8) ") show " ))


(define (comment s)
  (string-append "% " s "\n"))


(define (dashed-line thick on off dx dy)
  (string-append 
   (ly:number->string dx)
   " "
   (ly:number->string dy)
   " "
   (ly:number->string thick)
   " [ "
   (ly:number->string on)
   " "
   (ly:number->string off)
   " ] 0 draw_dashed_line"))

;; what the heck is this interface ?
(define (dashed-slur thick dash l)
  (string-append 
   (apply string-append (map number-pair->string l)) 
   (ly:number->string thick) 
   " [ "
   (ly:number->string dash)
   " "
   ;;UGH.  10 ?
   (ly:number->string (* 10 thick))
   " ] 0 draw_dashed_slur"))

(define lily-traced-cm-fonts
  (map symbol->string
       '(cmbx14
	 cmbx17
	 cmbxti12
	 cmbxti14
	 cmbxti6
	 cmbxti7
	 cmbxti8
	 cmcsc12
	 cmcsc7
	 cmcsc8
	 cmss5
	 cmss6
	 cmss7
	 cmti5
	 cmti6
	 cmtt17
	 cmtt5
	 cmtt6
	 cmtt7)))


(define (define-fonts internal-external-name-mag-pairs)

  (define (font-load-command name-mag command)

    ;; frobnicate NAME to jibe with external definitions.
    (define (possibly-capitalize-font-name name)
      (cond
       ((and (equal? (substring name 0 2) "cm")
	     (not (member name lily-traced-cm-fonts)))
	
	;; huh, how is this supposed to work?
	;;(string-upcase name)
	
	(string-append name ".pfb"))
       
       ((equal? (substring name 0 4) "feta")
	(regexp-substitute/global #f "feta([a-z-]*)([0-9]+)" name 'pre "GNU-LilyPond-feta" 1 "-" 2 'post))
       (else name)))
    
    ;;(format (current-error-port) "DEFINE-FONTS: ~S\n" internal-external-name-mag-pairs)
    
    (string-append
     "/" command
     " { /"
     ;; Ugh, the Bluesky type1 fonts for computer modern use capitalized 
     ;; postscript font names.
     (possibly-capitalize-font-name (car name-mag))
     " findfont "
     "20 " (ly:number->string (cdr name-mag)) " mul "
     "output-scale div scalefont setfont } bind def "
     "\n"))

  (define (ps-encoded-fontswitch name-mag-pair)
    (let* ((key (car name-mag-pair))
	   (value (cdr name-mag-pair)))
      (cons key
	    (cons value
		  (string-append "lilyfont"
				 (car value)
				 "-"
				 (number->string (cdr value)))))))
      
  (set! font-name-alist (map ps-encoded-fontswitch
			     internal-external-name-mag-pairs))

  (apply string-append
	 (map (lambda (x) (font-load-command (car x) (cdr x)))
	      (map cdr font-name-alist))))

(define (define-origin file line col) "")

(define (dot x y radius)
  (string-append
   " "
   (numbers->string
    (list x y radius)) " draw_dot"))

(define (zigzag-line centre? zzw zzh thick dx dy)
  (string-append
    (if centre? "true" "false")
    " "
    (ly:number->string zzw)
    " "
    (ly:number->string zzh)
    " "
    (ly:number->string thick)
    " 0 0 "
    (ly:number->string dx)
    " "
    (ly:number->string dy)
    " draw_zigzag_line "))

(define (draw-line thick x1 y1 x2 y2)
  (string-append 
  "	1 setlinecap
	1 setlinejoin "
  (ly:number->string thick)
	" setlinewidth "
   (ly:number->string x1)
   " "
   (ly:number->string y1)
   " moveto "
   (ly:number->string x2)
   " "
   (ly:number->string y2)
   " lineto stroke"))

(define (polygon points blotdiameter)
  (string-append
   " "
   (numbers->string points)
   (ly:number->string (/ (length points) 2))
   (ly:number->string blotdiameter)
   " draw_polygon"))

(define (end-output)
  "\nend-lilypond-output\n")

(define (ez-ball ch letter-col ball-col)
  (string-append
   " (" ch ") "
   (numbers->string (list letter-col ball-col))
   " /Helvetica-Bold " ;; ugh
   " draw_ez_ball"))

(define (filledbox breapth width depth height) 
  (string-append (numbers->string (list breapth width depth height))
		 " draw_box"))

(define (horizontal-line x1 x2 th)
  (draw-line th x1  0 x2 0))

(define (fontify name-mag-pair exp)

  (define (select-font name-mag-pair)
    (let ((c (assoc name-mag-pair font-name-alist)))
      
      (if c
	  (string-append " " (cddr c) " ")
	  (begin
	    (ly:warn
	     (format "Programming error: No such font: ~S" name-mag-pair))
	    
	    (display "FAILED\n" (current-error-port))
	    (if #f ;(pair? name-mag-pair))
		(display (object-type (car name-mag-pair)) (current-error-port))
		(write name-mag-pair (current-error-port)))
	    (if #f ;  (pair? font-name-alist)
		(display
		 (object-type (caaar font-name-alist)) (current-error-port))
		(write font-name-alist (current-error-port)))

	    ;; (format #f "\n%FAILED: (select-font ~S)\n" name-mag-pair))
	    ""))))
  
  (string-append (select-font name-mag-pair) exp))

(define (header creator generate) 
  (string-append
   "%!PS-Adobe-3.0\n"
   "%%Creator: " creator generate "\n"))

(define (header-end)
  (string-append
   ;; URG: now we can't use scm output without Lily
   (ly:gulp-file "lilyponddefs.ps")
   "{exch pop //systemdict /run get exec}\n\n"
   (ly:gulp-file "music-drawing-routines.ps")
   "{ exch pop //systemdict /run get exec }\n\n"
   ;; ps-testing wreaks havoc when used with lilypond-book.
   ;;  -- is this still true with new modules system?
;;   (if (defined? 'ps-testing) "\n /testing true def" "")
  ;   "\n /testing true def"
   ))

(define (lily-def key val)
  (let ((prefix "lilypondpaper"))
    (if (string=?
	 (substring key 0 (min (string-length prefix) (string-length key)))
	 prefix)
	(string-append "/" key " {" val "} bind def\n")
	(string-append "/" key " (" val ") def\n"))))

(define (no-origin) "")
  
(define (placebox x y s) 
  (string-append 
   (ly:number->string x) " " (ly:number->string y) " {" s "} place-box\n"))

(define (repeat-slash wid slope thick)
  (string-append
   (numbers->string (list wid slope thick))
   " draw_repeat_slash"))

(define (round-filled-box x y width height blotdiam)
   (string-append
    " "
    (numbers->string
     (list x y width height blotdiam)) " draw_round_box"))

(define (start-system width height)
  (string-append
   "\n" (ly:number->string height)
   " start-system\n"
   "{\n"
   "set-ps-scale-to-lily-scale\n"

   ;; URG
   (if (pair? header-stencils)
       (let ((s (output-stencils header-stencils)))
	 (set! header-stencils '())
	 (string-append s (stop-system) (start-system width height)))
       "")))

(define (stem breapth width depth height) 
  (string-append
   (numbers->string (list breapth width depth height))
   " draw_box" ))

(define (stop-last-system)
  (stop-system))

(define (stop-system)
  "}\nstop-system\n")

(define (text s)
  (string-append "(" s ") show "))

(define (unknown) 
  "\n unknown\n")

;; top-of-file, wtf?
(define (top-of-file)
  (header (string-append "GNU LilyPond (" (lilypond-version) "), ")
          (strftime "%c" (localtime (current-time))))
  ;;; ugh
  (ps-string-def
   "lilypond" 'tagline
   (string-append "Engraved by LilyPond (" (lilypond-version) ")")))

(define (output-paper-def pd)
  (apply
   string-append
   (module-map
    (lambda (sym var)
      (let ((val (variable-ref var))
	    (key (symbol->string sym)))
	
	(cond
	 ((string? val)
	  (ps-string-def "lilypondpaper" sym val))
	 ((number? val)
	  (ps-number-def "lilypondpaper" sym
			 (if (integer? val)
			     (number->string val)
			     (number->string (exact->inexact val)))))
	 (else ""))))
      
    (ly:output-def-scope pd))))


(define (ps-string-def a b c)
  (string-append "/" a (symbol->string b) " (" c ") def\n"))

(define (ps-number-def a b c)
  (string-append "/" a (symbol->string b) " " c " def\n"))


(define (output-scopes paper scopes fields basename)

  ;; FIXME: customise/generate these
  (let ((props '((;;(linewidth . 120)
		  (font-family . roman)
		  (word-space . 1)
		  (baseline-skip . 2)
		  (font-series . medium)
		  (font-style . roman)
		  (font-shape . upright)
		  (font-size . 0)))))
  
    (define (output-scope scope)
      (apply
       string-append
       (module-map
	(lambda (sym var)
	  (let ((val (variable-ref var))
		(tex-key (symbol->string sym)))
	    
	    (if (memq sym fields)
		(header-to-file basename sym val))
	    
	    (cond
	     ;; define strings, for /make-lilypond-title to pick up
	     ((string? val) (ps-string-def "lilypond" sym val))

	     ;; output markups ourselves
	     ((markup? val) (set! header-stencils
				  (append header-stencils
				     (list
				      (ly:stencil-get-expr
				       (interpret-markup paper props val)))))
	      
	      "")
	     ((number? val) (ps-number-def
			     "lilypond" sym (if (integer? val)
						(number->string val)
						(number->string
						 (exact->inexact val)))))
	     (else ""))))
	scope)))

    (string-append
     (apply string-append (map output-scope scopes)))))

(define (add-offsets a b)
  (cons (+ (car a) (car b))
	(+ (cdr a) (cdr b))))

(define (input? foe)
  #f)

(define header-stencils '())

(define (output-stencils lst)
  (apply string-append (map (lambda (x) (output-stencil x '(10 . -10))) lst)))

;; TODO:
;; de-urg me
;; implement ly:input stuff
;; replace C++ variant
;; stencil->string?
(define (output-stencil expr o)
  (let ((s ""))
    (while
     (pair? expr)
     (let ((head (car expr)))
       (cond ((input? head)
	      (set! s (string-append
		       s (define-origin (ly:input-file-string head))))
	      (set! expr (cadr expr)))
	     ((eq? head 'no-origin)
	      (set! s (string-append s (expression->string head)))
	      (set! expr (cadr expr)))
	     ((eq? head 'translate-stencil)
	      (set! o (add-offsets o (cadr expr)))
	      (set! expr (caddr expr)))
	     ((eq? head 'combine-stencil)
	      (set! s (string-append s (output-stencil (cadr expr) o)))
	      (set! expr (caddr expr)))
	     (else
	      (set!
	       s (string-append
		  s
		       (placebox (car o) (cdr o)
				 (expression->string expr))))
	      (set! expr #f)))))
  s))
