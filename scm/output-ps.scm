;;;; output-ps.scm -- implement Scheme output routines for PostScript
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c)  1998--2004 Jan Nieuwenhuizen <janneke@gnu.org>
;;;; Han-Wen Nienhuys <hanwen@cs.uu.nl>

;;;; Note: currently misused as testbed for titles with markup, see
;;;;       input/test/title-markup.ly
;;;; 
;;;; TODO:
;;;;   * UGR: SPACE character in CM* fonts
;;;;   * text setting, kerning?
;;;;   * font size and designsize
;;;;   * linewidth
;;;;   * font properties
;;;;   * construction/customisation of title markup
;;;;   * page layout
;;;;   * document output-interface

(debug-enable 'backtrace)

(define-module (scm output-ps))
(define this-module (current-module))

(use-modules
 (guile)
 (ice-9 regex)
 (lily))




;;; Lily output interface, PostScript implementation --- cleanup and docme

;;; Module entry
(define-public (ps-output-expression expr port)
  (display (expression->string expr) port))

;;; Global vars
;; alist containing fontname -> fontcommand assoc (both strings)
(define font-name-alist '())

;; WIP -- stencils from markup? values of output-scopes
(define header-stencil #f)

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

;;; helper functions, not part of output interface
(define (escape-parentheses s)
  (regexp-substitute/global #f "(^|[^\\])([\\(\\)])" s 'pre 1 "\\" 2 'post))

(define (offset-add a b)
  (cons (+ (car a) (car b))
	(+ (cdr a) (cdr b))))

;; FIXME: lily-def
(define (ps-string-def prefix key val)
  (string-append "/" prefix (symbol->string key) " ("
		 (escape-parentheses val)
		 ") def\n"))

(define (ps-number-def prefix key val)
  (let ((s (if (integer? val)
	       (number->string val)
	       (number->string (exact->inexact val)))))
    (string-append "/" prefix (symbol->string key) " " s " def\n")))



;;; Output-interface functions
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

(define (end-output)
  "\nend-lilypond-output\n")

(define (expression->string expr)
  (eval expr this-module))

(define (ez-ball ch letter-col ball-col)
  (string-append
   " (" ch ") "
   (numbers->string (list letter-col ball-col))
   " /Helvetica-Bold " ;; ugh
   " draw_ez_ball"))

(define (filledbox breapth width depth height) 
  (string-append (numbers->string (list breapth width depth height))
		 " draw_box"))

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

(define (horizontal-line x1 x2 th)
  (draw-line th x1  0 x2 0))

(define (lily-def key val)
  (let ((prefix "lilypondpaper"))
    (if (string=?
	 (substring key 0 (min (string-length prefix) (string-length key)))
	 prefix)
	(string-append "/" key " {" val "} bind def\n")
	(string-append "/" key " (" val ") def\n"))))


(define (make-title port)
  (if header-stencil
      (let ((x-ext (ly:stencil-get-extent header-stencil Y))
	    (y-ext (ly:stencil-get-extent header-stencil X)))
	(display (start-system (interval-length x-ext) (interval-length y-ext))
		 port)
	(output-stencil port (ly:stencil-get-expr header-stencil) '(0 . 0))
	(display (stop-system) port)))
  "")

(define (no-origin) "")

;; FIXME: duplictates output-scopes, duplicated in other backends
;; FIXME: silly interface name
(define (output-paper-def pd)
  (let ((prefix "lilypondpaper"))
    
    (define (scope-entry->string key var)
      (let ((val (variable-ref var)))
	(cond
	 ((string? val) (ps-string-def prefix key val))
	 ((number? val) (ps-number-def prefix key val))
	 (else ""))))
      
    (apply
     string-append
     (module-map scope-entry->string (ly:output-def-scope pd)))))

;; FIXME: duplicated in other output backends
;; FIXME: silly interface name
(define (output-scopes paper scopes fields basename)

  ;; FIXME: customise/generate these
  (let ((props '(((font-family . roman)
		  (word-space . 1)
		  (baseline-skip . 2)
		  (font-series . medium)
		  (font-style . roman)
		  (font-shape . upright)
		  (font-size . 0))))
	(prefix "lilypond")
    	(stencils '())
	(baseline-skip 2))

    (define (scope-entry->string key var)
      (let ((val (variable-ref var)))
	
	(if (memq key fields)
	    (header-to-file basename key val))
	
	(cond
	 ((eq? key 'font)
	  BARF
	  (format (current-error-port) "PROPS:~S\n" val)
	  (set! props (cons val props))
	  "")
	 
	 ;; define strings, for /make-lilypond-title to pick up
	 ((string? val) (ps-string-def prefix key val))
	 
	 ;; generate stencil from markup
	 ((markup? val) (set! stencils
			      (append
			       stencils
			       (list
				(interpret-markup paper props val))))
	  "")
	 ((number? val) (ps-number-def prefix key val))
	 (else ""))))
    
    (define (output-scope scope)
      (apply string-append (module-map scope-entry->string scope)))

    (let ((s (string-append (apply string-append (map output-scope scopes)))))
      (set! header-stencil (stack-lines DOWN 0 baseline-skip stencils))
      
      ;; trigger font load
      (ly:stencil-get-expr header-stencil)
      s)))

;; hmm, looks like recursing call is always last statement, does guile
;; think so too?
(define (output-stencil port expr offset)
  (if (pair? expr)
      (let ((head (car expr)))
	(cond
	 ((ly:input-location? head)
	  (display (apply define-origin (ly:input-location head)) port)
	  (output-stencil port (cadr expr) offset))
	 ((eq? head 'no-origin)
	  (display (expression->string head) port)
	  (output-stencil port (cadr expr) offset))
	 ((eq? head 'translate-stencil)
	  (output-stencil port (caddr expr) (offset-add offset (cadr expr))))
	 ((eq? head 'combine-stencil)
	  (output-stencil port (cadr expr) offset)
	  (output-stencil port (caddr expr) offset))
	 (else
	  (display (placebox (car offset) (cdr offset)
			     (expression->string expr)) port))))))

(define (placebox x y s) 
  (string-append 
   (ly:number->string x) " " (ly:number->string y) " {" s "} place-box\n"))

(define (polygon points blotdiameter)
  (string-append
   " "
   (numbers->string points)
   (ly:number->string (/ (length points) 2))
   (ly:number->string blotdiameter)
   " draw_polygon"))

(define (repeat-slash wid slope thick)
  (string-append
   (numbers->string (list wid slope thick))
   " draw_repeat_slash"))

(define (round-filled-box x y width height blotdiam)
   (string-append
    " "
    (numbers->string
     (list x y width height blotdiam)) " draw_round_box"))

(define (symmetric-x-triangle thick w h)
  (string-append
   (numbers->string (list h w thick))
   " draw_symmetric_x_triangle"))

(define (start-system width height)
  (string-append
   "\n" (ly:number->string height)
   " start-system\n"
   "{\n"
   "set-ps-scale-to-lily-scale\n"))

(define (stem breapth width depth height) 
  (string-append
   (numbers->string (list breapth width depth height))
   " draw_box" ))

(define (stop-last-system)
  (stop-system))

(define (stop-system)
  "}\nstop-system\n")

(define (text s)
  (string-append "(" (escape-parentheses s) ") show "))

;; top-of-file, wtf?
(define (top-of-file)
  (header (string-append "GNU LilyPond (" (lilypond-version) "), ")
          (strftime "%c" (localtime (current-time))))
  ;;; ugh
  (ps-string-def
   "lilypond" 'tagline
   (string-append "Engraved by LilyPond (" (lilypond-version) ")")))

(define (unknown) 
  "\n unknown\n")

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
