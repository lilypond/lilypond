;;;; tex.scm -- implement Scheme output routines for TeX
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c)  1998--2004 Jan Nieuwenhuizen <janneke@gnu.org>
;;;;                  Han-Wen Nienhuys <hanwen@cs.uu.nl>


;; (debug-enable 'backtrace)
(define-module (scm output-tex)
  #:re-export (quote)
  #:export (define-fonts
	     font-command
	     unknown
	     output-paper-def
	     output-scopes
	     blank
	     dot
	     beam
	     bracket
	     dashed-slur
	     char
	     dashed-line
	     zigzag-line
	     symmetric-x-triangle
	     ez-ball
	     comment
	     end-output
	     experimental-on
	     repeat-slash
	     header-end
	     header
	     placebox
	     bezier-sandwich
	     start-system
	     stop-system
	     stop-last-system
	     horizontal-line
	     filledbox
	     round-filled-box
	     text
	     tuplet
	     polygon
	     draw-line
	     between-system-string
	     define-origin
	     no-origin
	     start-page
	     stop-page
	     )
  )

(use-modules (ice-9 regex)
	     (ice-9 string-fun)
	     (ice-9 format)
	     (guile)
	     (srfi srfi-13)
	     (lily))

;;;;;;;;
;;;;;;;; DOCUMENT ME!
;;;;;;;;


(define (font-command font)
  (string-append
   "magfont"
   (string-encode-integer
    (hashq (ly:font-name font) 1000000))
   "m"
   (string-encode-integer
    (inexact->exact (round (* 1000 (ly:font-magnification font)))))))

(define (define-fonts paper font-list)
  (apply string-append
	 (map (lambda (x) (font-load-command paper x)) font-list)))

(define (unknown) 
  "%\n\\unknown\n")

(define (symbol->tex-key sym)
  (regexp-substitute/global
   #f "_" (output-tex-string (symbol->string sym)) 'pre "X" 'post) )

(define (tex-string-def prefix key str)
  (if (equal? "" (sans-surrounding-whitespace (output-tex-string str)))
      (string-append "\\let\\" prefix (symbol->tex-key key) "\\undefined%\n")
      (string-append "\\def\\" prefix (symbol->tex-key key) "{"  (output-tex-string str) "}%\n")
      ))

(define (tex-number-def prefix key number)
  (string-append "\\def\\" prefix (symbol->tex-key key) "{" number "}%\n"))

(define (output-paper-def pd)
  (apply
   string-append
   (module-map
    (lambda (sym var)
      (let ((val (variable-ref var))
	    (key (symbol->tex-key sym)))

	(cond
	 ((string? val)
	  (tex-string-def "lilypondpaper" sym val))
	 ((number? val)
	  (tex-number-def "lilypondpaper" sym
			  (if (integer? val)
			      (number->string val)
			      (number->string (exact->inexact val)))))
	 (else ""))))
      
    (ly:output-def-scope pd))))

(define (output-scopes paper scopes fields basename)
  (define (output-scope scope)
    (apply
     string-append
     (module-map
     (lambda (sym var)
       (let ((val (variable-ref var))
	     ;;(val (if (variable-bound? var) (variable-ref var) '""))
	     (tex-key (symbol->string sym)))
	 
	 (if (and (memq sym fields) (string? val))
	     (header-to-file basename sym val))

	 (cond
	  ((string? val)
	   (tex-string-def "lilypond" sym val))
	  ((number? val)
	   (tex-number-def "lilypond" sym
			   (if (integer? val)
			       (number->string val)
			       (number->string (exact->inexact val)))))
	  (else ""))))
     scope)))
  
  (apply string-append
	 (map output-scope scopes)))

(define (blank)
  "")

(define (dot x y radius)
  (embedded-ps (list 'dot x y radius)))

(define (beam width slope thick blot)
  (embedded-ps (list 'beam  width slope thick blot)))

(define (bracket arch_angle arch_width arch_height height arch_thick thick)
  (embedded-ps (list 'bracket  arch_angle arch_width arch_height height arch_thick thick)))

(define (dashed-slur thick dash l)
  (embedded-ps (list 'dashed-slur thick dash `(quote ,l))))

(define (char font i)
  (string-append "\\" (font-command font)
		 "\\char" (ly:inexact->string i 10) " "))

(define (dashed-line thick on off dx dy)
  (embedded-ps (list 'dashed-line  thick on off dx dy)))

(define (zigzag-line centre? zzw zzh thick dx dy)
  (embedded-ps (list 'zigzag-line centre? zzw zzh thick dx dy)))

(define (symmetric-x-triangle t w h)
  (embedded-ps (list 'symmetric-x-triangle t w h)))

(define (font-load-command paper font)
  (string-append
   "\\font\\" (font-command font) "="
   (ly:font-name font)
   " scaled "
   (ly:number->string (inexact->exact
		       (round (* 1000
			  (ly:font-magnification font)
			  (ly:paper-lookup paper 'outputscale)))))
   "\n"))

(define (ez-ball c l b)
  (embedded-ps (list 'ez-ball  c  l b)))

(define (header-to-file fn key val)
  (set! key (symbol->string key))
  (if (not (equal? "-" fn))
      (set! fn (string-append fn "." key))
      )
  (display
   (format "writing header field `~a' to `~a'..."
	   key
	   (if (equal? "-" fn) "<stdout>" fn)
	   )
   (current-error-port))
  (if (equal? fn "-")
      (display val)
      (display val (open-file fn "w"))
  )
  (display "\n" (current-error-port))
  ""
  )

(define (embedded-ps expr)
  (let ((ps-string
	 (with-output-to-string
	   (lambda () (ps-output-expression expr (current-output-port))))))
    (string-append "\\embeddedps{" ps-string "}")))
  
(define (comment s)
  (string-append "% " s "\n"))

(define (end-output) 
  (begin
    ;; uncomment for some stats about lily memory	  
    ;; (display (gc-stats))
    (string-append
     "\\lilypondend\n"
     ;; Put GC stats here.
     )))

(define (experimental-on)
  "")

(define (repeat-slash w a t)
  (embedded-ps (list 'repeat-slash  w a t)))

(define (header-end)
  (string-append
   "\\def\\scaletounit{ "
   (number->string (cond
		     ((equal? (ly:unit) "mm") (/ 72.0  25.4))
		     ((equal? (ly:unit) "pt") (/ 72.0  72.27))
		     (else (error "unknown unit" (ly:unit)))
		     ))
   " mul }%\n"
   "\\ifx\\lilypondstart\\undefined\n"
   "  \\input lilyponddefs\n"
   "\\fi\n"
   "\\outputscale = \\lilypondpaperoutputscale\\lilypondpaperunit\n"
   "\\lilypondstart\n"
   "\\lilypondspecial\n"
   "\\lilypondpostscript\n"))

(define (header creator time-stamp page-count)
  (string-append
   "% Generated by " creator "\n"
   "% at " time-stamp "\n"
   ;; FIXME: duplicated in every backend
   "\\def\\lilypondtagline{Engraved by LilyPond (version "
   (lilypond-version)")}\n"))

;; FIXME: explain ploblem: need to do something to make this really safe.  
(define (output-tex-string s)
  (if safe-mode?
      (regexp-substitute/global #f "\\\\" s 'pre "$\\backslash$" 'post)
      s))

(define (lily-def key val)
  (let ((tex-key
	 (regexp-substitute/global
	      #f "_" (output-tex-string key) 'pre "X" 'post))
	 
	(tex-val (output-tex-string val)))
    (if (equal? (sans-surrounding-whitespace tex-val) "")
	(string-append "\\let\\" tex-key "\\undefined\n")
	(string-append "\\def\\" tex-key "{" tex-val "}%\n"))))

(define (number->dim x)
  (string-append
   ;;ugh ly:* in backend needs compatibility func for standalone output
   (ly:number->string x) " \\outputscale "))

(define (placebox x y s) 
  (string-append "\\lyitem{"
		 (ly:number->string y) "}{"
		 (ly:number->string x) "}{"
		 s "}%\n"))

(define (bezier-sandwich l thick)
  (embedded-ps (list 'bezier-sandwich  `(quote ,l) thick)))

(define (start-system wd ht)
  (string-append "\\leavevmode\n"
		 "\\scoreshift = " (number->dim (* ht 0.5)) "\n"
		 "\\lilypondifundefined{lilypondscoreshift}%\n"
		 "  {}%\n"
		 "  {\\advance\\scoreshift by -\\lilypondscoreshift}%\n"
		 "\\lybox{"
		 (ly:number->string wd) "}{"
		 (ly:number->string ht) "}{%\n"))

(define (stop-system) 
  "}%\n%\n\\interscoreline\n%\n")
(define (stop-last-system)
  "}%\n")

(define (horizontal-line x1 x2 th)
  (filledbox (- x1)  (- x2 x1) (* .5 th)  (* .5 th )))

(define (filledbox breapth width depth height)
  (if (and #f (defined? 'ps-testing))
      (embedded-ps
       (string-append (ly:numbers->string (list breapth width depth height))
		      " draw_box" ))
      (string-append "\\lyvrule{"
		     (ly:number->string (- breapth)) "}{"
		     (ly:number->string (+ breapth width)) "}{"
		     (ly:number->string depth) "}{"
		     (ly:number->string height) "}")))

(define (round-filled-box x y width height blotdiam)
  (embedded-ps (list 'round-filled-box  x y width height blotdiam)))

(define (text font s)
  (let*
      ((perm (caddr (ly:font-encoding font))))
  (string-append "\\hbox{\\" (font-command font) "{}"
		 (output-tex-string
		  (if (vector? perm)
		      (reencode-string perm s)
		      s))
		  "}")))

(define (tuplet ht gapx dx dy thick dir)
  (embedded-ps (list 'tuplet  ht gapx dx dy thick dir)))

(define (polygon points blotdiameter)
  (embedded-ps (list 'polygon `(quote ,points) blotdiameter)))

(define (draw-line thick fx fy tx ty)
  (embedded-ps (list 'draw-line thick fx fy tx ty)))

;; TODO: this should be a default, which is overriden in PS
(define (between-system-string string)
  string
  )
(define (define-origin file line col)
  (if (procedure? point-and-click)
      (string-append "\\special{src:" ;;; \\string ? 
		     (point-and-click line col file)
		     "}" )
      ""))

;; no-origin not yet supported by Xdvi
(define (no-origin) "")

(define (start-page)
  "\n%\\vbox{\n")

(define (stop-page last?)
  (if last?
      "\n%}\n"
      "\n%}\n\\newpage\n"))
