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
	     horizontal-line
	     filledbox
	     round-filled-box
	     text
	     tuplet
	     polygon
	     draw-line
	     define-origin
	     no-origin
	     ))

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
    (hashq (ly:font-filename font) 1000000))
   "m"
   (string-encode-integer
    (inexact->exact (round (* 1000 (ly:font-magnification font)))))))

(define (unknown) 
  "%\n\\unknown\n")

(define-public (symbol->tex-key sym)
  (regexp-substitute/global
   #f "_" (output-tex-string (symbol->string sym)) 'pre "X" 'post) )

(define (string->param string)
  (string-append "{" string "}"))

(define (number->param number)
  (string->param (ly:number->string number)))

(define (number-pair->param o)
  (string-append (number->param (car o)) (number->param (cdr o))))

(define-public (tex-number-def prefix key number)
  (string-append
   "\\def\\" prefix (symbol->tex-key key) (string->param number) "%\n"))


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

(define-public (font-load-command bookpaper font)
  (string-append
   "\\font\\" (font-command font) "="
   (ly:font-filename font)
   " scaled "
   (ly:number->string (inexact->exact
		       (round (* 1000
			  (ly:font-magnification font)
			  (ly:bookpaper-outputscale bookpaper)))))
   "\n"))

(define (ez-ball c l b)
  (embedded-ps (list 'ez-ball  c  l b)))

(define (header-to-file fn key val)
  (set! key (symbol->string key))
  (if (not (equal? "-" fn))
      (set! fn (string-append fn "." key))
      )
  (display
   (format "Writing header field `~a' to `~a'..."
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

(define (repeat-slash w a t)
  (embedded-ps (list 'repeat-slash  w a t)))


(define-public (output-tex-string s) ;; todo: rename
   (if (ly:get-option 'safe)
      (regexp-substitute/global #f "\\\\"
				(regexp-substitute/global #f "([{}])" "bla{}" 'pre  "\\" 1 'post )
				'pre "$\\backslash$" 'post)
      
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
  (string-append
   "\\lyitem" (number->param x) (number->param y) (string->param s) "%\n"))

(define (bezier-sandwich l thick)
  (embedded-ps (list 'bezier-sandwich  `(quote ,l) thick)))

;; WTF is this in every backend?
(define (horizontal-line x1 x2 th)
  (filledbox (- x1) (- x2 x1) (* .5 th) (* .5 th)))

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
      ((mapping #f)       ;; (assoc-get  'char-mapping (ly:font-encoding-alist font))))


       ;; TODO: we'd better do this for PS only
       ;; LaTeX gets in the way, and we need to remap
       ;; nonprintable chars.
       

       (input-enc-name #f) ;; (assoc-get 'input-name (ly:font-encoding-alist font) ))
       )

    (string-append "\\hbox{\\" (font-command font)
		   (if (string? input-enc-name)
		       (string-append "\\inputencoding{" input-enc-name "}")
		       "{}")
		   (output-tex-string
		    (if (vector? mapping)
			(reencode-string mapping s)
			s))
		   "}")))


(define (tuplet ht gapx dx dy thick dir)
  (embedded-ps (list 'tuplet  ht gapx dx dy thick dir)))

(define (polygon points blotdiameter)
  (embedded-ps (list 'polygon `(quote ,points) blotdiameter)))

(define (draw-line thick fx fy tx ty)
  (embedded-ps (list 'draw-line thick fx fy tx ty)))

(define (define-origin file line col)
  (if (procedure? point-and-click)
      (string-append "\\special{src:" ;;; \\string ? 
		     (point-and-click line col file)
		     "}" )
      ""))

;; no-origin not yet supported by Xdvi
(define (no-origin) "")

