;;; pdftex.scm -- implement Scheme output routines for PDFTeX
;;;
;;;  source file of the GNU LilyPond music typesetter
;;;  modified from the existing tex.scm
;;; 
;;; (c) 1998--2001 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Han-Wen Nienhuys <hanwen@cs.uu.nl>
;;; Stephen Peters <portnoy@portnoy.org>


;; TODO: port this  to the new module framework.

(define-module (scm pdftex))

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
		    (ly-number->string (cdr name-mag-pair))
		    ))
	  "") ; issue no command
	(string-append "\\" (cddr c)))
    
    
    ))

(define (beam width slope thick)
  (embedded-pdf ((pdf-scm 'beam) width slope thick)))

(define (bracket arch_angle arch_width arch_height height arch_thick thick)
  (embedded-pdf ((pdf-scm 'bracket) arch_angle arch_width arch_height height arch_thick thick)))

(define (dashed-slur thick dash l)
  (embedded-pdf ((pdf-scm 'dashed-slur)  thick dash l)))

(define (hairpin thick w sh eh)
  (embedded-pdf ((pdf-scm 'hairpin) thick w sh eh)))

(define (char i)
  (string-append "\\char" (inexact->string i 10) " "))

(define (dashed-line thick on off dx dy)
  (embedded-pdf ((pdf-scm 'dashed-line) thick on off dx dy)))

(define (font-load-command name-mag command)
  (string-append
   "\\font\\" command "="
   (car name-mag)
   " scaled "
   (ly-number->string (inexact->exact (* 1000  (cdr name-mag))))
   "\n"))

(define (ez-ball c l b)
  (embedded-pdf ((pdf-scm 'ez-ball) c  l b)))

(define (embedded-pdf s)
  (string-append "\\embeddedpdf{ " s "}"))

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

(define (repeat-slash w a t)
  (embedded-pdf ((pdf-scm 'repeat-slash) w a t)))

(define (font-switch i)
  (string-append
   "\\" (font i) "\n"))

(define (font-def i s)
  (string-append
   "\\font" (font-switch i) "=" s "\n"))

(define (header-end)
  (string-append
   "\\input lilyponddefs\\newdimen\\outputscale \\outputscale=\\lilypondpaperoutputscale pt"
   "\\turnOnPostScript"
   "\\pdfcompresslevel=0"))

;; Note: this string must match the string in ly2dvi.py!!!
(define (header creator generate) 
  (string-append
   "% Generated automatically by: " creator generate "\n"))

(define (invoke-char s i)
  (string-append 
   "\n\\" s "{" (inexact->string i 10) "}" ))

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
  (let ((tex-key
	 (if use-regex
	     (regexp-substitute/global 
	      #f "_" (output-tex-string key) 'pre "X" 'post)      
	     (output-tex-string key)))
	(tex-val (output-tex-string val)))
    (if (equal? (sans-surrounding-whitespace tex-val) "")
	(string-append "\\let\\" tex-key "\\undefined\n")
	(string-append "\\def\\" tex-key "{" tex-val "}\n"))))

(define (number->dim x)
  (string-append
   ;;ugh ly-* in backend needs compatibility func for standalone output
   (ly-number->string x) " \\outputscale "))

(define (placebox x y s) 
  (string-append 
   "\\placebox{"
   (number->dim y) "}{" (number->dim x) "}{" s "}\n"))

(define (bezier-sandwich l thick)
  (embedded-pdf ((pdf-scm 'bezier-sandwich) l thick)))

(define (start-system ht)
  (string-append"\\vbox to " (number->dim ht) "{\\hbox{%\n"))

(define (stop-system) 
  "}\\vss}\\interscoreline\n")
(define (stop-last-system)
  "}\\vss}")
(define (filledbox breapth width depth height) 
  (string-append 
   "\\kern" (number->dim (- breapth))
   "\\vrule width " (number->dim (+ breapth width))
   "depth " (number->dim depth)
   "height " (number->dim height) " "))

(define (roundfilledbox x width y height blotdiam)
  (embedded-pdf ((pdf-scm 'roundfilledbox) x width y height blotdiam)))

(define (text s)
  (string-append "\\hbox{" (output-tex-string s) "}"))

(define (tuplet ht gapx dx dy thick dir)
  (embedded-pdf ((pdf-scm 'tuplet) ht gapx dx dy thick dir)))

(define (volta h w thick vert_start vert_end)
  (embedded-pdf ((pdf-scm 'volta) h w thick vert_start vert_end)))

(define (define-origin file line col)
  (if (procedure? point-and-click)
      (string-append "\\special{src:\\string:"
		     (point-and-click line col file)
		     "}" )
      "")
  )

					; no-origin not supported in PDFTeX
(define (no-origin) "")


(define (scm-pdftex-output)
  (primitive-eval (pdftex-scm 'all-definitions)))
