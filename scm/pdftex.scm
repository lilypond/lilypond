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

(use-modules (scm pdf)
	     (guile)
	     (ice-9 regex)
	     (ice-9 string-fun)	  
	     )

(define this-module (current-module))
(define (unknown) 
  "%\n\\unknown\n")


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
  (embedded-pdf (list 'beam  width slope thick)))

(define (bracket arch_angle arch_width arch_height height arch_thick thick)
  (embedded-pdf (list 'bracket  arch_angle arch_width arch_height height arch_thick thick)))

(define (dashed-slur thick dash l)
  (embedded-pdf (list 'dashed-slur   thick dash l)))

(define (char i)
  (string-append "\\char" (inexact->string i 10) " "))

(define (dashed-line thick on off dx dy)
  (embedded-pdf (list 'dashed-line  thick on off dx dy)))

(define (font-load-command name-mag command)
  (string-append
   "\\font\\" command "="
   (car name-mag)
   " scaled "
   (ly-number->string (inexact->exact (* 1000  (cdr name-mag))))
   "\n"))

(define (ez-ball c l b)
  (embedded-pdf (list 'ez-ball  c  l b)))

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

(if (or (equal? (minor-version) "4.1")
	(equal? (minor-version) "4")
	(equal? (minor-version) "3.4"))
    (define (embedded-pdf expr)
      (let ((ps-string
	     (with-output-to-string
	       (lambda () (pdf-output-expression expr (current-output-port))))))
	(string-append "\\embeddedpdf{" ps-string "}")))
    (define (embedded-pdf expr)
      (let
	  ((os (open-output-string)))
	(pdf-output-expression expr os)
	(string-append "\\embeddedpdf{" (get-output-string os) "}"))))


(define (comment s)
  (simple-format #f "% ~a\n" s))

(define (end-output) 
  (begin
					; uncomment for some stats about lily memory	  
					;		(display (gc-stats))
    (string-append "%\n\\lilypondend\n"
					; Put GC stats here.
		   )))

(define (experimental-on)
  "")

(define (repeat-slash w a t)
  (embedded-pdf (list 'repeat-slash w a t)))
(define (fontify name-mag-pair exp)
  (string-append (select-font name-mag-pair)
		 exp))


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
(define (define-fonts internal-external-name-mag-pairs)
  (set! font-name-alist (map tex-encoded-fontswitch
			     internal-external-name-mag-pairs))
  (apply string-append
	 (map (lambda (x)
		(font-load-command (car x) (cdr x)))
	      (map cdr font-name-alist))))


(define (font-switch i)
  (string-append
   "\\" (font i) "\n"))

(define (font-def i s)
  (string-append
   "\\font" (font-switch i) "=" s "\n"))

(define (header-end)
  (string-append
   "\\def\\lilyoutputscalefactor{"
   (number->string (cond
		    ((equal? (ly-unit) "mm") (/ 72.0  25.4))
		    ((equal? (ly-unit) "pt") (/ 72.0  72.27))
		    (else (error "unknown unit" (ly-unit)))
		    ))
   "}%\n"
   "\\ifx\\lilypondstart\\undefined\n"
   "  \\input lilyponddefs\n"
   "\\fi\n"
   "\\outputscale=\\lilypondpaperoutputscale \\lilypondpaperunit\n"
   "\\lilypondpostscript\n"
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
	(string-append "\\def\\" tex-key "{" tex-val "}%\n"))))

(define (number->dim x)
  (string-append
   ;;ugh ly-* in backend needs compatibility func for standalone output
   (ly-number->string x) " \\outputscale "))

(define (placebox x y s) 
  (string-append "\\lyitem{"
		 (ly-number->string y) "}{"
		 (ly-number->string x) "}{"
		 s "}%\n"))

(define (bezier-bow l thick)
  (embedded-pdf (list 'bezier-bow  `(quote ,l) thick)))

(define (bezier-sandwich l thick)
  (embedded-pdf (list 'bezier-sandwich  `(quote ,l) thick)))

(define (start-system wd ht)
  (string-append "\\leavevmode\n"
		 "\\scoreshift = " (number->dim (* ht 0.5)) "\n"
		 "\\lilypondifundefined{lilypondscoreshift}%\n"
		 "  {}%\n"
		 "  {\\advance\\scoreshift by -\\lilypondscoreshift}%\n"
		 "\\lybox{"
		 (ly-number->string wd) "}{"
		 (ly-number->string ht) "{%\n"))

(define (stop-system) 
  "}%\n%\n\\interscoreline\n%\n")
(define (stop-last-system)
  "}%\n")

(define (filledbox breapth width depth height) 
  (string-append "\\lyvrule{"
		 (ly-number->string (- breapth)) "}{"
		 (ly-number->string (+ breapth width)) "}{"
		 (ly-number->string depth) "}{"
		 (ly-number->string height) "}"))

(define (roundfilledbox x y width height blotdiam)
  (embedded-pdf (list 'roundfilledbox  x y width height blotdiam)))

(define (text s)
  (string-append "\\hbox{" (output-tex-string s) "}"))

(define (draw-line thick fx fy tx ty)
  (embedded-pdf (list 'draw-line thick fx fy tx ty)))

(define (define-origin file line col)
  (if (procedure? point-and-click)
      (string-append "\\special{src:\\string:"
		     (point-and-click line col file)
		     "}" )
      "")
  )

					; no-origin not supported in PDFTeX
(define (no-origin) "")



(define my-eval-in-module eval)

(if (or (equal? (minor-version) "4.1")
	(equal? (minor-version) "4")
	(equal? (minor-version) "3.4"))
    (set! my-eval-in-module eval-in-module))

(define-public (pdftex-output-expression expr port)
  (display (my-eval-in-module expr this-module) port) )
