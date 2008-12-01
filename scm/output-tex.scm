;;;; tex.scm -- implement Scheme output routines for TeX
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 1998--2008 Jan Nieuwenhuizen <janneke@gnu.org>
;;;;                 Han-Wen Nienhuys <hanwen@xs4all.nl>


;; The public interface is tight.
;; It has to be, because user-code is evalled with this module.

;; ***It should also be clean, well defined, documented and reviewed***

;; To be reasonably safe, you probably do not want to use the TeX
;; backend anyway, but rather the PostScript backend.  You may want
;; to run gs in a uml sandbox too.


(define-module (scm output-tex)
  #:re-export (quote)

  ;; JUNK this -- see lily.scm: ly:all-output-backend-commands
  #:export (unknown
	    blank
	    circle
	    dot
	    dashed-slur
	    named-glyph
	    dashed-line
	    comment
	    repeat-slash
	    placebox
	    bezier-sandwich
	    round-filled-box
	    text
	    setcolor
	    resetcolor
	    polygon
	    draw-line
	    no-origin
	    grob-cause))

(use-modules (ice-9 regex)
	     (ice-9 string-fun)
	     (guile)
	     (srfi srfi-13)
	     (scm framework-tex)
	     (lily))



;;;;;;;;
;;;;;;;; DOCUMENT ME!
;;;;;;;;


(define (char font i)
  (string-append "\\" (tex-font-command font)
		 "\\char" (ly:inexact->string i 10) " "))

(define (unknown) 
  "%\n\\unknown\n")

(define (url-link url x y)
  "")

(define (blank)
  "")

(define (circle radius thick)
  (embedded-ps (list 'circle radius thick)))

(define (dot x y radius)
  (embedded-ps (list 'dot x y radius)))

(define (embedded-ps string)
  (embedded-ps (list 'embedded-ps string)))

(define (dashed-slur thick on off lst)
  (embedded-ps (list 'dashed-slur thick on off `(quote ,lst))))

(define (named-glyph font name)
  (let* ((info (ly:otf-font-glyph-info font name))
	 (subfont (assoc-get 'subfont info))
	 (subidx  (assoc-get 'subfont-index info)))
    
    ;;(stderr "INFO: ~S\n" info)
    ;;(stderr "FONT: ~S\n" font)
    (if (and subfont subidx)
	(string-append "\\" (tex-font-command-raw
			     subfont
			     (ly:font-magnification font))
		       "\\char" (number->string subidx))

	(begin
	  (ly:warning (_ "cannot find ~a in ~a" name font))
	  ""))))

(define (dashed-line thick on off dx dy phase)
  (embedded-ps (list 'dashed-line  thick on off dx dy phase)))

(define (embedded-ps expr)
  (let ((ps-string
	 (with-output-to-string
	   (lambda () (ps-output-expression expr (current-output-port))))))
    (string-append "\\embeddedps{" ps-string "}")))

(define (repeat-slash w a t)
  (embedded-ps (list 'repeat-slash  w a t)))

(define (number->dim x)
  (string-append
   ;;ugh ly:* in backend needs compatibility func for standalone output
   (ly:number->string x) " \\output-scale "))

(define (placebox x y s) 
  (string-append
   "\\lyitem{" (ly:number->string x) "}{" (ly:number->string y) "}{" s "}%\n"))

(define (bezier-sandwich lst thick)
  (embedded-ps (list 'bezier-sandwich `(quote ,lst) thick)))


(define (round-filled-box x y width height blotdiam)
  (embedded-ps (list 'round-filled-box  x y width height blotdiam)))

(define (text font s)
  (format #f
   "\\hbox{\\~a{}~a}" (tex-font-command font)
   (sanitize-tex-string s)))

(define (setcolor r g b)
  (string-append "\\color[rgb]{"
  (number->string r) ", "
  (number->string g) ", "
  (number->string b) "}"))

;; FIXME
;; The PostScript backend saves the current color
;; during setcolor and restores it during resetcolor.
;; We don't do that here.
(define (resetcolor)
  (string-append "\\color[rgb]{0,0,0}\n"))

(define (polygon points blot-diameter fill)
  (embedded-ps (list 'polygon `(quote ,points) blot-diameter fill)))

(define (draw-line thick fx fy tx ty)
  (embedded-ps (list 'draw-line thick fx fy tx ty)))

;; no-origin not yet supported by Xdvi
(define (no-origin) "")


(define-public (line-location  file line col)
  "Print an input location, without column number ."
  (string-append (number->string line) " " file))

(define-public point-and-click #f)

(define (grob-cause offset grob)
  (define (line-column-location file line col)
    "Print an input location, including column number ."
    (string-append (number->string line) ":"
		   (number->string col) " " file))

  (if (procedure? point-and-click)
      (let* ((cause (ly:grob-property grob 'cause))
	     (music-origin (if (ly:stream-event? cause)
			       (ly:event-property cause 'origin)))
	     (location (if (ly:input-location? music-origin)
			   (ly:input-file-line-column music-origin))))
	(if (pair? location)
	     ;;; \\string ? 
	    (string-append "\\special{src:"
			   (line-column-location location) "}")
	    ""))
      ""))
