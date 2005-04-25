;;;; output-ps.scm -- implement Scheme output interface for PostScript
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 1998--2005 Jan Nieuwenhuizen <janneke@gnu.org>
;;;;                 Han-Wen Nienhuys <hanwen@cs.uu.nl>

;;;; Note: currently misused as testbed for titles with markup, see
;;;;       input/test/title-markup.ly
;;;; 
;;;; TODO:
;;;;   * %% Papersize in (header ...)
;;;;   * text setting, kerning.
;;;;   * document output-interface

(define-module (scm output-ps)
  #:re-export (quote)

  ;; JUNK this -- see lily.scm: ly:all-output-backend-commands
  #:export (unknown
	    blank
	    circle
	    dot
	    white-dot
	    beam
	    bracket
	    dashed-slur
	    char
	    setcolor
	    resetcolor
	    named-glyph
	    dashed-line
	    zigzag-line
	    ez-ball
	    comment
	    repeat-slash
	    placebox
	    bezier-sandwich
	    horizontal-line
	    embedded-ps
	    filledbox
	    round-filled-box
	    text
	    white-text
	    polygon
	    draw-line
	    no-origin))


(use-modules (guile)
	     (ice-9 regex)
	     (srfi srfi-1)
	     (srfi srfi-13)
	     (scm framework-ps)
	     (lily))


;;(map export
;;   (append (ly:all-stencil-expressions) (ly:all-output-backend-commands)))

;; huh?
;;(write (ly:all-output-backend-commands))
;;(write (ly:all-stencil-expressions))


;;; helper functions, not part of output interface
(define (escape-parentheses s)
  (regexp-substitute/global #f "(^|[^\\])([\\(\\)])" s 'pre 1 "\\" 2 'post))

(define (ps-encoding text)
  (escape-parentheses text))

;; FIXME: lily-def
(define-public (ps-string-def prefix key val)
  (string-append "/" prefix (symbol->string key) " ("
		 (escape-parentheses val)
		 ") def\n"))


(define (ps-number-def prefix key val)
  (let ((s (if (integer? val)
	       (ly:number->string val)
	       (ly:number->string (exact->inexact val)))))
    (string-append "/" prefix (symbol->string key) " " s " def\n")))


;;;
;;; Lily output interface, PostScript implementation --- cleanup and docme
;;;

;;; Output-interface functions
(define (beam width slope thick blot)
  (string-append
   (ly:numbers->string (list slope width thick blot)) " draw_beam" ))

;; two beziers
(define (bezier-sandwich lst thick)
  (string-append 
   (string-join (map ly:number-pair->string lst) " ")
   " "
   (ly:number->string thick)
   " draw_bezier_sandwich"))

(define (bracket arch_angle arch_width arch_height height arch_thick thick)
  (string-append
   (ly:numbers->string
    (list arch_angle arch_width arch_height height arch_thick thick))
   " draw_bracket"))

(define (circle radius thick fill)
  (format
   "~a ~a ~a draw_circle" radius thick
   (if fill
       "true "
       "false ")
   ))

(define (char font i)
  (string-append 
   (ps-font-command font) " setfont " 
   "(\\" (ly:inexact->string i 8) ") show"))

(define (dashed-line thick on off dx dy)
  (string-append 
   (ly:number->string dx) " "
   (ly:number->string dy) " "
   (ly:number->string thick)
   " [ "
   (ly:number->string on) " "
   (ly:number->string off)
   " ] 0 draw_dashed_line"))

;; what the heck is this interface ?
(define (dashed-slur thick on off l)
  (string-append 
   (string-join (map ly:number-pair->string l) " ")
   " "
   (ly:number->string thick) 
   " [ "
   (ly:number->string on)
   " "   
   (ly:number->string off)
   " ] 0 draw_dashed_slur"))

(define (dot x y radius)
  (string-append
   " "
   (ly:numbers->string
    (list x y radius)) " draw_dot"))

(define (draw-line thick x1 y1 x2 y2)
  (string-append 
   "1 setlinecap 1 setlinejoin "
   (ly:number->string thick) " setlinewidth "
   (ly:number->string x1) " "
   (ly:number->string y1) " moveto "
   (ly:number->string x2) " "
   (ly:number->string y2) " lineto stroke"))

(define (embedded-ps string)
  string)

;; FIXME. 
(define (ez-ball ch letter-col ball-col)
  (string-append
   " (" ch ") "
   (ly:numbers->string (list letter-col ball-col))
   ;; FIXME: barf
   " /Helvetica-Bold "
   " draw_ez_ball"))

;; FIXME: use draw_round_box
(define (filledbox breapth width depth height)
  (string-append (ly:numbers->string (list breapth width depth height))
		 " draw_box"))

(define (glyph-string
	 postscript-font-name
	 size
	 x-y-named-glyphs)
  (format #f "gsave 1 output-scale div 1 output-scale div scale
  /~a findfont ~a scalefont setfont\n~a grestore" postscript-font-name size
  (apply
   string-append
   (map (lambda  (item)
	  (let
	      ((x (car item))
		(y (cadr item))
		(g (caddr item)))

	    (if (and (= 0.0 x)
		     (= 0.0 y))
		(format #f " /~a glyphshow\n" g)
		(format #f " ~a ~a rmoveto /~a glyphshow\n"
			x y g))))
	x-y-named-glyphs))
  ))

(define (grob-cause offset grob)
  (let* ((cause (ly:grob-property grob 'cause))
	 (music-origin (if (ly:music? cause)
			   (ly:music-property cause 'origin)))
	 (location (if (ly:input-location? music-origin)
		       (ly:input-file-line-column music-origin)
		       #f
		       ))
	 (file (if (string? location)
		   (if (and
			(> (string-length location) 0)
			(eq? (string-ref (car location) 0 ) #\/))

		       location
		       (string-append (getcwd) "/" (car location)))
		   #f))
	 (x-ext (ly:grob-extent grob grob X)) 
	 (y-ext (ly:grob-extent grob grob Y)))

    (if (and location
	     (< 0 (interval-length x-ext))
	     (< 0 (interval-length y-ext)))
	
	(format "~a ~a ~a ~a (textedit://~a:~a:~a) mark_URI\n"
		(+ (car offset) (car x-ext))
		(+ (cdr offset) (car y-ext))
		(+ (car offset) (cdr x-ext))
		(+ (cdr offset) (cdr y-ext))
		file
		(cadr location)
		(caddr location))
	"")))

;; WTF is this in every backend?
(define (horizontal-line x1 x2 th)
  (draw-line th x1 0 x2 0))

(define (lily-def key val)
  (let ((prefix "lilypondlayout"))
    (if (string=?
	 (substring key 0 (min (string-length prefix) (string-length key)))
	 prefix)
	(string-append "/" key " {" val "} bind def\n")
	(string-append "/" key " (" val ") def\n"))))

(define (named-glyph font glyph)
  (string-append 
   (ps-font-command font) " setfont " 
   "/" glyph " glyphshow "))

(define (no-origin)
  "")

(define (placebox x y s) 
  (string-append 
   (ly:number->string x) " " (ly:number->string y) " { " s " } place-box\n"))

(define (polygon points blotdiameter)
  (string-append
   (ly:numbers->string points) " "
   (ly:number->string (/ (length points) 2)) " "
   (ly:number->string blotdiameter)
   " draw_polygon"))

(define (repeat-slash wid slope thick)
  (string-append
   (ly:numbers->string (list wid slope thick))
   " draw_repeat_slash"))

;; restore color from stack
(define (resetcolor)
  (string-append "setrgbcolor\n"))

(define (round-filled-box x y width height blotdiam)
  (string-append
   (ly:numbers->string
    (list x y width height blotdiam)) " draw_round_box"))

;; save current color on stack and set new color
(define (setcolor r g b)
  (string-append "currentrgbcolor "
  (ly:numbers->string (list r g b))
  " setrgbcolor\n"))

(define (text font s)
  ;; (ly:warning (_ "TEXT backend-command encountered in Pango backend"))
  ;; (ly:warning (_ "Arguments: ~a ~a"" font str))
  
  (let* ((space-length (cdar (ly:text-dimension font " ")))
	 (space-move (string-append (number->string space-length)
				    " 0.0 rmoveto "))
	 (out-vec (decode-byte-string s)))

    (string-append
     (ps-font-command font) " setfont "
     (string-join
      (vector->list
       (vector-for-each
	
	(lambda (sym)
	  (if (eq? sym 'space)
	      space-move
	      (string-append "/" (symbol->string sym) " glyphshow")))
	out-vec))))))

(define (unknown) 
  "\n unknown\n")

(define (url-link url x y)
  (format "~a ~a ~a ~a (~a) mark_URI"
	  (car x)
	  (car y)
	  (cdr x)
	  (cdr y)
	  url))

(define (utf8-string pango-font-description string)
  (ly:warning (_ "utf8-string encountered in PS backend")))

(define (white-dot x y radius)
  (string-append
   " "
   (ly:numbers->string
    (list x y radius)) " draw_white_dot"))

(define (white-text scale s)
  (let ((mystring (string-append
		   "(" s  ") " (number->string scale)
		   " /Helvetica-Bold "
		   " draw_white_text")))
    mystring
    ))

(define (zigzag-line centre? zzw zzh thick dx dy)
  (string-append
   (if centre? "true" "false") " "
   (ly:number->string zzw) " "
   (ly:number->string zzh) " "
   (ly:number->string thick) " "
   "0 0 "
   (ly:number->string dx) " "
   (ly:number->string dy)
   " draw_zigzag_line"))
