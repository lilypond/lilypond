;;;; output-ps.scm -- implement Scheme output interface for PostScript
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 1998--2006 Jan Nieuwenhuizen <janneke@gnu.org>
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
	    dashed-slur
	    char
	    setcolor
	    resetcolor
	    named-glyph
	    dashed-line
	    zigzag-line
	    comment
	    repeat-slash
	    placebox
	    bezier-sandwich
	    embedded-ps
	    round-filled-box
	    text
	    polygon
	    draw-line
	    no-origin))


(use-modules (guile)
	     (ice-9 regex)
	     (srfi srfi-1)
	     (srfi srfi-13)
	     (scm framework-ps)
	     (lily))

;;; helper functions, not part of output interface
(define (escape-parentheses s)
  (regexp-substitute/global #f "(^|[^\\])([\\(\\)])" s 'pre 1 "\\" 2 'post))

(define (ps-encoding text)
  (escape-parentheses text))

(define (round2 num)
  (/ (round (* 100 num)) 100))

(define (round4 num)
  (/ (round (* 10000 num)) 10000))

(define (str4 num)
  (format #f "~f" (round4 num)))

(define (number-pair->string4 numpair)
  (format #f "~f ~f" (round4 (car numpair)) (round4 (cdr numpair))))

(define (numbers->string4 numlist)
  (string-join (map str4 numlist) " "))

;; FIXME: lily-def
(define-public (ps-string-def prefix key val)
  (format #f "/ ~a~a (~a) def\n"
	  prefix
	  (symbol->string key)
	  (escape-parentheses val)))

(define (ps-number-def prefix key val)
  (let ((s (if (integer? val)
	       (ly:number->string val)
	       (ly:number->string (exact->inexact val)))))
    (format #f "/~a~a ~a def\n"
	    prefix
	    (symbol->string key) s)))


;;;
;;; Lily output interface, PostScript implementation --- cleanup and docme
;;;

;; two beziers
(define (bezier-sandwich lst thick)
  (format #f "~a ~a draw_bezier_sandwich" 
	  (string-join (map number-pair->string4 lst) " ")
	  (str4 thick)))

(define (char font i)
  (format #f "~a (\\~a) show"
   (ps-font-command font)
   (ly:inexact->string i 8)))

(define (circle radius thick fill)
  (format #f
   "~f ~f ~a draw_circle" (round4 radius) (round4 thick)
   (if fill
       "true "
       "false ")))

(define (dashed-line thick on off dx dy)
  (format #f "~a ~a ~a [ ~a ~a ] 0 draw_dashed_line"
   (str4 dx)
   (str4 dy)
   (str4 thick)
   (str4 on)
   (str4 off)))

;; what the heck is this interface ?
(define (dashed-slur thick on off l)
  (format #f "~a ~a [ ~a ~a ] 0 draw_dashed_slur"
	  (string-join (map number-pair->string4 l) " ")
	  (str4 thick)
	  (str4 on)
	  (str4 off)))

(define (dot x y radius)
  (format #f " ~a draw_dot"
   (numbers->string4 (list x y radius))))

(define (draw-line thick x1 y1 x2 y2)
  (format #f "1 setlinecap 1 setlinejoin ~a setlinewidth ~a ~a moveto ~a ~a lineto stroke"
   (str4 thick)
   (str4 x1)
   (str4 y1)
   (str4 x2)
   (str4 y2)))

(define (embedded-ps string)
  string)

(define (glyph-string postscript-font-name
		      size
		      cid?
		      w-x-y-named-glyphs)

  (format #f "gsave
  /~a ~a ~a output-scale div scalefont setfont\n~a grestore"
	  postscript-font-name

	  ;; with normal findfont, GS throws /typecheck for glyphshow.
	  (if cid?
	      " /CIDFont findresource "
	      " findfont")
	  size
	  (string-append
	    (apply
	      string-append
	      (map (lambda  (item)
		     (let*
		       ((w (car item))
			(x (cadr item))
			(y (caddr item))
			(g (cadddr item))
			(prefix (if  (string? g) "/" "")))

		       (format #f "~f ~f ~a~a\n" (round2 (+ w x))
			       (round2 y) prefix g)
		       ))
		   w-x-y-named-glyphs))
	    (format #f "~a print_glyphs" (length w-x-y-named-glyphs)))
	  ))

(define (grob-cause offset grob)
  (let* ((cause (ly:grob-property grob 'cause))
	 (music-origin (if (ly:music? cause)
			   (ly:music-property cause 'origin))))
    (if (not (ly:input-location? music-origin))
	""
	(let* ((location (ly:input-file-line-char-column music-origin))
	       (raw-file (car location))
	       (file (if (is-absolute? raw-file)
			 raw-file
			 (string-append (ly-getcwd) "/" raw-file)))
	       (x-ext (ly:grob-extent grob grob X))
	       (y-ext (ly:grob-extent grob grob Y)))

	  (if (and (< 0 (interval-length x-ext))
		   (< 0 (interval-length y-ext)))
	      (format #f "~$ ~$ ~$ ~$ (textedit://~a:~a:~a:~a) mark_URI\n"
		      (+ (car offset) (car x-ext))
		      (+ (cdr offset) (car y-ext))
		      (+ (car offset) (cdr x-ext))
		      (+ (cdr offset) (cdr y-ext))

		      ;; TODO
		      ;;full escaping.

		      ;; backslash is interpreted by GS.
		      (string-regexp-substitute "\\\\" "/" 
				      (string-regexp-substitute " " "%20" file))
		      (cadr location)
		      (caddr location)
		      (cadddr location))
	      "")))))

(define (lily-def key val)
  (let ((prefix "lilypondlayout"))
    (if (string=?
	  (substring key 0 (min (string-length prefix) (string-length key)))
	  prefix)
      (string-append "/" key " {" val "} bind def\n")
      (string-append "/" key " (" val ") def\n"))))

(define (named-glyph font glyph)
  (format #f "~a /~a glyphshow " ;;Why is there a space at the end?
	  (ps-font-command font)
	  glyph))

(define (no-origin)
  "")

(define (placebox x y s) 
  (format #f
"gsave ~a ~a translate
0 0 moveto
~a
grestore\n"

   (str4 x)
   (str4 y)
   s))

(define (polygon points blot-diameter filled?)
  (format #f "~a ~a ~a ~a draw_polygon"
	  (numbers->string4 points)
	  (str4 (/ (length points) 2))
	  (str4 blot-diameter)
	  (if filled? "true" "false")))

(define (repeat-slash wid slope thick)
  (format #f "~a draw_repeat_slash"
   (numbers->string4 (list wid slope thick))))

;; restore color from stack
(define (resetcolor) "setrgbcolor\n")

(define (round-filled-box x y width height blotdiam)
  (format #f "~a draw_round_box"
	  (numbers->string4
	    (list x y width height blotdiam))))

;; save current color on stack and set new color
(define (setcolor r g b)
  (format #f "currentrgbcolor ~a setrgbcolor\n"
	  (numbers->string4 (list r g b))))

(define (text font s)
  ;; (ly:warning (_ "TEXT backend-command encountered in Pango backend"))
  ;; (ly:warning (_ "Arguments: ~a ~a"" font str))
  
  (let* ((space-length (cdar (ly:text-dimension font " ")))
	 (space-move (string-append (number->string space-length)
				    " 0.0 rmoveto "))
	 (out-vec (decode-byte-string s)))

    (string-append
     (ps-font-command font) " "
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
  (format #f "~$ ~$ ~$ ~$ (~a) mark_URI"
	  (car x)
	  (car y)
	  (cdr x)
	  (cdr y)
	  url))

(define (utf-8-string pango-font-description string)
  (ly:warning (_ "utf-8-string encountered in PS backend")))


(define (zigzag-line centre? zzw zzh thick dx dy)
  (format #f "~a ~a ~a ~a 0 0 ~a ~a draw_zigzag_line"
   (if centre? "true" "false")
   (str4 zzw)
   (str4 zzh)
   (str4 thick)
   (str4 dx)
   (str4 dy)))
