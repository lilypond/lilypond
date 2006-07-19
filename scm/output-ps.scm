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
	    bezier-sandwich
	    char
	    circle
	    comment
	    dashed-line
	    dashed-slur
	    dot
	    draw-line
	    embedded-ps
	    named-glyph
	    no-origin
	    placebox
	    polygon
	    repeat-slash
	    resetcolor
	    resetrotatino
	    round-filled-box
	    setcolor
		setrotation
	    text
	    zigzag-line))


(use-modules (guile)
	     (ice-9 regex)
	     (srfi srfi-1)
	     (srfi srfi-13)
	     (scm framework-ps)
	     (lily))

;;; helper functions, not part of output interface
;;;


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
   "~a ~f ~f draw_circle"
   (if fill
     "true"
     "false")
   (round4 radius) (round4 thick)))

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
	  (let ((control-points (append (cddr l) (list (car l) (cadr l)))))
	    (string-join (map number-pair->string4 control-points) " "))
	  (str4 thick)
	  (str4 on)
	  (str4 off)))

(define (dot x y radius)
  (format #f " ~a draw_dot"
   (numbers->string4 (list radius x y))))

(define (draw-line thick x1 y1 x2 y2)
  (format #f "~a ~a ~a ~a ~a draw_line"
	  (str4 (- x2 x1))
	  (str4 (- y2 y1))
	  (str4 x1)
	  (str4 y1)
	  (str4 thick)))

(define (embedded-ps string)
  string)

(define (glyph-string postscript-font-name
		      size
		      cid?
		      w-x-y-named-glyphs)

  (define (glyph-spec w x y g)
    (let ((prefix (if (string? g) "/" "")))
      (format #f "~f ~f ~a~a"
	      (round2 (+ w x))
	      (round2 y)
	      prefix g)))
  
  (format #f
	  (if cid?
"/~a /CIDFont findresource ~a output-scale div scalefont setfont
~a
~a print_glyphs"

"/~a ~a output-scale div selectfont
~a
~a print_glyphs")
	  postscript-font-name
	  size
	  (string-join (map (lambda (x) (apply glyph-spec x))
			    (reverse w-x-y-named-glyphs)) "\n")
	  (length w-x-y-named-glyphs)))


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
      (format "/~a { ~a } bind def\n" key val)
      (format "/~a (~a) def\n" key val))))

(define (named-glyph font glyph)
  (format #f "~a /~a glyphshow " ;;Why is there a space at the end?
	  (ps-font-command font)
	  glyph))

(define (no-origin)
  "")

(define (placebox x y s) 
  (format #f
"~a ~a moveto
~a\n"
  (str4 x)
  (str4 y)
  s))

(define (polygon points blot-diameter filled?)
  (format #f "~a ~a ~a ~a draw_polygon"
	  (if filled? "true" "false")
	  (numbers->string4 points)
	  (number->string (- (/ (length points) 2) 1))
	  (str4 blot-diameter)))

(define (repeat-slash width slope beam-thickness)
  (define (euclidean-length x y)
    (sqrt (+ (* x x) (* y y))))

  (let ((x-width (euclidean-length beam-thickness (/ beam-thickness slope)))
	(height (* width slope)))
    (format #f "~a draw_repeat_slash"
	    (numbers->string4 (list x-width width height)))))

;; restore color from stack
(define (resetcolor) "setrgbcolor\n")


(define (round-filled-box left right bottom top blotdiam)
  (let* ((halfblot (/ blotdiam 2))
	 (x (- halfblot left))
	 (width (- right (+ halfblot x)))
	 (y (- halfblot bottom))
	 (height (- top (+ halfblot y))))
    (format #f "~a draw_round_box"
	    (numbers->string4
	      (list width height x y blotdiam)))))

;; save current color on stack and set new color
(define (setcolor r g b)
  (format #f "currentrgbcolor ~a setrgbcolor\n"
	  (numbers->string4 (list r g b))))

;; rotation around given point
(define (setrotation ang x y)
  (format "gsave ~a translate ~a rotate ~a translate\n"
    (numbers->string4 (list x y))
    (number->string ang)
    (numbers->string4 (list (* -1 x) (* -1 y)))))

(define (resetrotation ang x y)
  "grestore  ")


(define (text font s)
  ;; (ly:warning (_ "TEXT backend-command encountered in Pango backend"))
  ;; (ly:warning (_ "Arguments: ~a ~a"" font str))
  
  (let* ((space-length (cdar (ly:text-dimension font " ")))
	 (space-move (string-append (number->string space-length)
				    ;; how much precision do we need here?
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


(define (path thickness exps)
  (define  (path-exps->ps-path-exps exps)
    (if (pair? exps)
	(let*
	    ((head (car exps))
	     (rest (cdr exps))
	     (arity 
	      (cond
	       ((memq head '(rmoveto rlineto lineto moveto)) 2)
	       ((memq head '(rcurveto curveto)) 6)
	       (else 1)))
	     (args (take rest arity))
	     )

	  ;; WARNING: this is a vulnerability: a user can output arbitrary PS code here.
	  (cons (format "~a ~a "
			(string-join (map (lambda (x) (format "~a " x)) args) " ")
			head)
		(path-exps->ps-path-exps (drop rest arity))))
	'()))
    
    
  (format
   "1 setlinecap ~a setlinewidth\n~a stroke"
   thickness
   (string-join (path-exps->ps-path-exps exps) " ")))
  
