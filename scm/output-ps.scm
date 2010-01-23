;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 1998--2010 Jan Nieuwenhuizen <janneke@gnu.org>
;;;;                 Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;;
;;;; LilyPond is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; LilyPond is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.

;;;; Note: currently misused as testbed for titles with markup, see
;;;;       input/test/title-markup.ly
;;;; 
;;;; TODO:
;;;;   * %% Papersize in (header ...)
;;;;   * text setting, kerning.
;;;;   * document output-interface

(define-module (scm output-ps)
  #:re-export (quote))

(use-modules (guile)
	     (ice-9 regex)
	     (srfi srfi-1)
	     (srfi srfi-13)
	     (scm framework-ps)
	     (lily))

;;; helper functions, not part of output interface
;;;


;; ice-9 format uses a lot of memory
;; using simple-format almost halves lilypond cell usage

(define (str4 num)
  (if (or (nan? num) (inf? num))
      (begin
	(ly:warning (_ "Found infinity or nan in output. Substituting 0.0"))
	(if (ly:get-option 'strict-infinity-checking)
	    (exit 1))
	"0.0")
      (ly:number->string num)))

(define (number-pair->string4 numpair)
  (ly:format "~4l" numpair)) 

;;;
;;; Lily output interface, PostScript implementation --- cleanup and docme
;;;

;; two beziers
(define (bezier-sandwich lst thick)
  (ly:format "~l ~4f draw_bezier_sandwich" 
	     (map number-pair->string4 lst)
	  thick))

(define (char font i)
  (ly:format "~a (\\~a) show"
   (ps-font-command font)
   (ly:inexact->string i 8)))

(define (circle radius thick fill)
  (ly:format
   "~a ~4f ~4f draw_circle"
   (if fill
     "true"
     "false")
   radius thick))

(define (dashed-line thick on off dx dy phase)
  (ly:format "~4f ~4f ~4f [ ~4f ~4f ] ~4f draw_dashed_line"
   dx
   dy
   thick
   on
   off
   phase))

;; what the heck is this interface ?
(define (dashed-slur thick on off l)
  (ly:format "~l ~4f [ ~4f ~4f ] 0 draw_dashed_slur"
	  (let ((control-points (append (cddr l) (list (car l) (cadr l)))))
	    (map number-pair->string4 control-points))
	  thick
	  on
	  off))

(define (dot x y radius)
  (ly:format " ~4l draw_dot" (list radius x y)))

(define (draw-line thick x1 y1 x2 y2)
  (ly:format "~4f ~4f ~4f ~4f ~4f draw_line"
	  (- x2 x1) (- y2 y1)
	  x1 y1 thick))

(define (ellipse x-radius y-radius thick fill)
  (ly:format
   "~a ~4f ~4f ~4f draw_ellipse"
   (if fill
     "true"
     "false")
   x-radius y-radius thick))

(define (embedded-ps string)
  string)

(define (glyph-string postscript-font-name
		      size
		      cid?
		      w-x-y-named-glyphs)

  (define (glyph-spec w x y g)
    (let ((prefix (if (string? g) "/" "")))
      (ly:format "~4f ~4f ~4f ~a~a"
		 w x y
		 prefix g)))
  
  (ly:format 
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
  (if (ly:get-option 'point-and-click)
      (let* ((cause (ly:grob-property grob 'cause))
	     (music-origin (if (ly:stream-event? cause)
			       (ly:event-property cause 'origin))))
	(if (ly:input-location? music-origin)
	    (let* ((location (ly:input-file-line-char-column music-origin))
		   (raw-file (car location))
		   (file (if (is-absolute? raw-file)
			     raw-file
			     (string-append (ly-getcwd) "/" raw-file)))
		   (x-ext (ly:grob-extent grob grob X))
		   (y-ext (ly:grob-extent grob grob Y)))

	      (if (and (< 0 (interval-length x-ext))
		       (< 0 (interval-length y-ext)))
		  (ly:format "~4f ~4f ~4f ~4f (textedit://~a:~a:~a:~a) mark_URI\n"
			     (+ (car offset) (car x-ext))
			     (+ (cdr offset) (car y-ext))
			     (+ (car offset) (cdr x-ext))
			     (+ (cdr offset) (cdr y-ext))

			     ;; TODO
			     ;;full escaping.

			     ;; backslash is interpreted by GS.
			     (ly:string-substitute "\\" "/" 
						   (ly:string-substitute " " "%20" file))
			     (cadr location)
			     (caddr location)
			     (cadddr location))
		  ""))
	    ""))
      ""))

(define (named-glyph font glyph)
  (ly:format "~a /~a glyphshow " ;;Why is there a space at the end?
	     (ps-font-command font)
	     glyph))

(define (no-origin)
  "")

(define (oval x-radius y-radius thick fill)
  (ly:format
   "~a ~4f ~4f ~4f draw_oval"
   (if fill
     "true"
     "false")
   x-radius y-radius thick))

(define (placebox x y s) 
  (if (not (string-null? s))
      (ly:format "~4f ~4f moveto ~a\n" x y s)
      ""))

(define (polygon points blot-diameter filled?)
  (ly:format "~a ~4l ~a ~4f draw_polygon"
	     (if filled? "true" "false")
	     points
	     (- (/ (length points) 2) 1)
	     blot-diameter))

(define (repeat-slash width slope beam-thickness)
  (define (euclidean-length x y)
    (sqrt (+ (* x x) (* y y))))

  (let ((x-width (euclidean-length beam-thickness (/ beam-thickness slope)))
	(height (* width slope)))
    (ly:format "~4l draw_repeat_slash"
	     (list x-width width height))))


(define (round-filled-box left right bottom top blotdiam)
  (let* ((halfblot (/ blotdiam 2))
	 (x (- halfblot left))
	 (width (- right (+ halfblot x)))
	 (y (- halfblot bottom))
	 (height (- top (+ halfblot y))))
    (ly:format  "~4l draw_round_box"
		(list width height x y blotdiam))))

;; save current color on stack and set new color
(define (setcolor r g b)
  (ly:format "gsave ~4l setrgbcolor\n"
	      (list r g b)))

;; restore color from stack
(define (resetcolor) "grestore\n")

;; rotation around given point
(define (setrotation ang x y)
  (ly:format "gsave ~4l translate ~a rotate ~4l translate\n"
	     (list x y)
	     ang
	     (list (* -1 x) (* -1 y))))

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
  (ly:format "~a ~a currentpoint vector_add  ~a ~a currentpoint vector_add (~a) mark_URI"
	     (car x)
	     (car y)
	     (cdr x)
	     (cdr y)
	     url))

(define (path thickness exps)
  (define (convert-path-exps exps)
    (if (pair? exps)
	(let*
	    ((head (car exps))
	     (rest (cdr exps))
	     (arity 
	      (cond
	       ((memq head '(rmoveto rlineto lineto moveto)) 2)
	       ((memq head '(rcurveto curveto)) 6)
	       ((eq? head 'closepath) 0)
	       (else 1)))
	     (args (take rest arity))
	     )

	  ;; WARNING: this is a vulnerability: a user can output arbitrary PS code here.
	  (cons (ly:format
			"~l ~a "
			args 
			head)
		(convert-path-exps (drop rest arity))))
	'()))
    
    
  (ly:format
   "gsave currentpoint translate 1 setlinecap ~a setlinewidth\n~l stroke grestore"
   thickness
   (convert-path-exps exps)))
  
