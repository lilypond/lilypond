;;;; output-ps.scm -- implement Scheme output interface for PostScript
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c)  1998--2004 Jan Nieuwenhuizen <janneke@gnu.org>
;;;;                 Han-Wen Nienhuys <hanwen@cs.uu.nl>

;;;; Note: currently misused as testbed for titles with markup, see
;;;;       input/test/title-markup.ly
;;;; 
;;;; TODO:
;;;;   * %% Papersize in (header ...)
;;;;   * text setting, kerning.
;;;;   * document output-interface

(debug-enable 'backtrace)

(define-module (scm output-ps)
  #:re-export (quote)

  ;; JUNK this -- see lily.scm: ly:all-output-backend-commands
  #:export (unknown
	     blank
	     dot
	     white-dot
	     beam
	     bracket
	     dashed-slur
	     char
	     dashed-line
	     zigzag-line
	     symmetric-x-triangle
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
	     tuplet
	     polygon
	     draw-line
	     no-origin
	     ))


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

(define (offset-add a b)
  (cons (+ (car a) (car b))
	(+ (cdr a) (cdr b))))

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
(define (bezier-sandwich l thick)
  (string-append 
   (string-join (map ly:number-pair->string l) " ")
   " "
   (ly:number->string thick)
   " draw_bezier_sandwich"))

(define (bracket arch_angle arch_width arch_height  height arch_thick thick)
  (string-append
   (ly:numbers->string
    (list arch_angle arch_width arch_height height arch_thick thick))
   " draw_bracket"))

(define (char font i)
  (string-append 
    (ps-font-command font) " setfont " 
   "(\\" (ly:inexact->string i 8) ") show" ))

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

; todo: merge with tex-font-command?

(define (embedded-ps string)
  string)

(define (dot x y radius)
  (string-append
   " "
   (ly:numbers->string
    (list x y radius)) " draw_dot"))

(define (white-dot x y radius)
  (string-append
   " "
   (ly:numbers->string
    (list x y radius)) " draw_white_dot"))

(define (draw-line thick x1 y1 x2 y2)
  (string-append 
   "1 setlinecap 1 setlinejoin "
   (ly:number->string thick) " setlinewidth "
   (ly:number->string x1) " "
   (ly:number->string y1) " moveto "
   (ly:number->string x2) " "
   (ly:number->string y2) " lineto stroke"))

(define (ez-ball ch letter-col ball-col)
  (string-append
   " (" ch ") "
   (ly:numbers->string (list letter-col ball-col))
   " /Helvetica-Bold " ;; ugh
   " draw_ez_ball"))

(define (filledbox breapth width depth height) ; FIXME : use draw_round_box
  (string-append (ly:numbers->string (list breapth width depth height))
		 " draw_box"))

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

(define (round-filled-box x y width height blotdiam)
   (string-append
    (ly:numbers->string
     (list x y width height blotdiam)) " draw_round_box"))


(define (stem breapth width depth height) ; FIXME: use draw_round_box.
  (string-append
   (ly:numbers->string (list breapth width depth height))
   " draw_box" ))

(define (symmetric-x-triangle thick w h)
  (string-append
   (ly:numbers->string (list h w thick))
   " draw_symmetric_x_triangle"))

(define (text font s)
  (let*
      
      ;; ugh, we should find a better way to
      ;; extract the hsbw for /space from the font.
      
      ((space-length (cdar (ly:text-dimension font "t"))) 
       (commands '())
       (add-command (lambda (x) (set! commands (cons x commands)))) )

    (string-fold
     (lambda (chr word)
       "Translate space as into moveto, group the rest in words."
       (if (and (< 0 (string-length word))
		(equal? #\space  chr))
	   (add-command 
	    (string-append "(" (ps-encoding word) ") show\n")))

       (if (equal? #\space chr)
	   (add-command  (string-append (number->string space-length) " 0.0 rmoveto ")) )
       
       (if (equal? #\space chr)
	   ""
	   (string-append word (make-string 1 chr))))
     ""
     (string-append s " "))

    (string-append
     (ps-font-command font) " setfont "
     (string-join (reverse commands)))
    ))


(define (white-text scale s)
   (let ((mystring (string-append "(" s  ") " (number->string scale)   " /Helvetica-bold "
          " draw_white_text")))
  mystring))

(define (unknown) 
  "\n unknown\n")

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


(define (grob-cause grob)
  "")

(define (no-origin)
  "")
