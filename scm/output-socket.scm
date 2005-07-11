
(define-module (scm output-socket)
  #:re-export (quote)
  )

(use-modules (guile)
	     (srfi srfi-1)
	     (srfi srfi-13)
	     (lily))

(define (dummy . rest)
  "")

(for-each
 (lambda (x) 
   (module-define! (current-module)
		   x
		   dummy))
 
 (ly:all-stencil-expressions))

(define-public (named-glyph font glyph)
  (format "~a ~a glyphshow" glyph
	  (ly:font-name font)))

(define-public (placebox x y s) 
  (format "place at ~a ~a: ~a\n" x y s))

(define-public (round-filled-box x y width height blotdiam)
  (format "~a ~a ~a ~a ~a draw_round_box"
	  x y width height blotdiam
	  ))

(define-public (glyph-string
	 postscript-font-name
	 size cid?
	 x-y-named-glyphs)
  
  (format "~a ~a text: ~a " postscript-font-name size
	  (string-join (map (lambda (xyn) (caddr xyn))
			    x-y-named-glyphs))))
