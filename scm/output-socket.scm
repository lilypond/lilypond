
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


(define-public (draw-line thick x1 y1 x2 y2)
  (format "drawline ~a ~a ~a ~a ~a"
	  thick x1 y2 x2 y2))


(define-public (named-glyph font glyph)
  (format "glyphshow ~a \"~a\" ~a"
	  (ly:font-glyph-name-to-charcode font glyph)
	  (ly:font-name font)
	  (modified-font-metric-font-scaling font)
	  ))

(define-public (placebox x y s) 
  (format "at ~a ~a ~a\n" x y s))

(define-public (round-filled-box  breapth width depth height blot-diameter)
  (format "draw_round_box ~a ~a ~a ~a ~a"
	  breapth width depth height blot-diameter
	  ))

(define (music-cause grob)
  (let*
      ((cause (ly:grob-property  grob 'cause)))

    (cond
     ((ly:music? cause) cause)
     ((ly:grob? cause) (music-cause cause))
     (else
      #f))))

(define (grob-bbox grob offset)
  (let*
      ((x-ext (ly:grob-extent grob grob X))
       (y-ext (ly:grob-extent grob grob Y))
       (x (car offset))
       (y (cdr offset))
       )

    (list (+ x (car x-ext))
	  (+ y (car y-ext))
	  (+ x (cdr x-ext))
	  (+ y (cdr y-ext)))
    ))

(define-public (no-origin)
  "nocause\n")

(define-public (grob-cause offset grob)
  (let*
      ((cause (music-cause grob)))
  (if (and cause (integer? (ly:music-property cause 'input-tag)))
      (apply format
	     (append
	      (list "cause ~a ~a ~a ~a ~a\n" (ly:music-property cause 'input-tag))
	      (grob-bbox grob offset)
	     ))
      "")))

(define-public (glyph-string
	 postscript-font-name
	 size cid?
	 x-y-named-glyphs)
  
  (format "text \"~a\" ~a ~a " postscript-font-name size
	  (string-join (map (lambda (xyn) (caddr xyn))
			    x-y-named-glyphs))))
