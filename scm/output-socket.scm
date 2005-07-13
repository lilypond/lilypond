
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

(define-public (polygon xy-coords blot do-fill)
  (format "polygon ~a ~a ~a"
	  blot
	  (if do-fill "True" "False")
	  (string-join
	   (map number->string xy-coords))
  ))

(define-public (named-glyph font glyph)
  (format "glyphshow ~a \"~a\" ~a \"~a\""
	  (ly:font-glyph-name-to-charcode font glyph)
	  (ly:font-name font)
	  (modified-font-metric-font-scaling font)
	  glyph
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

    (map (lambda (x)
	   (if (inf? x) 0.0 x))
	 
	 (list (+ x (car x-ext))
	       (+ y (car y-ext))
	       (+ x (cdr x-ext))
	       (+ y (cdr y-ext)))
    )))

(define-public (no-origin)
  "nocause\n")

(define-public (grob-cause offset grob)
  (let*
      ((cause (music-cause grob))
       (tag (if (and cause (integer? (ly:music-property cause 'input-tag)))
		(ly:music-property cause 'input-tag)
		-1))
       (name (cdr (assoc 'name (ly:grob-property grob 'meta))))
       )
    
    (apply format
	   (append (list "cause ~a \"~a\" ~a ~a ~a ~a\n"
			 tag name)
	   
		   (grob-bbox grob offset))
	  )))


(define-public (utf-8-string
		descr
		string)
  
  (format "utf-8 \"~a\" \"~a\"" descr

	  ;; don't want unescaped spaces.
	  (string-regexp-substitute " " "\\040" string)))

