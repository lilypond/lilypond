
(define-module (scm output-socket)
  #:re-export (quote)
  )

(use-modules (guile)
	     (srfi srfi-1)
	     (srfi srfi-13)
	     (lily))


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

(define (event-cause grob)
  (let*
      ((cause (ly:grob-property  grob 'cause)))

    (cond
     ((ly:stream-event? cause) cause)
     (else
      #f))))

(define (grob-bbox grob offset)
  (let*
      ((x-ext (ly:grob-extent grob grob X))
       (y-ext (ly:grob-extent grob grob Y))
       (x (car offset))
       (y (cdr offset)))

    (if (interval-empty? x-ext)
	(set! x-ext '(0 . 0)))

    (if (interval-empty? y-ext)
	(set! y-ext '(0 . 0)))
    
    (list (+ x (car x-ext))
	  (+ y (car y-ext))
	  (+ x (cdr x-ext))
	  (+ y (cdr y-ext))
	  )))

(define-public (no-origin)
  "nocause\n")

(define-public (grob-cause offset grob)
  (let*
      ((cause (event-cause grob))
       (tag (if (and cause (integer? (ly:event-property cause 'input-tag)))
		(ly:event-property cause 'input-tag)
		-1))
       (name (cdr (assoc 'name (ly:grob-property grob 'meta))))
       )
    
    (apply format
	   (append (list "cause ~a \"~a\" ~a ~a ~a ~a\n"
			 tag name)
	   
		   (grob-bbox grob offset))
	  )))


(define (escape-string str)
  (string-regexp-substitute
   " " "\\040" 
   (string-regexp-substitute "\"" "\\\"" str)))
  
(define-public (utf-8-string
		descr
		string)
  
  (format "utf-8 \"~a\" \"~a\""
	  (escape-string descr)

	  ;; don't want unescaped spaces.
	  (escape-string string)
	  ))


(define (bezier-sandwich lst thick)
  (format
   #f
   "bezier_sandwich ~a [~a]"
   thick
   (string-append 
    (string-join (map (lambda (x) (format "(~a,~a)" (car x) (cdr x)))
		      lst) ","))))
