#(set-global-staff-size 16)

\paper {
  %% ugh. text on toplevel is a bit broken...

  oddHeaderMarkup = \markup {}
  evenHeaderMarkup = \markup {}
  oddFooterMarkup = \markup {}
  evenFooterMarkup = \markup {}
  }

\version "2.11.61"

#(define-markup-command (doc-char layout props name) (string?)
  (interpret-markup layout props
   (let* ((n (string-length name)))
     (if (> n 24)
	 ;; split long glyph names near the middle at dots
	 (let* ((middle-pos (round (/ n 2)))
		(left-dot-pos (string-rindex name #\. 0 middle-pos))
		(right-dot-pos (string-index name #\. middle-pos))
		(left-distance (if (number? left-dot-pos)
				   (- middle-pos left-dot-pos)
				   middle-pos))
		(right-distance (if (number? right-dot-pos)
				    (- right-dot-pos middle-pos)
				    middle-pos))
		(split-pos (if (> left-distance right-distance)
			       right-dot-pos
			       left-dot-pos))
		(left (substring name 0 split-pos))
		(right (substring name split-pos)))
	   (markup
	     #:pad-to-box '(0 . 36) '(-2 . 2) #:column (#:typewriter left
							#:typewriter #:concat ("  " right))
	     #:pad-to-box '(-2 . 4) '(-3.5 . 3.5) #:huge #:musicglyph name))
	 (markup
	   #:pad-to-box '(0 . 36) '(-2 . 2) #:typewriter name
	   #:pad-to-box '(-2 . 4) '(-3.5 . 3.5) #:huge #:musicglyph name)))))

#(define-markup-list-command (doc-chars layout props names) (list?)
   (define (min-length lst n)
     "(min	 (length lst) n)"
     (if (or (null? lst) (<= n 0))
	 0
	 (1+ (min-length (cdr lst) (1- n)))))
   (define (doc-chars-aux names acc)
     (let* ((n (min-length names 2))
	    (head (take names n))
	    (tail (drop names n)))
       (if (null? head)
	   (reverse! acc)
	   (doc-chars-aux tail
			 (cons (make-line-markup (map make-doc-char-markup head))
			       acc)))))
   (interpret-markup-list layout props (doc-chars-aux names (list))))

\markuplines \override-lines #'(word-space . 4)
             \doc-chars #(delete ".notdef"
                          (ly:otf-glyph-list (ly:font-load "emmentaler-20")))
