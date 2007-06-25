#(set-global-staff-size 16)

\paper {
  %% ugh. text on toplevel is a bit broken...

  oddHeaderMarkup = \markup {}
  evenHeaderMarkup = \markup {}
  oddFooterMarkup = \markup {}
  evenFooterMarkup = \markup {}
  }

\version "2.11.20"

#(define (doc-char name)
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
	   (make-line-markup
	    (list
	     (make-pad-to-box-markup
	      '(0 . 36)
	      '(-2 . 2)
	      (make-column-markup
	       (list
		(make-typewriter-markup left)
		(make-typewriter-markup
		 (make-concat-markup
		  (list "  " right))))))
	     (make-pad-to-box-markup
	      '(-2 . 4)
	      '(-3.5 . 3.5)
	      (make-huge-markup (make-musicglyph-markup name))))))
	 (make-line-markup
	  (list
	   (make-pad-to-box-markup
	    '(0 . 36)
	    '(-2 . 2)
	    (make-typewriter-markup name))
	   (make-pad-to-box-markup
	    '(-2 . 4)
	    '(-3.5 . 3.5)
	    (make-huge-markup (make-musicglyph-markup name))))))))

#(define (min-length lst n)
   "(min	 (length lst) n)"
   (if (or (null? lst) (<= n 0))
       0
       (1+ (min-length (cdr lst) (1- n)))))

#(define (doc-chars names acc)
   (let*
       ((n (min-length names 2))
	(head (take names n))
	(tail (drop names n)))
     (if (null? head)
	 acc
	 (doc-chars tail
		    (cons
		     (make-line-markup (map doc-char head))
		     acc)))))

#(define (group-lines lines)
   (let*
       ((n (min-length lines 25))
	(head (take lines n))
	(tail (drop lines n)))
     (cons
      (make-column-markup head)
      (if (null? tail)
	  '()
	  (group-lines tail)))))

#(let*
     ((glyphs (delete ".notdef"
		      (ly:otf-glyph-list
		       (ly:font-load "emmentaler-20"))))
      (lines (doc-chars glyphs '()))
      (pages (group-lines (reverse lines))))
  (collect-scores-for-book
   parser
   (map (lambda (x)
         (make-override-markup '(word-space . 4) x))
    pages)))
