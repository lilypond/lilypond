

\paper {
  %% ugh. text on toplevel is a bit broken... .
  
  oddHeaderMarkup = \markup {}
  evenHeaderMarkup = \markup {}
  oddFooterMarkup = \markup {}
  evenFooterMarkup = \markup {}
  }

#(define (doc-char name)
  (make-line-markup
   (list
    (make-pad-to-box-markup
     '(0 . 30)
     '(-2 . 2)
     (make-typewriter-markup (make-small-markup name)))
    (make-pad-to-box-markup
     '(-2 . 2)
     '(-2 . 2)
     (make-musicglyph-markup name)))))

#(define (min-length lst n)
  "(min  (length lst) n)"
  
  (if (or (null? lst) (<= n 0))
   0
   (1+ (min-length (cdr lst) (1- n)) )))

#(define (doc-chars names acc)
  (let*
   ((n (min-length names 2))
    (head (take names n))
    (tail (drop names n))
    )

   (if (null? head)
    acc
    (doc-chars  tail
     (cons (make-line-markup (map doc-char head)) acc)))
     ))

#(define (group-lines lines)
  (let*
   ((n (min-length lines 25))
    (head (take lines n))
    (tail (drop lines n))
    )

   (cons
    (make-column-markup head)
    (if (null? tail)
     '()
     (group-lines tail)))))
			
#(let*
  ((lines (doc-chars
	   (ly:font-glyph-list (format  "~a/fonts/otf/emmentaler-20.otf"
				(getenv "LILYPONDPREFIX")
			      ))
	 '()))
   (pages (group-lines (reverse lines))))

  (for-each 
   (lambda (x)
    (collect-scores-for-book parser
     (make-override-markup '(word-space . 8) x)))
			pages))


