;;;; figured bass support ...

(ly:add-interface
'bass-figure-interface
 "A bass figure, including bracket"
 '())

(define  (recursive-split-at pred? l)
  (if (null? l)
      '()
      (let*
	  ((x (split-at-predicate pred? l)))
	(set-cdr! x (recursive-split-at pred? (cdr x)))
	x
	)))

(define-public (make-bass-figure-markup figures context)
  
  (define (no-end-bracket? f1 f2)
    (eq? (ly:music-property f1 'bracket-stop) '())
    )
  (define (no-start-bracket? f1 f2)
    (eq? (ly:music-property f2 'bracket-start) '())
    )

  ;; TODO: support slashed numerals here.
  (define (fig-to-markup fig-music)
    (let*
	((align-accs (eq? #t (ly:context-property context 'alignBassFigureAccidentals)))
	 (fig  (ly:music-property fig-music 'figure))
	 (acc  (ly:music-property fig-music 'alteration))
	 (acc-markup #f)
	 (fig-markup
	  (if (string? fig)
	      (make-simple-markup fig)
	      (if align-accs (make-simple-markup " ")
		  (if (not (eq? acc '()))
		      (make-simple-markup "")
		      (make-strut-markup)))
	      )))

      (if (number? acc)
	  (make-line-markup (list fig-markup
				  (alteration->text-accidental-markup acc)))
	  fig-markup)
      ))
  
  (define (fig-seq-to-markup figs)
    (let*
	(
	 (c (make-dir-column-markup (map fig-to-markup figs)))
	 )
      (if (eq? (ly:music-property (car figs) 'bracket-start) #t)
	  (make-bracket-markup c)
	  c
	  )))
  
  (let*
      (
       (ends (recursive-split-at no-end-bracket? (reverse figures)))
       (starts (map (lambda (x) (recursive-split-at no-start-bracket? x)) ends))
       )
    (make-dir-column-markup (map fig-seq-to-markup (apply append starts)))
    ))

