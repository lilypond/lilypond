
(define-public (stack-molecules axis dir padding mols)
  "Stack molecules MOLS in direction AXIS,DIR, using PADDING."
  (if (null? mols)
      '()
      (if (pair? mols)
	  (ly:molecule-combine-at-edge (car mols) axis dir 
				       (stack-molecules axis dir padding (cdr mols))
				       padding
				       )
	  )
  ))


(define-public (stack-lines dir padding baseline mols)
  "Stack vertically with a baseline-skip."
  (if (null? mols)
      '()
      (if (null? (cdr mols))
	  (car mols)
	  (ly:molecule-combine-at-edge (car mols) Y dir 
				       (stack-lines dir padding baseline (cdr mols))
				       padding baseline
				       )
	  )
  ))

(define-public (fontify-text font-metric text)
  "Set TEXT with font FONT-METRIC, returning a molecule."
  (let* ((b  (ly:text-dimension font-metric text)))
    (ly:make-molecule
     (ly:fontify-atom font-metric `(text ,text)) (car b) (cdr b))
    ))

(define-public (bracketify-molecule mol axis thick protusion padding)
  "Add brackets around MOL, producing a new molecule."

  (let* (
	 (ext (ly:molecule-get-extent mol axis))
	 (lb (ly:bracket axis ext thick (- protusion)))
	 (rb (ly:bracket axis ext thick protusion))
	 )
    (set! mol (ly:molecule-combine-at-edge mol (other-axis  axis) 1 lb padding))
    (set! mol (ly:molecule-combine-at-edge mol (other-axis  axis) -1 rb padding))
    mol
  ))

(define-public (box-molecule xext yext)
  "Make a filled box."
  
  (ly:make-molecule
      (list 'filledbox (- (car xext)) (cdr xext)
                       (- (car yext)) (cdr yext))
      xext yext)		       
)


(define-public (box-grob-molecule grob)
  "Make a box of exactly the extents of the grob.  The box precisely
encloses the contents.
"
  (let* ((xext (ly:get-extent grob grob 0))
	 (yext (ly:get-extent grob grob 1))
	 (thick 0.1))

    (ly:molecule-add (box-molecule xext (cons (- (car yext) thick) (car yext) ))
		     (box-molecule xext (cons  (cdr yext) (+ (cdr yext) thick) ))
		     (box-molecule (cons (cdr xext) (+ (cdr xext) thick)) yext)
		     (box-molecule (cons (- (car xext) thick) (car xext)) yext))))


;; TODO merge this and prev function. 
(define-public (box-molecule mol thick padding)
  "Add a box around MOL, producing a new molecule."
  (let* (
	 (x-ext (widen-interval (ly:molecule-get-extent mol 0) padding))
	 (y-ext (widen-interval (ly:molecule-get-extent mol 1) padding))
	 (x-rule (box-molecule (widen-interval x-ext thick)
			       (cons 0 thick)))
	 (y-rule (box-molecule (cons 0 thick) y-ext)))
    
    (set! mol (ly:molecule-combine-at-edge mol 0 1 y-rule (* 0.5 padding)))
    (set! mol (ly:molecule-combine-at-edge mol 0 -1  y-rule (* 0.5 padding)))
    (set! mol (ly:molecule-combine-at-edge mol 1 1  x-rule 0.0))  
    (set! mol (ly:molecule-combine-at-edge mol 1 -1 x-rule 0.0))
    
    mol))
