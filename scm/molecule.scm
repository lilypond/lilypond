
(define (stack-molecules axis dir padding mols)
  "Stack molecules MOLS in direction AXIS,DIR, using PADDING."
  (if (null? mols)
      '()
      (if (pair? mols)
	  (ly-combine-molecule-at-edge (car mols) axis dir 
				       (stack-molecules axis dir padding (cdr mols))
				       padding
				       )
	  )
  ))




(define (fontify-text font-metric text)
  "Set TEXT with font FONT-METRIC, returning a molecule."
  (let* ((b  (ly-text-dimension font-metric text)))
    (ly-make-molecule
     (ly-fontify-atom font-metric `(text ,text)) (car b) (cdr b))
    ))

(define (other-axis a)
  (remainder (+ a 1) 2))
  
(define (bracketify-molecule mol axis thick protusion padding)
  "Add brackets around MOL, producing a new molecule."

  (let* (
	 (ext (ly-get-molecule-extent mol axis))
	 (lb (ly-bracket axis ext -1 thick protusion))
	 (rb (ly-bracket axis ext 1 thick protusion))
	 )
    (set! mol (ly-combine-molecule-at-edge mol (other-axis  axis) 1 lb padding))
    (set! mol (ly-combine-molecule-at-edge mol (other-axis  axis) -1 rb padding))
    mol
  ))
