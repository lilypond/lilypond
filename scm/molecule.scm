
(define-public (stack-molecules axis dir padding mols)
  "Stack molecules MOLS in direction AXIS,DIR, using PADDING."
  (if (null? mols)
      '()
      (if (pair? mols)
	  (ly:combine-molecule-at-edge (car mols) axis dir 
				       (stack-molecules axis dir padding (cdr mols))
				       padding
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
	 (ext (ly:get-molecule-extent mol axis))
	 (lb (ly:bracket axis ext thick (- protusion)))
	 (rb (ly:bracket axis ext thick protusion))
	 )
    (set! mol (ly:combine-molecule-at-edge mol (other-axis  axis) 1 lb padding))
    (set! mol (ly:combine-molecule-at-edge mol (other-axis  axis) -1 rb padding))
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
	 (mol (ly:make-molecule '() '(10000 . -10000) '(10000 . -10000)))
	 (thick 0.1)
	 )

    (set! mol (ly:add-molecule mol (box-molecule xext (cons (- (car yext) thick) (car yext) ))))
    (set! mol (ly:add-molecule mol (box-molecule xext (cons  (cdr yext) (+ (cdr yext) thick) ))))
    (set! mol (ly:add-molecule mol (box-molecule (cons (cdr xext) (+ (cdr xext) thick)) yext)))
    (set! mol (ly:add-molecule mol (box-molecule (cons (- (car xext) thick) (car xext)) yext)))
    mol
  ))
