;;;; figured bass support ...

;;;; todo: make interfaces as 1st level objects in LilyPond.


(define (fontify-text font-metric text)
  "Set TEXT with font FONT-METRIC, returning a molecule."
  (let* ((b  (ly-text-dimension font-metric text)))
    (ly-make-molecule
     (ly-fontify-atom font-metric `(text ,text)) (car b) (cdr b))
    ))

(define (brew-one-figure grob fig-music)
  "Brew a single column for a music figure"
  (let* (
	 (mf (ly-get-font grob '( (font-family .  music)  )))
	 (nf (ly-get-font grob '( (font-family .  number)  )))
	 (mol (ly-make-molecule  '() '(0 . 0) '(0 . 1.0)))
	 (fig  (ly-get-mus-property fig-music 'figure))
	 (acc  (ly-get-mus-property fig-music 'alteration))
	 )
    
    (if (number? fig)
	(begin
	  (set! mol   (fontify-text nf (number->string fig)))
	  (ly-align-to! mol Y CENTER)
	))
    
    (if (number? acc)
	(set! mol
	      (ly-combine-molecule-at-edge
	       mol 0 1 (ly-find-glyph-by-name mf (string-append "accidentals-" (number->string acc)))
	       0.2))
	)
    (if (molecule? mol)
	(ly-align-to! mol X CENTER)
	)
    mol))


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

(define (brew-bass-figure grob)
  "Make a molecule for a Figured Bass grob"
  (let* (
	 (figs (ly-get-grob-property grob 'causes ))
	 (fig-mols (map (lambda (x) (brew-one-figure grob x)) figs))
	 (fig-mol (stack-molecules 1 -1 0.2 fig-mols))
	 )

    (ly-align-to! fig-mol Y DOWN)
    fig-mol
  ))

