;;;; figured bass support ...

;;;; todo: make interfaces as 1st level objects in LilyPond.

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



(define (brew-bass-figure grob)
  "Make a molecule for a Figured Bass grob"
  (let* (
	 (figs (ly-get-grob-property grob 'causes ))
	 (mol (ly-make-molecule '() '(0 . 0) '(0 . 0)))
	 (padding (ly-get-grob-property grob 'padding))
	 (kerning (ly-get-grob-property grob 'kern))
	 (thickness (*
		     (ly-get-paper-variable grob 'stafflinethickness)
		     (ly-get-grob-property grob 'thickness))
		    )
	 )



    (define (brew-complete-figure grob figs mol)
      "recursive function: take some stuff from FIGS, and add it to MOL." 
      (define (end-bracket? fig)
	(eq? (ly-get-mus-property fig 'bracket-stop) #t)
	)
      
      (if (null? figs)
	  mol
	  (if (eq? (ly-get-mus-property (car figs) 'bracket-start) #t)
	      (let* (
		     (gather-todo (take-from-list-until figs '() end-bracket?))
		     (unbr-mols
		      (map
		       (lambda (x) (brew-one-figure grob x))
		       (reverse! (car gather-todo) '())))
		     (br-mol (bracketify-molecule
			      (stack-molecules Y UP kerning unbr-mols)
			      Y thickness (* 2 padding) padding))
		     )
		(brew-complete-figure
		 grob (cdr gather-todo)
		 (ly-combine-molecule-at-edge mol Y UP br-mol kerning)
		 )
		)
	      (brew-complete-figure
	       grob (cdr figs)
	       (ly-combine-molecule-at-edge mol Y UP (brew-one-figure grob (car figs))
					    kerning))
	      )
	  ))

    
    (set! mol (brew-complete-figure grob (reverse figs) mol))
    (ly-align-to! mol Y DOWN)
    mol
    ))

