\version "1.5.68"

\header { texidoc=" You can write molecule callbacks in Scheme, thus
providing custom glyphs for notation elements.  A simple example is
adding parentheses to existing molecule callbacks.

The parenthesized beam is less successful due to implementation of the
Beam. The note head is also rather naive, since the extent of the
parens are also not seen by accidentals.
"
	
}

#(define (parenthesize-callback callback)
   "Construct a function that will do CALLBACK and add parentheses.
Example usage:

  \property Voice.NoteHead \\override #'molecule-callback
		      =
		      #(parenthesize-callback Note_head::brew_molecule)
		    
"

   
   (define (parenthesize-molecule grob)
     "This function adds parentheses to the original callback for
GROB.  The dimensions of the molecule is not affected.
"
     
     (let* (
	    (fn (ly-get-default-font grob))
	    (pclose (ly-find-glyph-by-name fn "accidentals-rightparen"))
	    (popen (ly-find-glyph-by-name fn "accidentals-leftparen"))
	    (subject (callback grob))

	    ; remember old size
	    (subject-dim-x (ly-get-molecule-extent subject 0))
	    (subject-dim-y (ly-get-molecule-extent subject 1))
	)

        ; add parens
        (set! subject
	     (ly-combine-molecule-at-edge 
	      (ly-combine-molecule-at-edge subject 0 1 pclose 0.2)
	      0 -1 popen  0.2))

	; revert old size.
       (ly-set-molecule-extent! subject 0 subject-dim-x)
       (ly-set-molecule-extent! subject 1 subject-dim-y)
       subject
    )
     )
   parenthesize-molecule
   )
    


\score {
	\notes \relative c' { c4 e

		    \property Voice.NoteHead \override #'molecule-callback
		      =
		      #(parenthesize-callback Note_head::brew_molecule)
		    g bes
		    \property Voice.NoteHead \revert #'molecule-callback
		    \property Voice.Beam \override #'molecule-callback
		      =
		      #(parenthesize-callback Beam::brew_molecule)

		    a8 gis8 a2.
		    
		    }

	\paper { linewidth = -1. }
	}

