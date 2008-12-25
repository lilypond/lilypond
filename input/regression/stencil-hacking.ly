
\version "2.12.0"

\header { texidoc=" You can write stencil callbacks in Scheme, thus
providing custom glyphs for notation elements.  A simple example is
adding parentheses to existing stencil callbacks.

The parenthesized beam is less successful due to implementation of the
Beam. The note head is also rather naive, since the extent of the
parens are also not seen by accidentals.
"
	
}

#(define (parenthesize-callback callback)
   "Construct a function that will do CALLBACK and add parentheses.
Example usage:

  \\property NoteHead \\override #'print-function
		   =
		      #(parenthesize-callback ly:note-head::print)
		    
"

   
   (define (parenthesize-stencil grob)
     "This function adds parentheses to the original callback for
GROB.  The dimensions of the stencil is not affected.
"
     
     (let* ((fn (ly:grob-default-font grob))
	    (pclose (ly:font-get-glyph fn "accidentals.rightparen"))
	    (popen (ly:font-get-glyph fn "accidentals.leftparen"))
	    (subject (callback grob))

	    ; remember old size
	    (subject-dim-x (ly:stencil-extent subject 0))
	    (subject-dim-y (ly:stencil-extent subject 1)))

        ;; add parens
        (set! subject
	     (ly:stencil-combine-at-edge 
	      (ly:stencil-combine-at-edge subject 0 1 pclose 0.2)
	      0 -1 popen  0.2))

	; revert old size.
       (ly:make-stencil
        (ly:stencil-expr subject) subject-dim-x subject-dim-y)))
   parenthesize-stencil)
    

\layout { ragged-right = ##t }
\relative c' {
    c4 e

    \override NoteHead  #'stencil
    =
    #(parenthesize-callback ly:note-head::print)
    g bes
    \revert NoteHead #'stencil

    \override Beam  #'stencil
    =
    #(parenthesize-callback ly:beam::print)

    a8 gis8 a2.
    
}



