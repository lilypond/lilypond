\version "2.19.21"

\header { texidoc=" You can write stencil callbacks in Scheme, thus
providing custom glyphs for notation elements.  A simple example is
adding parentheses to existing stencil callbacks.

The parenthesized beam is less successful due to implementation of the
Beam. The note head is also rather naive, since the extent of the
parens are also not seen by accidentals.
"
	
}

%% Silence a warning about missing font-property, if compiled with the
%% check-internal-types option.
#(if (ly:get-option 'check-internal-types)
     (ly:expect-warning "Grob `Beam' has no interface for property"))

#(define (parenthesize-callback callback)
   "Construct a function that will do CALLBACK and add parentheses.
Example usage:

  \\override NoteHead.stencil
  =
  #(parenthesize-callback ly:note-head::print)"
   
   (define (parenthesize-stencil grob)
     "This function adds parentheses to the original callback for
GROB.  It does not affect the dimensions of the stencil.
"
     
     (let* ((fn (ly:grob-default-font grob))
	    (pclose (ly:font-get-glyph fn "accidentals.rightparen"))
	    (popen (ly:font-get-glyph fn "accidentals.leftparen"))
	    (subject (callback grob))

	    ; remember old size
	    (subject-dim-x (ly:stencil-extent subject X))
	    (subject-dim-y (ly:stencil-extent subject Y)))

        ;; add parens
        (set! subject
	     (ly:stencil-combine-at-edge 
	      (ly:stencil-combine-at-edge subject X RIGHT pclose 0.2)
	      X LEFT popen  0.2))

	; revert old size.
       (ly:make-stencil
        (ly:stencil-expr subject) subject-dim-x subject-dim-y)))
   parenthesize-stencil)
    

\layout { ragged-right = ##t }
\relative {
    c'4 e

    \override NoteHead.stencil
    =
    #(parenthesize-callback ly:note-head::print)
    g bes
    \revert NoteHead.stencil

    \override Beam.stencil
    =
    #(parenthesize-callback ly:beam::print)

    a8 gis8 a2.
    
}



