
\version "2.3.16" 
\header {

texidoc = "The @code{\applyoutput} expression is the most flexible way to
tune properties for individual grobs.

Here, the layout of a note head is changed depending on its vertical
position.
"

}

#(define  (mc-squared gr org cur)
  (let*
      (
       (ifs (ly:grob-property gr 'interfaces))
       (sp (ly:grob-property gr 'staff-position))
       )
  (if (and (memq 'note-head-interface ifs)
	   (memq sp '(-2 -3 -5)))
      (begin
	(ly:grob-set-property! gr 'print-function brew-new-markup-stencil)
	(ly:grob-set-property! gr 'font-family 'roman)
	(ly:grob-set-property!
	 gr 'text
	 (make-raise-markup -0.5
			    (case sp
			      ((-5) (make-simple-markup "m"))
			      ((-3) (make-simple-markup "c "))
			      ((-2) (make-smaller-markup (make-bold-markup "2")))
			      (else (make-simple-markup "bla"))
			      ))))
      )))

\score {

 \context Voice \relative  c' {
				      \stemUp
				      \set autoBeaming =  ##f
   { <d f g b>8
     \context Voice \applyoutput #mc-squared

	 <d f g b>
   }
	      
   }
\paper { raggedright = ##t
%		     outputscale = 5
		     } 
}
