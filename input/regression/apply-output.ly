
\version "2.1.25" 
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
       (ifs (ly:get-grob-property gr 'interfaces))
       (sp (ly:get-grob-property gr 'staff-position))
       )
  (if (and (memq 'note-head-interface ifs)
	   (memq sp '(-2 -3 -5)))
      (begin
	(ly:set-grob-property! gr 'print-function brew-new-markup-stencil)
	(ly:set-grob-property! gr 'font-family 'roman)
	(ly:set-grob-property!
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

\notes \context Voice \relative  c' {
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
