\version "2.19.24"

\header {
  texidoc = "The @code{\\applyOutput} expression is the most flexible way to
tune properties for individual grobs.

Here, the layout of a note head is changed depending on its vertical
position.
"
}

#(define (mc-squared gr org cur)
   (let ((sp (ly:grob-property gr 'staff-position)))
     (ly:grob-set-property!
      gr 'stencil
      (grob-interpret-markup gr
			     #{ \markup \raise #-0.5
				#(case sp
				   ((-5) "m")
				   ((-3) "c ")
				   ((-2) #{ \markup \teeny \bold 2 #})
				   (else "bla")) #}))))

\new Voice \relative {
  \set autoBeaming = ##f

  <d' f g b>8

  \applyOutput Voice.NoteHead #mc-squared
  <d f g b>8
}
