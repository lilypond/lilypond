\header {
  texidoc = "Excercise all output functions"
}

\version "2.5.0"

\paper { raggedright = ##t }

\relative {
  \new StaffGroup \new PianoStaff <<
    \new Staff <<
      {
	#(set-octavation 1)
	\times 2/3 {  c'8[\< f]( f''\!)  }
	#(set-octavation 0)
	<f \5>
      }
      \skip 1 >>
    \new Staff \relative c'' {
      \makeClusters { <g a>8 <e a> }
      \override Glissando #'style = #'zigzag
      f2 \glissando f'
      \override NoteHead #'print-function = #Note_head::brew_ez_stencil
      \override NoteHead #'Y-extent-callback = #'()
      \override NoteHead #'X-extent-callback = #'()
      f e 
    }
  >>
}
