\header {
  texidoc = "Excercise all output functions"
}

\version "2.11.53"

\paper { ragged-right = ##t }

\relative {
  \new StaffGroup \new PianoStaff <<
    \new Staff
      {
	\ottava #1
	\times 2/3 {  c'8[\< f]( f''\!)  }
	\ottava #0
	<f,, \5>4

	\override TextScript #'color = #red
	g4^"red"
      }
    
    \new Staff \relative c'' {
      \makeClusters { <g a>8 <e a> <g a>4 }
      
      \override Glissando #'style = #'zigzag
      \slurDashed
      f2( \glissando f')
      \easyHeadsOn
      f e 
    }
  >>
}
