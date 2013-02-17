\header {
  texidoc = "Exercise all output functions"
}

\version "2.17.11"

\paper { ragged-right = ##t }

\relative c'' {
  \new StaffGroup \new PianoStaff <<
    \new Staff
      {
	\ottava #1
	\tuplet 3/2 { c8[\< f]( f''\!) }
	\ottava #0
	<f,, \5>4

	\override TextScript.color = #red
	g4^"red"
      }

    \new Staff \relative c'' {
      \makeClusters { <g a>8 <e a> <g a>4 }

      \override Glissando.style = #'zigzag
      \slurDashed
      f2( \glissando f')
      \easyHeadsOn
      f e
    }
  >>
}
