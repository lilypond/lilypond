
\version "2.1.23"

\header {

texidoc="Bar number settable and padding adjustable.  Bar numbers
start counting after the anacrusis.

The padding should be increased, to prevent clashes at the start of the line.
 
"

}

\score {
  \notes \relative c'' {
      \partial 4 c4 
      c1 c c
      \set Score.currentBarNumber = #99999
      \override Score.BarNumber  #'padding = #3
      c1 c
  }
  \paper {
    raggedright = ##t
    \translator {
	\ScoreContext
	\override BarNumber #'break-visibility = #all-visible
    }
  }
}
