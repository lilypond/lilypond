\version "1.7.18"

\header {

texidoc="Bar number settable and padding adjustable.  Bar numbers
start counting after the anacrusis."

}

\score {
  \notes \relative c'' {
      \partial 4 c4 
      c1 c c
      \property Score.currentBarNumber = #99999
      \property Score.BarNumber \override #'padding = #3
      c1 c
  }
  \paper {
    raggedright = ##t
    \translator {
	\ScoreContext
	BarNumber \override #'break-visibility = #all-visible
    }
  }
}
