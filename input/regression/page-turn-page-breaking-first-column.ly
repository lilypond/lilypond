\version "2.21.0"
\header {
  texidoc = "Allowing the first command column to be breakable caused a crash in
Page_turn_page_breaking."
}

\book {
  \score {
    \new Staff
      \new Voice <<
	{
	  \time 4/4
	  \bar "||"			% trick Page_turn_engraver into allowing page break
	  s1*5 |
	}
	{
	  R1
	  d4
	}
      >>

    \layout {
      \set Score.skipBars = ##t
      \context {
	\Staff
	\consists "Page_turn_engraver"
      }
    }
  }

  \paper {
    page-breaking = #ly:page-turn-breaking
  }
}
