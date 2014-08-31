\version "2.19.13"

\header {
  texidoc = "The @code{VerticalAxisGroup.remove-layer}
property can be used for typesetting temporary divisi staves where
the switch to split staves is done only at line breaks such that all
complex passages are rendered in separate staves."
}

boring = \set Staff.keepAliveInterfaces = #'()
tricky = \unset Staff.keepAliveInterfaces

violI=\relative d' {
  \boring \repeat unfold 100 d4
  \tricky <d g'>2
  \boring \repeat unfold 98 d4
  \bar "|."
}

violII=\relative g {
  \boring \repeat unfold 100 g4
  \tricky <g d'>2
  \boring \repeat unfold 98 g4
  \bar "|."
}

\score {
  \new StaffGroup \with { \consists "Keep_alive_together_engraver" }
  <<
    \new Staff \with { instrumentName = "Violin I"
		       shortInstrumentName = "V I"
		       \override VerticalAxisGroup.remove-empty = ##t
		       \override VerticalAxisGroup.remove-first = ##t
		       \override VerticalAxisGroup.remove-layer = 1
		     }
    \violI
    \new Staff \with { instrumentName = "Violin II"
		       shortInstrumentName = "V II"
		       \override VerticalAxisGroup.remove-empty = ##t
		       \override VerticalAxisGroup.remove-first = ##t
		       \override VerticalAxisGroup.remove-layer = 1
		     }
    \violII
    \new Staff \with { instrumentName = "Violins"
		       shortInstrumentName = "V I&II"
		       \override VerticalAxisGroup.remove-layer = 2
		     }
    <<  \violI \\ \violII  >>
  >>
  \layout {
    short-indent = 2\cm
    indent = 3\cm
  }
}
