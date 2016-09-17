\version "2.19.48"

\header {
  texidoc = "The @code{VerticalAxisGroup.remove-layer} property
  can be used to keep staves alive with reference to other staves
  in the @code{Keep_alive_together_engraver} group."
}

\layout {
  indent = 40\mm
  short-indent = 15\mm
}

\score {
  <<
    \new Staff \with {
      instrumentName = "Continuous"
      shortInstrumentName = "cont"
    } { \repeat unfold 104 g'4 \bar "|." }
    \new StaffGroup \with {
      \consists Keep_alive_together_engraver
    } <<
      \new Staff \with {
        keepAliveInterfaces = #'()
        instrumentName = \markup \center-column { "Alive with A or B" }
        shortInstrumentName = "with A or B"
        \override VerticalAxisGroup.remove-empty = ##t
        \override VerticalAxisGroup.remove-first = ##t
        \override VerticalAxisGroup.remove-layer = #'any
      } { \repeat unfold 104 c''4 }
      \new Staff \with {
        instrumentName = "A"
        shortInstrumentName = "A"
        \override VerticalAxisGroup.remove-empty = ##t
        \override VerticalAxisGroup.remove-first = ##t
        \override VerticalAxisGroup.remove-layer = ##f
      } {
        \repeat unfold 16 c'4
        R1*4
        \repeat unfold 16 c'4
        R1*14
      }
      \new Staff \with {
        keepAliveInterfaces = #'()
        instrumentName = \markup \center-column { "Alive with A" }
        shortInstrumentName = "with A"
        \override VerticalAxisGroup.remove-empty = ##t
        \override VerticalAxisGroup.remove-first = ##t
        \override VerticalAxisGroup.remove-layer = #'above
      } { \repeat unfold 104 c''4 }
      \new Staff \with {
        instrumentName = "B"
        shortInstrumentName = "B"
        \override VerticalAxisGroup.remove-empty = ##t
        \override VerticalAxisGroup.remove-first = ##t
        \override VerticalAxisGroup.remove-layer = ##f
      } {
        R1*8
        \repeat unfold 16 c'4
        R1*13
        c'1
      }
    >>
  >>
}
