\version "2.25.25"

\header {
  texidoc = "Incipits may use other context tyoes then the default
@code{MensuralStaff}"
}

\score {
  <<
    \new Staff \with { instrumentName = "MensuralStaff, default" }
      {
        \incipit  { \clef "petrucci-c1" c'4 }
        c'4 d' e' f' g'1
      }

    \new Staff \with { instrumentName = "KievanStaff" }
      {
        \incipit \new KievanStaff { c'4 d' }
        c'4 d' e' f' g'1
      }

    \new Staff \with { instrumentName = "PetrucciStaff" }
      {
        \incipit \new PetrucciStaff { c'4 }
        c'4 d' e' f' g'1
      }

    \new Staff \with { instrumentName = "VaticanaScore" }
      {
        \incipit \new VaticanaScore { c'4 }
        c'4 d' e' f' g'1
      }

    \new Staff \with { instrumentName = "TabStaff" }
      {
        \incipit
          \new TabStaff
            \with {
              \magnifyStaff 0.5
              \override InstrumentName.font-size = 6
            }
            { c'4 }
        c'4 d' e' f' g'1
      }
  >>
  \layout {
    indent = 6\cm
    incipit-width = 2\cm
    \context {
      \Staff
      \override InstrumentName.padding = 0
    }
    \context {
      \VaticanaScore
      \consists Instrument_name_engraver
    }
  }
}
