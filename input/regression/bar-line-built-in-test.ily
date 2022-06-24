\version "2.23.10"

\paper { ragged-right = ##t }

\layout {
  \context {
    \Score
    %% Hack bar numbers to point to invisible bar lines: make them
    %% center a "V" over a bar line if it is present.
    barNumberVisibility = #all-bar-numbers-visible
    \override BarNumber.break-align-symbols = #'(staff-bar left-edge)
    \override BarNumber.break-visibility = #all-visible
    \override BarNumber.self-alignment-X = #CENTER
    \override BarNumber.stencil = #(lambda (grob)
                                    (ly:grob-set-property! grob 'text "V")
                                    (ly:text-interface::print grob))

    %% Disable automatic measure bar lines.  This might be
    %% unnecessary; the point is to leave room to change the behavior
    %% of \bar in this regard without breaking this test.
    measureBarType = #'()
  }
}

staff = \new Staff \fixed c' {
  \bar \testBar R1 \bar \testBar R1 \bar \testBar
}

\score {
  \new PianoStaff \with { instrumentName = \testBar } << \staff \staff >>
}
