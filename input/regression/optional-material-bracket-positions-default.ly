\version "2.25.25"

\header {
  texidoc = "Optional-material brackets extend 1.5 staff spaces beyond bar lines
by default."
}

#(ly:set-option 'warning-as-error #t)

#(set-global-staff-size 40)

\paper {
  %% close vertical spacing of scores
  score-system-spacing.basic-distance = #8
  score-system-spacing.minimum-distance = #0
  score-system-spacing.padding = #1
}

\layout {
  \context {
    \Staff
    \omit TimeSignature
  }
}

music = {
  \startOptionalMaterial a4 \stopOptionalMaterial
  \startOptionalMaterial f'4 \stopOptionalMaterial
  \startOptionalMaterial e''4 \stopOptionalMaterial
  \startOptionalMaterial c'''4 \stopOptionalMaterial
  \fine
}

\new Staff \with {
  \override StaffSymbol.line-count = 0
} \music

\new Staff \with {
  \override StaffSymbol.line-count = 1
} \music

\new Staff \with {
  \override StaffSymbol.line-count = 4
} \music

\new Staff \with {
  \override StaffSymbol.line-count = 5
} \music

\new Staff \with {
  \override StaffSymbol.line-count = 6
} \music
