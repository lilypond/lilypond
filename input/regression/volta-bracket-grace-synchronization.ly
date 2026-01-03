\version "2.25.32"

\header {
  texidoc = "Volta brackets correctly span alternatives when multiple voices
begin with different lengths of grace notes.  The brackets in the shared staff
should look like the brackets in the part staves."
}

#(ly:set-option 'warning-as-error #t)

voiceA = \fixed c' {
  \repeat volta 3 {
    e'1
    \alternative {
      e'1
      e'1
      e'1
    }
    %% Additional material after \alternative avoids entangling this case with
    %% repeat bar lines.
    e'1
  }
}

voiceB = \fixed c' {
  \repeat volta 3 {
    c'1
    \alternative {
      { \grace d'8 c'1 }
      { \grace d'8 c'1 }
      { \grace d'8 c'1 }
    }
    %% Additional material after \alternative avoids entangling this case with
    %% repeat bar lines.
    \grace d'8 c'1
  }
}

voiceC = \fixed c' {
  \repeat volta 3 {
    a1
    \alternative {
      { \grace { f8 g } a1 }
      { \grace { f8 g } a1 }
      { \grace { f8 e } d1 }
    }
    %% Additional material after \alternative avoids entangling this case with
    %% repeat bar lines.
    \grace { f8 e } d1
  }
}

shared = << \voiceA \\ \voiceB \\ \voiceC >>

\score { \new Staff \with { instrumentName = "A" } \voiceA }
\score { \new Staff \with { instrumentName = "B" } \voiceB }
\score { \new Staff \with { instrumentName = "C" } \voiceC }
\score {
  \new Staff \with { instrumentName = \markup \column { A B C } } \shared
}
