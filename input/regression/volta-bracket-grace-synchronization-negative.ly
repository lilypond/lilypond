\version "2.25.32"

\header {
  texidoc = "Volta brackets for alternatives beginning with different lengths of
grace notes are not merged if they pertain to different volte.

In the shared staff, the brackets created for the grace notes of part@tie{}B
should end prematurely where part@tie{}A enters.
"
}

#(ly:set-option 'warning-as-error #t)

#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "already have a VoltaBracket; ending it prematurely"))
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "already have a VoltaBracket; ending it prematurely"))
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "already have a VoltaBracket; ending it prematurely"))
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "already have a VoltaBracket; ending it prematurely"))
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "already have a VoltaBracket; ending it prematurely"))

voiceA = \fixed c' {
  \repeat volta 3 {
    c'1
    \alternative {
      \volta 1 c'1
      \volta 2 c'1
      \volta 3 c'1
    }
    %% Additional material after \alternative avoids entangling this case with
    %% repeat bar lines.
    c'1
  }
}

voiceB = \fixed c' {
  \repeat volta 6 {
    a1
    \alternative {
      \volta 1,2 { \grace { d16 e f g } a1 }
      \volta 3,4 { \grace { d16 e f g } a1 }
      \volta 5,6 { \grace { d16 e f g } a1 }
    }
    %% Additional material after \alternative avoids entangling this case with
    %% repeat bar lines.
    \grace { d16 e f g } a1
  }
}

shared = << \voiceA \\ \voiceB >>

\score { \new Staff \with { instrumentName = "A" } \voiceA }
\score { \new Staff \with { instrumentName = "B" } \voiceB }
\score {
  \new Staff \with { instrumentName = \markup \column { A B } } \shared
}
