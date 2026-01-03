\version "2.25.32"

\header {
  texidoc="When a new volta bracket interrupts one that is already in progress,
a warning is issued.  In this case, two voices differ in the extent of the first
alternative, although they agree on the extent of the second alternative and
they even unfold to the same length."
}

#(ly:set-option 'warning-as-error #t)

#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "already have a VoltaBracket; ending it prematurely"))

voiceA = {
  \repeat volta 2 {
    c''1
    \alternative {
      \volta 1 { 1 1 }
      \volta 2 { 1 }
    }
  }
  1
}

voiceB = {
  \repeat volta 2 {
    a'1
    1
    \alternative {
      \volta 1 { 1 }
      \volta 2 { 1 }
    }
  }
}

shared = << \voiceA \\ \voiceB >>

\score { \new Staff \with { instrumentName = "A" } \voiceA }
\score { \new Staff \with { instrumentName = "B" } \voiceB }
\score {
  \new Staff \with { instrumentName = \markup \column { A B } } \shared
}
\score {
  \new Staff \with { instrumentName = "unfolded" } \unfoldRepeats \shared
}
