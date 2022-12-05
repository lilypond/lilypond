\version "2.24.1"

\header {
  texidoc="A whole-measure rest starting in a volta alternative is
placed correctly."
}

#(ly:set-option 'warning-as-error #t)

{
  \partial 4
  \repeat volta 2 {
    c'4 c'1
    \alternative {
      \volta 1 { c'2. }
      \volta 2 { R1 }
    }
  }
}
