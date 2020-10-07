\version "2.21.7"

\header {
  texidoc = "The output should include a clef, key signature, and time
signature."
}

#(ly:set-option 'warning-as-error #t)

\score {
  {
    \clef "bass"
    \key d \major
    s4
  }
}
