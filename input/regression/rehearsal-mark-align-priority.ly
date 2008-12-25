
\header {
  texidoc = "When the break-align-symbols property is given as a list,
  the alignment depends on which symbols are visible."
}

\version "2.12.0"
  
\relative {
  \override Score.RehearsalMark #'break-align-symbols = #'(clef key-signature staff-bar)
  c1
  \clef "bass"
  \mark "clef"
  \noBreak

  c1
  \clef "treble"
  \key g \major
  \mark "clef"
  \noBreak

  c1
  \key f \major
  \mark "key"
  \noBreak

  c1
  \mark "bar"
  \noBreak

  c1
}
