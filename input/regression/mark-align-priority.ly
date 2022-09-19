
\header {
  texidoc = "When the break-align-symbols property is given as a list,
  the alignment depends on which symbols are visible."
}

\version "2.23.14"
  
\relative {
  \override Score.TextMark.break-align-symbols = #'(clef key-signature staff-bar)
  \override Score.TextMark.self-alignment-X = #CENTER
  c'1
  \clef "bass"
  \textMark "clef"
  \noBreak

  c1
  \clef "treble"
  \key g \major
  \textMark "clef"
  \noBreak

  c1
  \key f \major
  \textMark "key"
  \noBreak

  c1
  \textMark "bar"
  \noBreak

  c1
}
