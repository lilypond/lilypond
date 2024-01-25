\version "2.25.13"

\header {
  texidoc = "@code{\\incipit} can be used without the default enclosing
context @code{MensuralStaff}."
}

notes =
\relative c'' {
  \clef treble
  \key f \major
  \time 4/4
  c4 d e f
  \bar "|."
}

\score {
  \new Staff \with { instrumentName = "Violino discordato" }
  {
    \incipit \new Staff \with {
      firstClef = ##f
      \omit TimeSignature
      \omit Stem
    } < a d' fis' cis'' >
    \notes
  }
  \layout {
    indent = 4\cm
    incipit-width = 1\cm
  }
}
