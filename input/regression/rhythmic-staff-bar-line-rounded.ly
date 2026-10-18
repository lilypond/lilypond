\version "2.27.5"

\header
{
  texidoc = "In @code{RhythmicStaff}, bar lines have rounded corners."
}

\markup \scale #'(20 . 20) \score {
  \layout {
    indent = 0
  }
  \new RhythmicStaff {
    \senzaMisura
    s1024 \bar "|"
  }
}
