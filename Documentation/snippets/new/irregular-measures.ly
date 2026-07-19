\version "2.27.4"

\header {
  categories = "Rhythms, Real music"

  texidoc = "
@qq{Es ist ein Ros entsprungen} was first published without bar lines in
@cite{Musae Sioniae} (1609).  This example adds irregular measures to the
melody.

The @code{\\measure} command works for complete measures.  The full length of
the incomplete measure at the end of the repeated section is specified with
@code{\\setMeasureLengthFromHere} to preserve correctness if the repeat were
unfolded.  In isolation, @code{\\setMeasureLengthFromHere} would normally be
followed at the end of the measure by @code{\\setDefaultMeasureLength} to
revert to the time signature's measure length, but that is unnecessary in this
case because the following measure is also irregular.
"

  doctitle = "Irregular measures"
}


\layout {
  \verboseBarNumbers
}

\fixed c' {
  \time 2/2
  \key f \major
  \repeat volta 2 {
    \premeasure { c'1 }
    \measure { c'2 c' d' c' }
    \measure { c'1 a bes }
    \measure { a2 g1 f e2 }
    \setMeasureLengthFromHere 1*2
    f1
    \break
  }
  r2 a |
  \measure { g2 e f d }
  \measure { c1 r2 c' }
  \measure { c'2 c' d' c' }
  \measure { c'1 a bes }
  \measure { a2 g1 f e2 }
  \measure { f\longa }
  \fine
}
