\header {

  texidoc = "If a hairpin ends on the first note of a new staff, we
do not print that ending.  But on the previous line, this hairpin
should not be left open, and should end at the bar line. "
}

\version "2.14.0"

\layout {
  line-width = 4.\cm
}

\relative c' {
  c1\>
  \break
  c1\!
  \override Hairpin #'to-barline = ##f
  c1\>
  \break
  c1\!
}
