\version "2.11.61"
\header {
  lsrtags = "expressive-marks"
  texidoc = "If the note which ends a hairpin falls on a downbeat,
the hairpin stops at the bar line immediately preceding.  This behavior
can be controlled by overriding the @code{'to-barline} property.
"
  doctitle = "Setting hairpin behavior at bar lines"
}

\relative c'' {
  e4\< e2.
  e1\!
  \override Hairpin #'to-barline = ##f
  e4\< e2.
  e1\!
}
