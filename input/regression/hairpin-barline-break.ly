\header {

  texidoc ="If a hairpin ends on the first note of a new stave, we
don't print that ending.  But on the previous line, this hairpin
should not be left open, and should end at the barline. "
}

\version "2.10.0"
\layout { ragged-right = ##t }
\relative c' {
  \set hairpinToBarline = ##t
  c1\>
  \break
  c1\!
}
