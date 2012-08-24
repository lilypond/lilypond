\version "2.16.0"

\header {
  texidoc = "
Setting the @code{font-name} property does not change the font
size.  The two strings below should be concatenated and have the
same font size.

Note that `the same font size' is related to what lilypond reports
on the console if in verbose mode (3.865234375 units for this
regression test).  If you actually look at the two fonts the
optical size differs enormously.
"
}

\markup \concat {
  "pfsm"
  \override #'(font-name . "Emmentaler-20")
  "pfsm"
}
