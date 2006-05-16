\header {

  texidoc = "By setting @code{hairpinToBarline}, hairpins will stop at
the barline preceding the ending note."

}

\version "2.9.5"

\paper {
  ragged-right = ##t
}

\relative c'' {
  \set hairpinToBarline = ##t
  \override Hairpin #'bound-padding = #1.0
  c4\< c2. c4\!
}
