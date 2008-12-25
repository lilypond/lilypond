\header {

  texidoc = "Hairpins whose end note is preceded by a bar line
should end at that bar line. "

}

\version "2.12.0"

\relative c'' {
  \override Hairpin #'bound-padding = #1.0
  c4\< c2. c4\!
}
