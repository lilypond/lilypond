\header {

  texidoc = "Hairpins whose end note is preceded by a bar line
should end at that bar line. "

}

\version "2.19.21"

\relative {
  \override Hairpin.bound-padding = #1.0
  c''4\< c2. c4\!
}
