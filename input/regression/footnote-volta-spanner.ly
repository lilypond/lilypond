\header {
  texidoc = "Footnotes on volta brackets also work"
}

\version "2.21.1"

\relative c'' \repeat volta 2 {
  c1
  \footnote  #'(1 . 1) "volta" Score.VoltaBracket
} \alternative {
  { d } { e }
}
