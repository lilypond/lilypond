\version "2.11.51"
\header {
  texidoc = "In nested syntax, graces are still properly handled."
}
\layout { ragged-right= ##t }

\relative c'' {
  f1
  \grace e8 f1
  << { \grace { e8 } f1 } >>
}


