\version "2.19.21"
\header {
  texidoc = "In nested syntax, graces are still properly handled."
}
\layout { ragged-right= ##t }

\relative {
  f''1
  \grace e8 f1
  << { \grace { e8 } f1 } >>
}


