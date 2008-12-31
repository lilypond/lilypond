
\version "2.12.0"
\header {
  texidoc = "Rests get a little less space, since they are narrower.
However, the quarter rest in feta font is relatively wide, causing this 
effect to be very small.
"
}

\layout {
  ragged-right = ##t
}
\relative c'' \context Staff {
  \time 12/4
  r4 c4 c4 c4 r4 r4 c4
}



