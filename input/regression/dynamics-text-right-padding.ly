\version "2.13.11"
\header {
  texidoc = "The space between an absolute dynamic and a dynamic text
span can be changed using @code{'right-padding}.
"
}

\relative c' {
  \dimTextDim
  \once \override DynamicText #'right-padding = #0
  c4\fff\> c c c\!
}
