\version "2.25.26"

\header {
  texidoc = "Ottava brackets can be created over skips."
}

#(ly:set-option 'warning-as-error #t)

{
  c'1
  \ottava 1
  s1
  \ottava 0
  c'1
}
