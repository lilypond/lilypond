\version "2.23.8"

\header {
  texidoc = "@code{\\autoLineBreaksOff} can be used to turn off
automatic line breaking.  @code{\autoLineBreaksOn} reenables it."
}

\paper {
  #(set-paper-size "a8")
}

{
  c'1 1 1
  \autoLineBreaksOff
  d'1 1 1 1
  \once \autoLineBreaksOn
  e'1 1 1 1 1 1 1 \break
  1 1 1 1 1 1 1 \break % page breaks are allowed; see also \autoBreaksOff
  1 1 1 1 1 1 1
  \autoLineBreaksOn
  f'1 1 1 1 1 1 1
}
