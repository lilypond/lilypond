\version "2.23.8"

\header {
  texidoc = "@code{\\autoBreaksOff} disables automatic line breaks
and page breaks.  @code{\\autoBreaksOn} reenables both of them."
}

\paper {
  #(set-paper-size "a8")
}

{
  c'1 1 1 1 1 1 1
  \autoBreaksOff
  % \break allows a line break, but not a page break.
  d'1 1 1 1 1 1 1 \break
  1 1 1 1 1 1 1 \break
  1 1 1 1 1 1 1 \break
  1 1 1 1 1 1 1 \break
  1 1 1 1 1 1 1 \break
  % \pageBreak can be used to force a page break.
  \pageBreak
  1 1 1 1 1 1 1
  \autoBreaksOn
  e'1 1 1 1 1 1 1
  1 1 1 1 1 1 1
}
