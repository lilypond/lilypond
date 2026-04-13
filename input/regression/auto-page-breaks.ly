\version "2.25.35"

\header {
  texidoc = "@code{\\autoPageBreaksOff} turns off automatic
page breaking; @code{\\autoPageBreaksOn} reenables it."
}

\paper {
  #(set-paper-size "b8")
  short-indent = 2
}

{
  \*15 c'1
  \autoPageBreaksOff
  \*15 d'1
  \pageBreak
  \*15 e'1
  \autoPageBreaksOn
  \*15 f'1
}
