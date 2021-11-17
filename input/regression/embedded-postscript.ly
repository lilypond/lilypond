\version "2.23.5"

\header {
  texidoc = "
PostScript code can be directly inserted inside a @code{\\markup}
block.
"
}

\score {
  \relative c'' {
    a4_\markup { \postscript "3 4 moveto 5 3 rlineto stroke" }
    a'2.
  }
}

  
