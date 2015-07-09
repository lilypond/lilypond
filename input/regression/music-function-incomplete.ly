\version "2.19.24"

\header {
  texidoc = "For defining a music function, one can supply one or
  several music function calls chained together,  cutting the last
  call short using @code{\\etc}.  The remaining arguments are
  supplied when calling the music function defined in this manner."
}

\layout { ragged-right = ##t }

highlight = \tweak font-size 3 \tweak color #red \etc
mode = \key c \etc

{ c' \highlight d' e'-\highlight -! \mode \minor c'' }
