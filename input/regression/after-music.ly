\version "2.27.4"

\header {
  texidoc = "@code{\after} can, just like @code{\skip}, take music as
the expression for its delay."
}

\fixed c' {
  {
    \after { 2. 16 } -> \*16 c16
  }
}
