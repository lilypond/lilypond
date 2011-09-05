\version "2.14.0"

\header{
  texidoc = "
A score with @code{skipTypesetting} set for the whole score
will not segfault.
"
}


{
  \set Score.skipTypesetting = ##t
  c'4
}

