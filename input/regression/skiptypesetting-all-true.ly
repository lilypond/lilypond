\version "2.13.43"

\header{
  \texinfo = "
A score with @code{skipTypesetting} set for the whole score
will not segfault.
"
}


{
  \set Score.skipTypesetting = ##t
  c'4
}

