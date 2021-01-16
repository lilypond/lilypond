\version "2.16.0"
#(ly:set-option 'warning-as-error #t)

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
