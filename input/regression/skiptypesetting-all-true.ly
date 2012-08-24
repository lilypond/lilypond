\version "2.16.0"
#(ly:expect-warning (_ "didn't find a vertical alignment in this system"))
#(ly:expect-warning (_ "didn't find a vertical alignment in this system"))
#(ly:expect-warning (_ "didn't find a vertical alignment in this system"))
#(ly:expect-warning (_ "didn't find a vertical alignment in this system"))
#(ly:expect-warning (_ "didn't find a vertical alignment in this system"))
#(ly:expect-warning (_ "didn't find a vertical alignment in this system"))
#(ly:expect-warning (_ "system with empty extent"))
#(ly:expect-warning (_ "didn't find a vertical alignment in this system"))

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

