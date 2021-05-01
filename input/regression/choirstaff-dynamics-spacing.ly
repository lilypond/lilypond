\version "2.23.3"

\header {
  texidoc = "In @code{ChoirStaff} contexts, dynamics are allowed
to cross columns."
}

\new ChoirStaff <<
  \new Staff { c4\p 4\p 4\p 4\ppppp }
  \new Staff { c4 4 4 4 }
>>
