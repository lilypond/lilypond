\version "2.19.21"

\header {
  texidoc = "Three types of double repeat bar line are supported."
}
\new StaffGroup <<
  \new Staff \relative {
    c'1
    \mark "\":|:\""
    \bar ":..:"
    c1
    \mark "\":|.|:\""
    \bar ":|.|:"
    c1
    \mark "\":|.:\""
    \bar ":|.:"
    c1
  }
  \new Staff \relative {
    \repeat unfold 4 { c'1 }
  }
>>
