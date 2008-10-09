\version "2.11.60"

\header {
  texidoc = "Three types of double repeat bar line are supported."
}
\new StaffGroup <<
  \new Staff \relative c' {
    c1
    \mark "\":|:\""
    \bar ":|:"
    c1
    \mark "\":|.|:\""
    \bar ":|.|:"
    c1
    \mark "\":|.:\""
    \bar ":|.:"
    c1
  }
  \new Staff \relative c' {
    \repeat unfold 4 { c1 }
  }
>>
