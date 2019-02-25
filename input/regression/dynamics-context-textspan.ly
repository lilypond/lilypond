\version "2.21.0"

\header {
  texidoc = "Text spanners work in the @code{Dynamics} context."
}

<<
  \new Staff \relative {
    c'1 | c
  }
  \new Dynamics {
    \override TextSpanner.bound-details.left.text = "rit."
    s1\startTextSpan
    s1\stopTextSpan
  }
  \new Staff \relative {
    c'1 | c
  }
>>
