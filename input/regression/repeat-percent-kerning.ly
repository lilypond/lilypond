\version "2.21.0"
\header {
  texidoc = "The positioning of dots and slashes in percent repeat
glyphs can be altered using @code{dot-negative-kern} and
@code{slash-negative-kern}."
}

<<
  \new Staff {
    \set Staff.instrumentName = "(default)"
    \repeat percent 2 { c'1 }
    \repeat percent 2 { d'1 e' }
  }
  \new Staff {
    \override PercentRepeat.dot-negative-kern = #1.1
    \repeat percent 2 { c'1 }
    \override DoublePercentRepeat.dot-negative-kern = #0
    \override DoublePercentRepeat.slash-negative-kern = #1
    \repeat percent 2 { d'1 e' }
  }
>>
