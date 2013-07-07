\header {

  texidoc = "Key signatures get the required amount of horizontal space."

}
\version "2.16.0"
<<
  \new Staff {
    \voiceOne
    \key f \minor
    f'4 f' f' f'
    \key b \major
    e''8 e'' e''4 e''2
  }
  \new Staff {
    R1 \bar "||"
    R1
  }
>>
