\version "2.23.6"

\header {
  texidoc = "Balloons also reserve space vertically when the
@code{Balloon_engraver} is in @code{Score} context."
}

\new Score \with {
  \consists Balloon_engraver
}
<<
  \new Staff c'1
  \new Staff <a''-\balloonText #'(0 . 1) "A note head">1
>>
