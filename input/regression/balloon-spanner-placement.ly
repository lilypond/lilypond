\version "2.23.6"

\header {
  texidoc = "@code{BalloonText} supports the @code{spanner-placement} property."
}

\paper {
  ragged-right = ##t
}

\new Staff \with {
  \consists Balloon_engraver
}
{
  \balloonGrobText Hairpin #'(1 . -1) "First broken piece"
  c'1\< \break d'1 \break e'1 \break
  \override Staff.BalloonText.spanner-placement = #RIGHT
  \balloonGrobText Hairpin #'(1 . -1) "Last broken piece"
  f'1\> \break e'1 \break d'1 <>\!
}
