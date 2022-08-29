\version "2.23.13"

\header {
  texidoc="In staff groups where span bar lines are engraved, caesura
marks aligned on bar lines appear outside the extremal staves only,
even at points where no span bar is visible.

The top @code{PianoStaff} should not have fermatas between the staves
where the other @code{PianoStaff} and @code{ChoirStaff}s do."
}

music = \fixed c' {
  d1
  \caesura
  e1
  \caesura
  \bar "k"

  f1
  \override Staff.CaesuraScript.direction = #DOWN
  \caesura
  g1
  \caesura
  \bar "k"
}

\score { <<
  \new PianoStaff \with {
    caesuraType = #'((scripts . (fermata)))
  } <<
    \new Staff \music
    \new Staff \music
  >>
  \new PianoStaff \with {
    caesuraType = #'((breath . caesura) (scripts . (fermata)))
  } <<
    \new Staff \music
    \new Staff \music
  >>
  \new ChoirStaff \with {
    caesuraType = #'((scripts . (fermata)))
  } <<
    \new Staff \music
    \new Staff \music
  >>
  \new ChoirStaff \with {
    caesuraType = #'((breath . caesura) (scripts . (fermata)))
  } <<
    \new Staff \music
    \new Staff \music
  >>
>> }
