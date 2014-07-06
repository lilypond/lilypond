\version "2.19.8"

\header {
  texidoc = "Ties should be scaled along with notation size when
using the @code{\magnifyMusic} command.  They can get thicker than
the default, but not thinner."
}

template = {
  \omit Staff.TimeSignature
  \time 7/8
  \magnifyMusic 0.50 s8
  \magnifyMusic 0.63 s
  \magnifyMusic 0.80 s
  \magnifyMusic 1.00 s
  \magnifyMusic 1.26 s
  \magnifyMusic 1.59 s
  \magnifyMusic 2.00 s
}

\score {
  \new StaffGroup <<
    \new Staff \new Voice = "upper" <<
      \template
      \repeat unfold 7 { g'32[~ g' a'~ a'] }
    >>
    \new Staff \new Voice <<
      \template
      \repeat unfold 7 { \tieUp g'32[~ g' a'~ a'] }
    >>
    \new Lyrics \with {
      \override VerticalAxisGroup.staff-affinity = #DOWN
    } \lyricsto "upper" {
      "50%" _ _ _ _ _ "100%" _ _ _ _ _ " 200%"
    }
    \new Staff \new Voice <<
      \clef bass
      \template
      \repeat unfold 7 { f32[~ f e~ e] }
    >>
    \new Staff \new Voice <<
      \clef bass
      \template
      \repeat unfold 7 { \tieDown f32[~ f e~ e] }
    >>
  >>
}
