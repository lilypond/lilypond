\version "2.19.8"

\header {
  texidoc = "Repeat ties should be scaled along with notation size
when using the @code{\magnifyMusic} command.  They can get thicker
than the default, but not thinner."
}

template = {
  \omit Staff.TimeSignature
  \time 7/2
  \magnifyMusic 0.50 s2
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
      \repeat unfold 7 { g'4\repeatTie \hide Rest r4 }
    >>
    \new Lyrics \with {
      \override VerticalAxisGroup.staff-affinity = #DOWN
    } \lyricsto "upper" {
      " 50%" \skip 1 \skip 1 " 100%" \skip 1 \skip 1 " 200%"
    }
    \new Staff \new Voice <<
      \clef bass
      \template
      \repeat unfold 7 { f4\repeatTie \hide Rest r4 }
    >>
  >>
}
