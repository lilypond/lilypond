\version "2.25.14"

\header {
  texidoc = "Dashed and dotted bar lines print nicely with small values of
@code{set-global-staff-size} and @code{layout-set-staff-size}."
}


#(set-global-staff-size 10)

mus =
  { b4 \bar "!" b \bar ";" }

staffGroup =
  \new StaffGroup
  <<
    \new Staff
      \with {
        \override VerticalAxisGroup.staff-staff-spacing =
         #'((basic-distance . 9)
            (minimum-distance . 8)
            (padding . 12))
      }
      { \clef alto \mus }
    \new TabStaff \mus
  >>

\score {
  \staffGroup
}

\score {
  \staffGroup
  \layout { #(layout-set-staff-size 33) }
}
