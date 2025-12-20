\version "2.25.32"

#(set-default-paper-size "a7")
#(set-global-staff-size 11)

\book {
  \header {
    texidoc = "By setting the @code{bottom-padding} subproperty
within the @code{line-break-system-details} property of the
@code{NonMusicalPaperColumn} grob, the distance between the bottom
of the page and the lowest staff can be controlled individually.

In this test, the systems are almost touching vertically,
because a large @code{bottom-padding} value overrides the
@code{ragged-last-bottom} property (set to @code{#f})."
}

  \score {
    <<
      \new Staff <<
        \new Voice {
          s1*5 \break
          s1*5 \break
          \once \override
            Score.NonMusicalPaperColumn.line-break-system-details =
              #'((bottom-padding . 45))
          s1*5 \break }
        \new Voice { \repeat unfold 15 { c'4 c' c' c' } }
      >>
      \new Staff { \repeat unfold 15 { d'4 d' d' d' } }
    >>
  }
  \paper { ragged-last-bottom = ##f }
}
