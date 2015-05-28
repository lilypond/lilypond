\version "2.19.21"

\header {
  texidoc = "Two-sided mode allows you to use different margins for
odd and even pages."
}

someNotes = \relative { \repeat unfold 200 { c'4 d e f } }

\paper {
  two-sided = ##t
  inner-margin = 10 \mm
  outer-margin = 20 \mm
}

\book {
  \score { \someNotes }
}

