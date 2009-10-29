\version "2.13.8"

\header {
  texidoc = "Two-sided mode allows you to use different margins for
odd and even pages."
}

someNotes = \relative c' { \repeat unfold 200 { c4 d e f } }

\paper {
  two-sided = ##t
  inner-margin = 10 \mm
  outer-margin = 20 \mm
}

\book {
  \score { \someNotes }
}

