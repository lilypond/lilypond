\version "2.25.35"

\header {
  texidoc = "Two-sided mode allows you to use different margins for
odd and even pages."
}

someNotes = \relative { \*200 { c'4 d e f } }

\paper {
  two-sided = ##t
  inner-margin = 10 \mm
  outer-margin = 20 \mm
}

\book {
  \score { \someNotes }
}
