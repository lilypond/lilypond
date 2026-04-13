\version "2.25.35"

\header {
  texidoc = "In two-sided mode, a binding offset can be specified, which is added
to the inner margin automatically."
}

someNotes = \relative { \*200 { c'4 d e f } }

\paper {
  two-sided = ##t
  inner-margin = 10 \mm
  outer-margin = 20 \mm
  binding-offset = 5 \mm
}

\book {
  \score { \someNotes }
}
