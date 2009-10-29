\version "2.13.8"

\header {
  texidoc = "In two-sided mode, a binding offset can be specified, which is added
to the inner margin automatically."
}

someNotes = \relative c' { \repeat unfold 200 { c4 d e f } }

\paper {
  two-sided = ##t
  inner-margin = 10 \mm
  outer-margin = 20 \mm
  binding-offset = 5 \mm
}

\book {
  \score { \someNotes }
}

