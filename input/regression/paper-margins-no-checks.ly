\version "2.19.21"

\header {
  texidoc = "All checks can be avoided by setting check-consistency to ##f in \paper."
}

someNotes = \relative { \repeat unfold 40 { c'4 d e f } }

\paper {
  left-margin = 20 \mm
  right-margin = 40 \mm
  line-width = 200 \mm
  check-consistency = ##f
}

\book {
  \score { \someNotes }
}


