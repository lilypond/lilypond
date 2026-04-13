\version "2.25.35"

\header {
  texidoc = "All checks can be avoided by setting check-consistency to ##f in \\paper."
}

someNotes = \relative { \*40 { c'4 d e f } }

\paper {
  left-margin = 20 \mm
  right-margin = 40 \mm
  line-width = 200 \mm
  check-consistency = ##f
}

\book {
  \score { \someNotes }
}
