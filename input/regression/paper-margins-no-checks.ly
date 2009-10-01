\version "2.13.5"

\header {
  texidoc = "All checks can be avoided by setting check-consistency to ##f in \paper."
}

someNotes = \relative c' { \repeat unfold 40 { c4 d e f } }

\paper {
  left-margin = 20 \mm
  right-margin = 40 \mm
  line-width = 200 \mm
  check-consistency = ##f
}

\score { \someNotes }


