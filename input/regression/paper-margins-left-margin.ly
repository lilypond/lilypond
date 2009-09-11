\version "2.13.4"

\header {
  texidoc = "Here only left-margin is given, right-margin will remain default."
}

someNotes = \relative c' { \repeat unfold 40 { c4 d e f }}

\paper {
  left-margin = 40 \mm
}

\score { \someNotes }
