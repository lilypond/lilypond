\version "2.13.4"

\header {
  texidoc = "Here only right-margin is given, left-margin will remain default."
}

someNotes = \relative c' { \repeat unfold 40 { c4 d e f }}

\paper {
  right-margin = 40 \mm
}

\score { \someNotes }
