\version "2.13.5"

\header {
  texidoc = "Here only left-margin is given, right-margin will remain default."
}

someNotes = \relative c' { \repeat unfold 40 { c4 d e f } }

\paper {
  #(set-paper-size (ly:get-option 'paper-size))
  left-margin = 40 \mm
}

\book {
  \score { \someNotes }
}
