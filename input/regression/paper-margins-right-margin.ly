\version "2.13.5"

\header {
  texidoc = "Here only right-margin is given, left-margin will remain default."
}

someNotes = \relative c' { \repeat unfold 40 { c4 d e f } }

\paper {
  #(set-paper-size (ly:get-option 'paper-size))
  right-margin = 40 \mm
}

\book {
  \score { \someNotes }
}
