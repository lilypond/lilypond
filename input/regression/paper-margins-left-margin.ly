\version "2.19.21"

\header {
  texidoc = "Here only left-margin is given, right-margin will remain default."
}

someNotes = \relative { \repeat unfold 40 { c'4 d e f } }

\paper {
  #(set-paper-size (ly:get-option 'paper-size))
  left-margin = 40 \mm
}

\book {
  \score { \someNotes }
}
