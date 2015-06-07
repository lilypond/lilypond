\version "2.19.21"

\header {
  texidoc = "Here only right-margin is given, left-margin will remain default."
}

someNotes = \relative { \repeat unfold 40 { c'4 d e f } }

\paper {
  #(set-paper-size (ly:get-option 'paper-size))
  right-margin = 40 \mm
}

\book {
  \score { \someNotes }
}
