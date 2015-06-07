\version "2.19.21"

\header {
  texidoc = "Paper margin settings do not have to be complete.
Missing values are added automatically.  If no paper settings
are specified, default values are used."
}

someNotes = \relative { \repeat unfold 40 { c'4 d e f } }

\paper {
  #(set-paper-size (ly:get-option 'paper-size))
}

\book {
  \score { \someNotes }
}

