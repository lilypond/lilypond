\version "2.16.0"

\header {
  texidoc = "Default values for margins, indents, and offsets are
accessible in paper-defaults-init.ly and apply to the default
paper size returned by (ly:get-option 'paper-size).  For other
paper sizes, they are scaled linearly."
}

someNotes = \repeat unfold 20 { c4 d e f }

\paper {
  #(set-paper-size "a6")
}

\book {
  \markup { For other paper sizes, margins are scaled accordingly. }
  \score {
    \relative c' {
      \someNotes
    }
  }
}

