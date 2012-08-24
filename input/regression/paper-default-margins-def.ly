\version "2.16.0"

\header {
  texidoc = "Default values for margins, indents, and offsets are
accessible in paper-defaults-init.ly and apply to the default
paper size returned by (ly:get-option 'paper-size).  For other
paper sizes, they are scaled linearly."
}

someNotes = \repeat unfold 30 { c4 d e f }

\paper {
  #(set-paper-size (ly:get-option 'paper-size))
}

\book {
  \markup { If the paper size remains default, the margin values from
            paper-defaults-init.ly remain unchanged. }
  \score {
    \relative c' {
      \someNotes
      \someNotes
    }
  }
}
