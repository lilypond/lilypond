\version "2.13.7"

\header {
  texidoc = "Default margin values are accessible in paper-defaults-init.ly
and apply to the default paper size returned by (ly:get-option
'paper-size). For other paper sizes, they are scaled linearly.
This also affects head- and foot-separation as well as indents."
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
