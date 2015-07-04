\version "2.18.0"

\header {
  texidoc = "This checks the warning of @code{\\barNumberCheck}."
}

\layout { ragged-right = ##t }

#(ly:expect-warning (_ "Barcheck failed got ~a expect ~a") 2 3)
\book {
  \score {
    {
      c'2^\markup \with-color #green \bold "\\barNumberCheck #1"
      \barNumberCheck #1
      d'^\markup \with-color #red \bold "\\barNumberCheck #3"
      \barNumberCheck #3
    }
  }
}
