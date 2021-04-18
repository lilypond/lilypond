\version "2.23.3"

\header {
  texidoc = "Centered bar numbers honor the @code{barNumberVisibility}
context property."
}

\layout {
  \context {
    \Score
    centerBarNumbers = ##t
    barNumberVisibility = #(every-nth-bar-number-visible 3)
  }
}

\repeat unfold 20 { c'1 }