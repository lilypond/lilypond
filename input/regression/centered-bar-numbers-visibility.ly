\version "2.25.35"

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

\*20 { c'1 }