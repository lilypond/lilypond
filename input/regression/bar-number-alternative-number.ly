\version "2.25.22"

\header {
  texidoc = "When @code{alternativeNumberingStyle} is set to
@code{numbers-with-letters}, the bar numbers in an alternative have a letter
corresponding to the first volta number for the alternative."
}

\layout {
  \context {
    \Score
    \override BarNumber.break-visibility = #all-visible
    alternativeNumberingStyle = #'numbers-with-letters
  }
}

\fixed c' {
  \repeat volta 4 {
    f1
    \alternative {
      \volta 3,2,1 g
      \volta 4 a
    }
  }
}

\fixed c' {
  \repeat volta 4 {
    f1
    \alternative {
      \volta 1,3 g
      \volta 2,4 a
    }
  }
}
