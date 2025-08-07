\version "2.16.0"

\header {
  texidoc = "The @code{\\justify-line} markup command produces equally
spaced words in a line.

Also test behaviour if entries are too long; there shouldn't be any
collisions."
}

#(set-default-paper-size "a6landscape")

\paper {
  line-width = 8\cm
  bookTitleMarkup = \markup {
    \column {
      \justify-line { This }
      \justify-line { This is }
      \justify-line { This is a }
      \justify-line { This is a longer }
      \justify-line { This is a longer test }
      \justify-line { This is a longer test with }
      \justify-line { This is a longer test with many }
      \justify-line { This is a longer test with many words }
      \justify-line { This is a longer test with many words on }
      \justify-line { This is a longer test with many words on a }
      \justify-line { This is a longer test with many words on a single }
      \justify-line { This is a longer test with many words on a single line }
    }
  }

  tagline = ##f
}

\book {
  \score {
    \new Staff \relative c'' {
      \repeat unfold 4 c1
    }
  }
}
