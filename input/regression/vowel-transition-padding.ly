\version "2.21.0"
\header {
  texidoc = "Padding does not cause @code{VowelTransition}s to become
shorter than @code{minimum-length}.  Instead, space is added if
necessary leaving the arrow at @code{minimum-length}."
  }

\layout {
  ragged-right = ##t
}

\score {
  {
    \relative {
      c'2( c) |
      c( c) |
      c( ^\markup { \left-column { \tiny {
          "Padded, but"
          "spacing is"
          "not changed."
          "(Arrow is still"
          "longer than"
          "minimum-length)" } } } c) |
      c( c) | \break
      c c |
      c ^\markup { \left-column { \tiny {
          "Space is added"
          "to allow for"
          "padding. Arrow"
          "is drawn at"
          "minimum-length" } } } c |
    }

    \addlyrics {
      a \vowelTransition b
      \once \override VowelTransition.bound-details.left.padding = #3
      \once \override VowelTransition.bound-details.right.padding = #3
      c \vowelTransition d
      eeeee \vowelTransition fffff
      \once \override VowelTransition.bound-details.left.padding = #3
      \once \override VowelTransition.bound-details.right.padding = #3
      ggggg \vowelTransition hhhhh
    }
  }
}
