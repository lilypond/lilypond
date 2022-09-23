\version "2.21.0"

\header {

  texidoc = "For vowel transitions, @code{minimum-length} refers to
the drawn length of the arrow.  The protrusion of the syllables and
padding is in effect added to @code{minimum-length} for spacing.  This
default behavior can be changed by overriding @code{springs-and-rods},
which may cause the transition arrow not to be drawn if there is
insufficient space (rather than adding the space necessary to draw it
at @code{minimum-length}).  @code{minimum-length-after-break} controls
the minimum length of the segment following a system break."

}

\layout {
  ragged-right = ##t
}

\score {
  {
    \relative {
      c'2( ^\markup {
        \left-column { \tiny { Padding increases spacing } } } c) |
      c( c) |
      c( ^\markup {
        \left-column { \tiny { Padding shortens arrow } } } c) |
      c( c) |
      c4 ^\markup {
        \left-column { \tiny { "Not enough space"
                               "to draw arrow at"
                               "minimum-length." } } } c r2 | \break
      c2 ^\markup {
        \left-column { \tiny { "Spacing increased"
                               "by extent of"
                               "bounds protrusion" } } } c |
      c ^\markup {
        \left-column { \tiny { "No space added"
                               "to compensate for"
                               "bounds protrusion."
                               "Not enough space"
                               "to draw arrow at"
                               "minimum-length." } } } c \bar "||" \break
      c1 ^\markup { \tiny { "minimum-length-after-break only applies..." } }
      c1 \break
      c1 ^\markup { \tiny { "...after a system break." } }
    }

    \addlyrics {
      \override VowelTransition.minimum-length = #7
      \override VowelTransition.bound-details.left.padding = #4
      \override VowelTransition.bound-details.right.padding = #4
      a \vowelTransition b
      \temporary \override VowelTransition.springs-and-rods = #ly:spanner::set-spacing-rods
      c \vowelTransition d
      e \vowelTransition f
      \revert VowelTransition.springs-and-rods
      eeeee \vowelTransition fffff
      \once \override VowelTransition.springs-and-rods = ##f
      ggggg \vowelTransition hhhhh
      i \vowelTransition
      \override VowelTransition.after-line-breaking = ##t
      \override VowelTransition.minimum-length-after-break = #20
      jjjjjjjjjjjjjjjjjjjj \vowelTransition
      kkkkkkkkkkkkkkkkkkkk
    }
  }
}
