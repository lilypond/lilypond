\version "2.23.0"

\header {
  texidoc = "@code{BendSpanner} can be used in different styles:
the default, @code{'hold}, printing a dashed line (only useful in combination
with a previous @code{BendSpanner}), @code{'pre-bend}, printing a vertical line,
and @code{'pre-bend-hold}, printing a vertical line continued by a dashed
horizontal line.

The @code{'style} property may be set using @code{\\tweak}, @code{\\override} or
one of @code{\\bendHold}, @code{\\preBend} and @code{\\preBendHold}."
}

bend-styles = {
  <>^"default"
  \grace f'4\^ g'1

  <>^"'hold"
  \once \override BendSpanner.style = #'hold
  \grace f'4\^ g'1
  \grace f'4\^ g'1\bendHold \^ g'1

  <>^"'pre-bend"
  \grace f'4-\tweak style #'pre-bend \^ g'1
  \grace f'4\preBend \^ g'1-\tweak style #'hold \^ g'1

  <>^"'pre-bend-hold"
  \grace f'4-\tweak style #'pre-bend-hold \^ g'1
  \grace f'4\preBendHold \^ g'1-\tweak style #'hold \^ g'1
  \grace f'4 \^ g'1-\tweak style #'hold \^ g'1\^ f'
  \bar "|."
}

\score {
  \new StaffGroup
  <<
    \new Staff { \override TextScript.font-size = -2 \clef "G_8" \bend-styles }
    \new TabStaff \bend-styles
  >>
  \layout {
    \context {
      \Voice
      \omit StringNumber
    }
    \context {
      \TabStaff
      minimumFret = #5
    }
  }
}
