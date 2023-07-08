\version "2.25.7"

\header {
  texidoc = "When the @code{Timing} context is initialized with
@code{timeSignatureFraction = ##f}, various derived properties are set
accordingly.  The output should have no time signature or bar lines."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  \context {
    \Score
    forbidBreakBetweenBarLines = ##f
    timeSignatureFraction = ##f
  }
}

\fixed c' {
  c8 8 8 8  8 8 8 8
  d8 8 8 8  8 8 8 8
  e8 8 8 8  8 8 8 8
  f8 8 8 8  8 8 8 8
  g8 8 8 8  8 8 8 8
  a8 8 8 8  8 8 8 8
}
