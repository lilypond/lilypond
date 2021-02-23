\version "2.23.1"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="User-defined bar lines can be printed for @code{\\repeat
volta} by setting the @code{startRepeatType}, @code{endRepeatType},
and @code{doubleRepeatType} context properties.  This output should
show two adjacent repeated sections with unusually ornate bar lines."
}

\layout {
  ragged-right = ##t
}

\defineBarLine ".[|;:" #'("." ".[|;:" "  |") % start
\defineBarLine ":;|]." #'(":;|]." #f "  |  ") % end
\defineBarLine ":;|][|;:" #'(":;|]." ".[|;:" "  |  |") % double

\new Score \with {
  doubleRepeatType = ":;|][|;:"
  endRepeatType = ":;|]."
  startRepeatType = ".[|;:"
} \new Staff \fixed c' {
  %% This test is intended to be sensitive to enhancements that would
  %% allow different behavior at repeats that are not measure-aligned.
  r2. \repeat unfold 2 { \repeat volta 2 { r4 | r2. } }
}
