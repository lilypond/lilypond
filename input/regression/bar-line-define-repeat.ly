\version "2.23.1"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="User-defined bar lines can be printed for @code{\\repeat
volta} by setting the @code{startRepeatType}, @code{endRepeatType},
and @code{doubleRepeatType} context properties.

If the user does not define variants of these bar types annotated with
the value of @code{underlyingRepeatType}, LilyPond falls back on those
that are defined.

This output should show two adjacent repeated sections with unusually
ornate bar lines."
}

\layout {
  ragged-right = ##t
}

%% For a score with span-bars, ":;|]." would need to be defined as
%% well, but we don't need to complicate this test with that.
\defineBarLine ".[|;:" #'("." ".[|;:" "  |") % start
\defineBarLine ":;|]." #'(":;|]." #f "  |  ") % end
\defineBarLine ":;|][|;:" #'(":;|]." ".[|;:" "  |  |") % double

\new Score \with {
  doubleRepeatType = ":;|][|;:"
  endRepeatType = ":;|]."
  startRepeatType = ".[|;:"
} \new Staff \fixed c' {
  r2. \repeat unfold 2 { \repeat volta 2 { r4 | r2. } }
}
