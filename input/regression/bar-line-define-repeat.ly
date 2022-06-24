\version "2.23.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="User-defined bar lines can be printed for @code{\\repeat
volta} by setting the @code{startRepeatBarType}, @code{endRepeatBarType},
and @code{doubleRepeatBarType} context properties.

This output should show two adjacent repeated sections with unusually
ornate bar lines."
}

%% For a score with span-bars, ":;|]." would need to be defined as
%% well, but we don't need to complicate this test with that.
\defineBarLine ".[|;:" #'("." ".[|;:" "  |") % start
\defineBarLine ":;|]." #'(":;|]." #f "  |  ") % end
\defineBarLine ":;|][|;:" #'(":;|]." ".[|;:" "  |  |") % double

\layout {
  \context {
    \Score
    doubleRepeatBarType = ":;|][|;:"
    endRepeatBarType = ":;|]."
    startRepeatBarType = ".[|;:"
  }
}

testBars = ##{#}

\include "bar-line-define-repeat-test.ily"
