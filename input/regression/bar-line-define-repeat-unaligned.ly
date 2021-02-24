\version "2.23.1"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="User-defined bar lines can be printed for @code{\\repeat
volta} by setting the @code{startRepeatType}, @code{endRepeatType},
and @code{doubleRepeatType} context properties, and when the bar line
does not fall on a measure boundary, the value of
@code{underlyingRepeatType} is appended.  This output should show two
adjacent repeated sections with unusually ornate bar lines."
}

\layout {
  ragged-right = ##t
}

%% Notice that we define *only* the -|| versions of these.
%%
%% For a score with span-bars, ":;|]." would need to be defined as
%% well, but we don't need to complicate this test with that.
\defineBarLine ".[|;:-||" #'("." ".[|;:" "  |") % start
\defineBarLine ":;|].-||" #'(":;|]." #f "  |  ") % end
\defineBarLine ":;|][|;:-||" #'(":;|]." ".[|;:" "  |  |") % double

\new Score \with {
  %% N.B. We don't want the -|| annotation here.  LilyPond should
  %% append it automatically for unaligned repeats/segni.
  doubleRepeatType = ":;|][|;:"
  endRepeatType = ":;|]."
  startRepeatType = ".[|;:"
} \new Staff \fixed c' {
  r2. \repeat unfold 2 { \repeat volta 2 { r4 | r2. } }
}
