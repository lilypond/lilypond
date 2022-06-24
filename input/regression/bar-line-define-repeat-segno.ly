\version "2.23.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="User-defined bar lines with in-staff segni can be printed
by setting the @code{segnoBarType}, @code{startRepeatSegnoBarType},
@code{endRepeatSegnoBarType}, and @code{doubleRepeatSegnoBarType}
context properties.

The output should show two adjacent repeated sections
with unusually ornate bar lines with in-staff segni, followed by an
in-staff segno that is flanked by thick bar lines."
}

%% For a score with span-bars, ":;|]." would need to be defined as well,
%% but we don't need to complicate this test with that.
\defineBarLine "S.[|;:" #'("." "S.[|;:" "   |") % start repeat
\defineBarLine ":;|].S" #'(":;|]." "S" "  |   ") % end repeat
\defineBarLine ":;|]S[|;:" #'(":;|]." "S.[|;:" "  |   |") % double repeat
\defineBarLine ".S." #'("." ".S." " = ") % segno alone

\layout {
  \context {
    \Score
    doubleRepeatSegnoBarType = ":;|]S[|;:"
    endRepeatSegnoBarType = ":;|].S"
    startRepeatSegnoBarType = "S.[|;:"
    segnoBarType = ".S."
  }
}

testBars = \inStaffSegno

\include "bar-line-define-repeat-test.ily"
