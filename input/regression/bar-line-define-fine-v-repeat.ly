\version "2.23.12"

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (G_ "found music after \\fine"))

\header {
  texidoc="At @code{\\fine} without @code{\\inStaffSegno},
user-defined bar lines can be printed by setting the
@code{fineBarType}, @code{startRepeatBarType},
@code{endRepeatBarType}, and @code{doubleRepeatBarType} context
properties.

The output should show two adjacent repeated sections with doubled
dots and thick bar lines, followed by a double thick bar line without
dots."
}

\layout {
  \context {
    \Score
    doubleRepeatBarType = "::..::"
    endRepeatBarType = "::.."
    fineBarType = "..-test"
    startRepeatBarType = "..::"
  }
}

\defineBarLine "..::" #'("..::" "..::" "") % start repeat
\defineBarLine "::.." #'("::.." "::.." "") % end repeat
\defineBarLine "::..::" #'("::..::" "::..::" "") % double repeat
\defineBarLine "..-test" #'(".." ".." "") % fine

testBars = { \fine \section }

\include "bar-line-define-repeat-test.ily"
