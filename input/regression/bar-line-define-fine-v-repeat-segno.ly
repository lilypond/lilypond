\version "2.23.12"

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (G_ "found music after \\fine"))

\header {
  texidoc="Where @code{\\fine} and @code{\\inStaffSegno} occur
together, user-defined bar lines can be printed by setting the
@code{fineSegnoBarType}, @code{fineStartRepeatSegnoBarType},
@code{endRepeatSegnoBarType}, and @code{doubleRepeatSegnoBarType}
context properties.

The output should show two adjacent repeated sections with doubled
dots and thick bar lines, followed by a double thick bar line without
dots.  There should also be an in-staff segno in every case."
}

\layout {
  \context {
    \Score
    doubleRepeatSegnoBarType = "::..S..::"
    endRepeatSegnoBarType = "::..S"
    fineSegnoBarType = "..S"
    fineStartRepeatSegnoBarType = "..S..::"
  }
}

\defineBarLine "..S..::" #'("..S..::" "..S..::" "") % start repeat
\defineBarLine "::..S" #'("::..S" "::..S" "") % end repeat
\defineBarLine "::..S..::" #'("::..S..::" "::..S..::" "") % double rep.
\defineBarLine "..S" #'("..S" "..S" "") % fine + segno

testBars = { \fine \section \inStaffSegno }

\include "bar-line-define-repeat-test.ily"
