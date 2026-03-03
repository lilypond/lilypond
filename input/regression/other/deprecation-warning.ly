\version "2.25.80"

\header {
  texidoc = "Scheme function @code{ly:deprecation-warning} prints a given
message the first time only.  Duplicate messages are recognized by the
formatted message, not by the format string alone."
}

\include "testing-functions.ily"
#(ly:set-option 'warning-as-error #t)

%% The logger suppresses messages based on equality after formatting.
#(ly:expect-warning "bzt: testing ly:deprecation-warning A 1")
#(ly:expect-warning "bzt: testing ly:deprecation-warning A 2")
#(expect-true "A 1 first time"
  (ly:deprecation-warning "bzt: testing ly:deprecation-warning A 1"))
#(expect-true "A 2 first time"
  (ly:deprecation-warning "bzt: testing ly:deprecation-warning A ~a" 2))
#(expect-false "A 1 again"
  (ly:deprecation-warning "bzt: testing ly:deprecation-warning A ~a" 1))
#(expect-false "A 2 again"
  (ly:deprecation-warning "bzt: testing ly:deprecation-warning A ~a" 2))
#(expect-false "A 1 again"
  (ly:deprecation-warning "bzt: testing ly:deprecation-warning A 1"))
#(expect-false "A 2 again"
  (ly:deprecation-warning "bzt: testing ly:deprecation-warning A 2"))

%% The logger does not suppress based on format string alone.
#(ly:expect-warning "bzt: testing ly:deprecation-warning B 1")
#(ly:expect-warning "bzt: testing ly:deprecation-warning B 2")
#(expect-true "B 1 first time"
  (ly:deprecation-warning "bzt: testing ly:deprecation-warning B ~a" 1))
#(expect-true "B 2 first time"
  (ly:deprecation-warning "bzt: testing ly:deprecation-warning B ~a" 2))
