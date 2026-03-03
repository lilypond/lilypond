\version "2.25.80"

\header {
  texidoc = "Calling the deprecated function
@code{calculate-@/compound-@/beat-@/grouping} triggers a warning."
}

\include "testing-functions.ily"
#(ly:set-option 'warning-as-error #t)

#(ly:expect-warning
  (G_ "the function '~a' is deprecated; use '~a'")
  "calculate-compound-beat-grouping" "beat-structure")
#(expect-equal ""
  (calculate-compound-beat-grouping '((2 3 8) (2 3 4)))
  '(2 3 4 6))

{ s }
