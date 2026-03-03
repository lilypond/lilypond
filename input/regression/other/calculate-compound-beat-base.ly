\version "2.25.80"

\header {
  texidoc = "Calling deprecated functions
@code{calculate-@/compound-@/beat-@/base} and
@code{calculate-@/compound-@/beat-@/base-@/as-@/moment} triggers warnings."
}

\include "testing-functions.ily"
#(ly:set-option 'warning-as-error #t)

#(ly:expect-warning
  (G_ "the function '~a' is deprecated; use '~a'")
  "calculate-compound-beat-base" "beat-base")
#(expect-equal ""
  (calculate-compound-beat-base '(2 3 4))
  1/4)

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning
  (G_ "the function '~a' is deprecated; use '~a'")
  "calculate-compound-beat-base-as-moment" "beat-base")
#(expect-equal ""
  (calculate-compound-beat-base-as-moment '(2 3 4))
  (ly:make-moment 1/4))

{ s }
