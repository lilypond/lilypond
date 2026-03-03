\version "2.25.80"

\header {
  texidoc = "Calling deprecated functions
@code{calculate-@/compound-@/measure-@/length} and
@code{calculate-@/compound-@/measure-@/length-@/as-@/moment} triggers warnings."
}

\include "testing-functions.ily"
#(ly:set-option 'warning-as-error #t)

#(ly:expect-warning
  (G_ "the function '~a' is deprecated; use '~a'")
  "calculate-compound-measure-length" "calc-measure-length")
#(expect-equal ""
  (calculate-compound-measure-length '(2 3 4))
  5/4)

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning
  (G_ "the function '~a' is deprecated; use '~a'")
  "calculate-compound-measure-length-as-moment" "calc-measure-length")
#(expect-equal ""
  (calculate-compound-measure-length-as-moment '(2 3 4))
  (ly:make-moment 5/4))

{ s }
