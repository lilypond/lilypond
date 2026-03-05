\version "2.25.80"

\header {
  texidoc = "Calling the deprecated function @code{ly:moment-mod} triggers a
warning."
}

\include "testing-functions.ily"
#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme "the function '%s' is deprecated")
  "ly:moment-mod")

#(expect-equal ""
  (ly:moment-mod (ly:make-moment 101 1 1004 1) (ly:make-moment 5))
  (ly:make-moment 1 1 4 1)) % 101 % 5 = 1; 1004 % 5 = 4

{ s }
