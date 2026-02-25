\version "2.25.35"

\header {
  texidoc = "This is a set of tests of the Scheme function
@code{ly:parser-lookup}.  Problems are reported in the standard error stream."
}

\include "testing-functions.ily"
#(ly:set-option 'warning-as-error #t)

testAlistVar = #'((a . "a val") (b . "b val"))
testSimpleVar = "simple val"

#(expect-equal
  "undefined; implicit default"
  (ly:parser-lookup 'testNoSuchVar)
  '())

#(expect-equal
  "undefined; explicit default"
  (ly:parser-lookup 'testNoSuchVar #:default *unspecified*)
  *unspecified*)

#(expect-equal
  "defined simple var"
  (ly:parser-lookup 'testSimpleVar)
  "simple val")
