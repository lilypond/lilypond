\version "2.25.35"

\header {
  texidoc = "This is a set of tests of the Scheme function
@code{ly:parser-define-once!}.  Problems are reported in the standard error
stream."
}

\include "testing-functions.ily"
#(ly:set-option 'warning-as-error #t)

testAlreadyDefined = "already defined"
#(ly:parser-define-once! 'testAlreadyDefined "FAIL")
#(expect-equal "symbol is already bound to a value"
  #{ \testAlreadyDefined #}
  "already defined")

#(ly:parser-define-once! 'testFormerlyUndefined "formerly undefined")
#(expect-equal "symbol is not yet bound to a value"
  #{ \testFormerlyUndefined #}
  "formerly undefined")
