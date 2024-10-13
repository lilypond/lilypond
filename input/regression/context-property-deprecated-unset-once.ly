\version "2.25.22"

\header {
  texidoc = "@code{\once \unset} works on a deprecated context property.  The
output should have three bar lines: the middle line should be normal and the
outer lines should be dotted."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "the property '%s' is deprecated; use '%s'")
  "deprecatedLyUnsetOnce" "measureBarType")

%% This ad-hoc property name should be unique within the full set of test cases
%% because the deprecation warning is issued only once per lilypond process.  If
%% multiple tests are run in the same process, they won't all be able to observe
%% a warning for the same property.
#(define-deprecated-property
  'translation-type? 'deprecatedLyUnsetOnce number?
  #:new-symbol 'measureBarType)

\new Staff \with {
  measureBarType = ";"
} {
  R1
  R1
  \once \unset Staff.deprecatedLyUnsetOnce
  R1
}
