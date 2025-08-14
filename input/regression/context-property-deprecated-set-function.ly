\version "2.25.28"

\header {
  texidoc = "@code{\\propertySet} works on a deprecated context property.  The
output should be a single measure with a B@tie{}note."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "the property '%s' is deprecated; use '%s'")
  "deprecatedLySetFn" "skipTypesetting")

%% This ad-hoc property name should be unique within the full set of test cases
%% because the deprecation warning is issued only once per lilypond process.  If
%% multiple tests are run in the same process, they won't all be able to observe
%% a warning for the same property.
#(define-deprecated-property
  'translation-type? 'deprecatedLySetFn boolean?
  #:new-symbol 'skipTypesetting
  #:old->new not)

\fixed c' {
  \propertySet Timing.deprecatedLySetFn ##f
  R1^"FAIL"
  %% We set the deprecated property again, but lilypond warns only once.
  \propertySet Timing.deprecatedLySetFn ##t
  b1
}
