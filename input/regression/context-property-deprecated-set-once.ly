\version "2.25.22"

\header {
  texidoc = "@code{\once \set} works on a deprecated context property.  The
output should be a single measure with a B@tie{}note."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "the property '%s' is deprecated; use '%s'")
  "deprecatedLySetOnce" "skipTypesetting")

%% This ad-hoc property name should be unique within the full set of test cases
%% because the deprecation warning is issued only once per lilypond process.  If
%% multiple tests are run in the same process, they won't all be able to observe
%% a warning for the same property.
#(define-deprecated-property
  'translation-type? 'deprecatedLySetOnce boolean?
  #:new-symbol 'skipTypesetting
  #:old->new not)

\fixed c' {
  \once \set Timing.deprecatedLySetOnce = ##f
  R1^"FAIL"
  b1
}
