\version "2.25.22"

\header {
  texidoc = "@code{\set} warns and refuses to assign a value that does not
match a deprecated property's type predicate."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "the property '%s' is deprecated; use '%s'")
  "deprecatedLySetWrong" "skipTypesetting")
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "the property '%s' must be of type '%s', ignoring invalid value '%s'")
  "deprecatedLySetWrong" "boolean" "\"not a Boolean\"")

%% This ad-hoc property name should be unique within the full set of test cases
%% because the deprecation warning is issued only once per lilypond process.  If
%% multiple tests are run in the same process, they won't all be able to observe
%% a warning for the same property.
#(define-deprecated-property
  'translation-type? 'deprecatedLySetWrong boolean?
  #:new-symbol 'skipTypesetting
  #:old->new not)

{
  \set Timing.deprecatedLySetWrong = "not a Boolean"
  s
}
