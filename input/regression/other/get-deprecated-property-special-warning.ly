\version "2.25.23"

\header {
  texidoc = "Getting a deprecated property may issue a special warning."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning "All mimsy were þe borogoves")

%% This ad-hoc property name should be unique within the full set of test cases
%% because the deprecation warning is issued only once per lilypond process.  If
%% multiple tests are run in the same process, they won't all be able to observe
%% a warning for the same property.
#(define-deprecated-property
  'translation-type? 'deprecatedScmGetSpecialWarning boolean?
  #:new-symbol 'skipTypesetting
  #:new->old identity
  #:warning "All mimsy were þe borogoves")

{
  \applyContext
  #(lambda (ctx)
    (ly:context-property ctx 'deprecatedScmGetSpecialWarning))
  s
}
