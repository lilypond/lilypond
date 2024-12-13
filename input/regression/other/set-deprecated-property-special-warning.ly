\version "2.25.23"

\header {
  texidoc = "Setting a deprecated property may issue a special warning."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning "And þe mome raths outgrabe.")

%% This ad-hoc property name should be unique within the full set of test cases
%% because the deprecation warning is issued only once per lilypond process.  If
%% multiple tests are run in the same process, they won't all be able to observe
%% a warning for the same property.
#(define-deprecated-property
  'translation-type? 'deprecatedLySetSpecialWarning boolean?
  #:new-symbol 'skipTypesetting
  #:old->new identity
  #:warning "And þe mome raths outgrabe.")

{
  \set Timing.deprecatedLySetSpecialWarning = ##f
  s
}
