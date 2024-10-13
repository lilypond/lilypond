\version "2.25.22"

\header {
  texidoc = "@code{ly:context-set-property!} works on a deprecated context
property.  The output should be a single measure with a B@tie{}note."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "the property '%s' is deprecated; use '%s'")
  "deprecatedScmSet" "skipTypesetting")

%% This ad-hoc property name should be unique within the full set of test cases
%% because the deprecation warning is issued only once per lilypond process.  If
%% multiple tests are run in the same process, they won't all be able to observe
%% a warning for the same property.
#(define-deprecated-property
  'translation-type? 'deprecatedScmSet boolean?
  #:new-symbol 'skipTypesetting
  #:old->new not)

\fixed c' {
  \context Timing {
    \applyContext
    #(lambda (ctx)
      (ly:context-set-property! ctx 'deprecatedScmSet #f))
  }

  c1^"FAIL"

  %% We set the deprecated property again, but lilypond warns only once.
  \context Timing {
    \applyContext
    #(lambda (ctx)
      (ly:context-set-property! ctx 'deprecatedScmSet #t))
  }

  b1
}
