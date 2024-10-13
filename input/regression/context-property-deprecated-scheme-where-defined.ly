\version "2.25.22"

\header {
  texidoc = "@code{ly:context-property-where-defined} works on a deprecated
context property.  A coda sign should appear at the start of measure@tie{}2."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "the property '%s' is deprecated; use '%s'")
  "deprecatedScmWhere" "internalBarNumber")

%% This ad-hoc property name should be unique within the full set of test cases
%% because the deprecation warning is issued only once per lilypond process.  If
%% multiple tests are run in the same process, they won't all be able to observe
%% a warning for the same property.
#(define-deprecated-property
  'translation-type? 'deprecatedScmWhere procedure?
  #:new-symbol 'internalBarNumber
  #:new->old identity)

\fixed c' {
  R1
  \applyContext
  #(lambda (ctx)
    (let ((t-ctx (ly:context-property-where-defined ctx 'deprecatedScmWhere)))
     (ly:context-set-property! t-ctx 'rehearsalMarkFormatter format-coda-mark)))
  \mark \default
  R1
}
