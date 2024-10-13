\version "2.25.22"

\header {
  texidoc = "@code{ly:context-property} works on a deprecated context property.
Odd-numbered measures should be marked with circles and even-numbered measures
should be marked with boxes."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "the property '%s' is deprecated; use '%s'")
  "deprecatedScmGet" "currentBarNumber")

#(define-public (num->mk-formatter n)
  (if (and (number? n) (zero? (modulo n 2)))
   format-mark-box-numbers
   format-mark-circle-numbers))

%% This ad-hoc property name should be unique within the full set of test cases
%% because the deprecation warning is issued only once per lilypond process.  If
%% multiple tests are run in the same process, they won't all be able to observe
%% a warning for the same property.
#(define-deprecated-property
  'translation-type? 'deprecatedScmGet procedure?
  #:new-symbol 'currentBarNumber
  #:new->old num->mk-formatter)

\fixed c' {
  \repeat unfold 4 {
    \context Score {
      \applyContext
      #(lambda (ctx)
        (ly:context-set-property! ctx 'rehearsalMarkFormatter
         (ly:context-property ctx 'deprecatedScmGet)))
    }
    \mark \default
    R1
  }
}
