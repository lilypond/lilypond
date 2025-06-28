\version "2.25.27"

\header {
  texidoc = "This is a set of tests of the @code{beat-structure} Scheme
function.  This set includes cases where the time signature is @code{#f}
(for senza misura).  Problems are reported in the standard error stream."
}

\include "testing-functions.ily"
#(ly:set-option 'warning-as-error #t)

testSettingsWithoutStructure = #'(
  (#f . ())
)

testSettingsWithBase = #'(
  (#f . ((beatBase . 1/2)
         (beatStructure . (1 1))))
)

#(expect-equal "signature not found in settings; default base"
  (beat-structure +inf.0 #f '())
  '())

#(expect-equal "structure not found in settings; default base"
  (beat-structure +inf.0 #f testSettingsWithoutStructure)
  '())

#(expect-equal "structure and base found in settings; same base"
  (beat-structure 1/2 #f testSettingsWithBase)
  '(1 1))
