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

testSettingsWithoutBase = #'(
  (#f . ((beatStructure . (1 1))))
)

testSettingsWithBase = #'(
  (#f . ((beatBase . 1/2)
         (beatStructure . (1 1))))
)

#(expect-equal "signature not found in settings; default base"
  (beat-structure +inf.0 #f '())
  '())
#(expect-equal "signature not found in settings; custom base"
  (beat-structure 1/4 #f '())
  '(+inf.0))
%% robustness test
#(expect-equal "signature not found in settings; zero base"
  (beat-structure 0 #f '())
  '(+inf.0))

#(expect-equal "structure not found in settings; default base"
  (beat-structure +inf.0 #f testSettingsWithoutStructure)
  '())
#(expect-equal "structure not found in settings; custom base"
  (beat-structure 1/4 #f testSettingsWithoutStructure)
  '(+inf.0))
%% robustness test
#(expect-equal "structure not found in settings; zero base"
  (beat-structure 0 #f testSettingsWithoutStructure)
  '(+inf.0))

#(expect-equal "structure found in settings; default base"
  (beat-structure +inf.0 #f testSettingsWithoutBase)
  '())
#(expect-equal "structure found in settings; custom base"
  (beat-structure 1/4 #f testSettingsWithoutBase)
  '(+inf.0))
%% robustness test
#(expect-equal "structure found in settings; zero base"
  (beat-structure 0 #f testSettingsWithoutBase)
  '(+inf.0))

#(expect-equal "structure and base found in settings; same base"
  (beat-structure 1/2 #f testSettingsWithBase)
  '(1 1))
#(expect-equal "structure and base found in settings; different base"
  (beat-structure 1/4 #f testSettingsWithBase)
  '(2 2))
#(expect-equal "structure and base found in settings; senza-misura base"
  (beat-structure +inf.0 #f testSettingsWithBase)
  '())
%% robustness test
#(expect-equal "structure and base found in settings; zero base"
  (beat-structure 0 #f testSettingsWithBase)
  '(+inf.0))
