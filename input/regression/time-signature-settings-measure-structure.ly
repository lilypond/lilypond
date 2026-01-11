\version "2.25.32"

\header {
  texidoc = "The structure argument of @code{\\overrideTimeSignatureSettings}
may group beats into larger submeasure divisions.  In this test, dotted bar
lines should divide the notes of the measure into groups of 3, 4, 6,
and@tie{}2."
}

\new Staff {
  \submeasureBarsOn
  \overrideTimeSignatureSettings
  15/8                    % timeSignature
  #1/8                    % beatBase
  #'((3) (2 2) (3 3) (2)) % structure
  #'()                    % beamExceptions
  \time 15/8
  \repeat unfold 15 {c''8}
}
