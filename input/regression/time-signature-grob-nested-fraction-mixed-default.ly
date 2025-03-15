\version "2.25.25"

\header {
  texidoc = "This test sweeps various @code{TimeSignature} grob properties
controlling how fractional parts are printed.  In all cases, the value of
@code{nested-fraction-mixed} is left at the default."
}

#(ly:set-option 'warning-as-error #t)

\include "time-signature-grob-nested-fraction-mixed.ily"
