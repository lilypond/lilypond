\version "2.25.25"

\header {
  texidoc = "This test sweeps various @code{TimeSignature} grob properties
controlling how fractional parts are printed.  In all cases, the value of
@code{nested-fraction-mixed} is set to @code{#t}."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  \context {
    \Score
    \override TimeSignature.nested-fraction-mixed = ##t
  }
}

\include "time-signature-grob-nested-fraction-mixed.ily"
