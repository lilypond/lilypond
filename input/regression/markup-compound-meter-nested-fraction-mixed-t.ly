\version "2.25.25"

\header {
  texidoc = "This test sweeps various properties controlling how
@code{\\compound-meter} prints fractional parts.  In all cases, the value of
@code{nested-fraction-mixed} is set to @code{#t}."
}

#(ly:set-option 'warning-as-error #t)
#(define-markup-command (test-override layout props arg)
  (markup?)
  (interpret-markup layout props
   (make-override-markup '(nested-fraction-mixed . #t) arg)))

\include "markup-compound-meter-nested-fraction.ily"
