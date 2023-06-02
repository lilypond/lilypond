\version "2.25.6"

\header {
  texidoc = "Setting @code{Timing@/.timeSignatureFraction} to @code{#f} prints a
special sign in place of a time signature, provided that @code{TimeSignature} is
appropriately configured.  The first and third measures should have an X-shaped
sign."
}

\new Staff \with {
  \senzaMisuraTimeSignatureX
} \fixed c' {
  \set Timing.timeSignatureFraction = ##f
  b1
  \set Timing.timeSignatureFraction = #'(2 . 2)
  b1
  \set Timing.timeSignatureFraction = ##f
  b1
}
