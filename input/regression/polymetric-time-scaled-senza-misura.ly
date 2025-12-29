\version "2.25.32"

\header {
  texidoc = "This test defines a @code{\\senzaMisura} command (which may be
added to LilyPond once all required infrastructure is ready) and demonstrates
that @code{\\polymetric \\senzaMisura} can be used to set scalable timing
properties in a local context just as @code{\\polymetric \\time} can.

No time signature or bar lines appear in the output.  The notes within the
phrasing slur are scaled to 2/3 of their written duration, but they are beamed
in pairs like the unscaled notes (not as triplets)."
}

#(ly:set-option 'warning-as-error #t)

#(when (defined? 'senzaMisura) (error "update this test"))
senzaMisura =
#(define-music-function () ()
  (make-music 'ReferenceTimeSignatureMusic 'time-signature #f))

\new Score \with {
  forbidBreakBetweenBarLines = ##f
  %% Allow some beams to help reviewers count notes.
  \overrideTimeSignatureSettings
    ##f       % timeSignature
    #1/4      % beatBase
    #'(1)     % beatStructure
    #'()      % beamExceptions
} <<
  \new Staff {
    \senzaMisura

    \contextPropertyCheck Timing.beamExceptions #'()
    \contextPropertyCheck Timing.beatBase #1/4
    \contextPropertyCheck Timing.beatStructure #'(1)
    \contextPropertyCheck Timing.measureLength #+inf.0
    \contextPropertyCheck Timing.meterScalingFactor \default

    \contextPropertyCheck Staff.beamExceptions \default
    \contextPropertyCheck Staff.beatBase \default
    \contextPropertyCheck Staff.beatStructure \default
    \contextPropertyCheck Staff.measureLength \default
    \contextPropertyCheck Staff.meterScalingFactor \default

    \repeat unfold 12 c'8
  }
  \new Staff {
    \repeat unfold 4 c'8
    \scaleDurations 2/3 {
      \context Staff \polymetric \senzaMisura
      \contextPropertyCheck Staff.beamExceptions #'()
      \contextPropertyCheck Staff.beatBase #1/4
      \contextPropertyCheck Staff.beatStructure #'(1)
      \contextPropertyCheck Staff.measureLength \default
      \contextPropertyCheck Staff.meterScalingFactor #2/3
      c'8\( \repeat unfold 4 c'8 c'8\)
      \context Staff \polymetric \default
    }
    \repeat unfold 4 c'8
  }
>>
