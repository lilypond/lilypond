\version "2.25.32"

\header {
  texidoc = "@code{\\polymetric \\default} is ignored with a warning when issued
in a @code{Timing} context.  In this case, the command is issued in @code{Staff}
context after @code{\\enablePerStaffTiming}.  The output should have 6/8 time."
}

#(ly:set-option 'warning-as-error #t)

testMusic = \new Staff \fixed c' {
  \time 6/8

  \tag "layout" {
    \contextPropertyCheck Score.beamExceptions \default
    \contextPropertyCheck Score.beatBase \default
    \contextPropertyCheck Score.beatStructure \default
    \contextPropertyCheck Score.measureLength \default
    \contextPropertyCheck Score.meterScalingFactor \default
    \contextPropertyCheck Score.submeasureStructure \default
    \contextPropertyCheck Score.timeSignature 4/4

    \contextPropertyCheck Staff.beamExceptions #'()
    \contextPropertyCheck Staff.beatBase #1/8
    \contextPropertyCheck Staff.beatStructure #'(3 3)
    \contextPropertyCheck Staff.measureLength #6/8
    \contextPropertyCheck Staff.meterScalingFactor \default
    \contextPropertyCheck Staff.submeasureStructure #'(6)
    \contextPropertyCheck Staff.timeSignature 6/8
  }
  \tag "midi" {
    \contextPropertyCheck Score.beamExceptions #'()
    \contextPropertyCheck Score.beatBase #1/8
    \contextPropertyCheck Score.beatStructure #'(3 3)
    \contextPropertyCheck Score.measureLength #6/8
    \contextPropertyCheck Score.meterScalingFactor \default
    \contextPropertyCheck Score.submeasureStructure #'(6)
    \contextPropertyCheck Score.timeSignature 6/8

    \contextPropertyCheck Staff.beamExceptions \default
    \contextPropertyCheck Staff.beatBase \default
    \contextPropertyCheck Staff.beatStructure \default
    \contextPropertyCheck Staff.measureLength \default
    \contextPropertyCheck Staff.meterScalingFactor \default
    \contextPropertyCheck Staff.submeasureStructure \default
    \contextPropertyCheck Staff.timeSignature \default
  }

  \tag "layout" {
    \applyContext #(lambda (context)
                    (ly:expect-warning (G_ "ignoring in Timing context")))
  }

  \polymetric \default

  \applyContext #(lambda (context) (ly:check-expected-warnings))

  \tag "layout" {
    \contextPropertyCheck Score.beamExceptions \default
    \contextPropertyCheck Score.beatBase \default
    \contextPropertyCheck Score.beatStructure \default
    \contextPropertyCheck Score.measureLength \default
    \contextPropertyCheck Score.meterScalingFactor \default
    \contextPropertyCheck Score.submeasureStructure \default
    \contextPropertyCheck Score.timeSignature 4/4

    \contextPropertyCheck Staff.beamExceptions #'()
    \contextPropertyCheck Staff.beatBase #1/8
    \contextPropertyCheck Staff.beatStructure #'(3 3)
    \contextPropertyCheck Staff.measureLength #6/8
    \contextPropertyCheck Staff.meterScalingFactor \default
    \contextPropertyCheck Staff.submeasureStructure #'(6)
    \contextPropertyCheck Staff.timeSignature 6/8
  }
  \tag "midi" {
    \contextPropertyCheck Score.beamExceptions #'()
    \contextPropertyCheck Score.beatBase #1/8
    \contextPropertyCheck Score.beatStructure #'(3 3)
    \contextPropertyCheck Score.measureLength #6/8
    \contextPropertyCheck Score.meterScalingFactor \default
    \contextPropertyCheck Score.submeasureStructure #'(6)
    \contextPropertyCheck Score.timeSignature 6/8

    \contextPropertyCheck Staff.beamExceptions \default
    \contextPropertyCheck Staff.beatBase \default
    \contextPropertyCheck Staff.beatStructure \default
    \contextPropertyCheck Staff.measureLength \default
    \contextPropertyCheck Staff.meterScalingFactor \default
    \contextPropertyCheck Staff.submeasureStructure \default
    \contextPropertyCheck Staff.timeSignature \default
  }

  \repeat unfold 6 c8
}

\score {
  \layout { \enablePerStaffTiming }
  \keepWithTag "layout" \testMusic
}

%% \enablePerStaffTiming is not expected to be used for MIDI output because the
%% MIDI file format supports only a global time signature.  This accounts for
%% the conditional expectations above.
\score {
  \midi {}
  \keepWithTag "midi" \testMusic
}
