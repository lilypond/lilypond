\version "2.25.32"

\header {
  texidoc = "@code{\\polymetric \\default} is ignored with a warning when issued
in a @code{Timing} context.  In this case, the command is issued in the default
@code{Score} context.  The output should have 6/8 time."
}

#(ly:set-option 'warning-as-error #t)

\score {
  \layout {}
  \midi {}
  \fixed c' {
    \time 6/8

    \contextPropertyCheck Score.beamExceptions #'()
    \contextPropertyCheck Score.beatBase #1/8
    \contextPropertyCheck Score.beatStructure #'(3 3)
    \contextPropertyCheck Score.measureLength #6/8
    \contextPropertyCheck Score.meterScalingFactor \default
    \contextPropertyCheck Score.timeSignature 6/8

    \applyContext #(lambda (context)
                    (ly:expect-warning (G_ "ignoring in Timing context")))

    \polymetric \default % warns; leaves properties alone

    \applyContext #(lambda (context) (ly:check-expected-warnings))

    \contextPropertyCheck Score.beamExceptions #'()
    \contextPropertyCheck Score.beatBase #1/8
    \contextPropertyCheck Score.beatStructure #'(3 3)
    \contextPropertyCheck Score.measureLength #6/8
    \contextPropertyCheck Score.meterScalingFactor \default
    \contextPropertyCheck Score.timeSignature 6/8

    \repeat unfold 6 c8
  }
}
