\version "2.25.32"

\header {
  texidoc = "A warning is issued when simultaneous @code{\\time} and
@code{\\polymetric \\time} commands require different measure lengths.  In this
case, both the @code{Score} and @code{Staff} time signatures are 4/4, but the
@code{Staff} meter is scaled by a factor of 5/4."
}

#(ly:set-option 'warning-as-error #t)

\score {
  \layout {}
  \midi {}
  %% Related test cases check that there is no dependence on the order in which
  %% \time and \polymetric \time appear in the source.  This test is simpler
  %% because it focuses narrowly on proving sensitivity to \scaleDurations.
  \new Staff {
    \applyContext #(lambda (context)
                    (ly:expect-warning
                     (ly:translate-cpp-warning-scheme
                      "conflicting measure length: %s") 5/4)
                    (ly:expect-warning
                     (ly:translate-cpp-warning-scheme
                      "measure length in Timing context: %s") 1))

    \scaleDurations 5/4 {
      \context Staff \polymetric \time 4/4

      \contextPropertyCheck Score.beamExceptions
      #'((end . ((1/8 . (4 4))
                 (1/12 . (3 3 3 3)))))
      \contextPropertyCheck Score.beatBase #1/4
      \contextPropertyCheck Score.beatStructure #'(1 1 1 1)
      \contextPropertyCheck Score.measureLength 1
      \contextPropertyCheck Score.meterScalingFactor \default
      \contextPropertyCheck Score.timeSignature 4/4

      %% notwithstanding the warning, the settings take effect
      \contextPropertyCheck Staff.beamExceptions
      #'((end . ((1/8 . (4 4))
                 (1/12 . (3 3 3 3)))))
      \contextPropertyCheck Staff.beatBase #1/4
      \contextPropertyCheck Staff.beatStructure #'(1 1 1 1)
      \contextPropertyCheck Staff.measureLength \default
      \contextPropertyCheck Staff.meterScalingFactor #5/4
      \contextPropertyCheck Staff.timeSignature 4/4

      \repeat unfold 4 r4
    }

    \applyContext #(lambda (context) (ly:check-expected-warnings))
  }
}
