\version "2.25.32"

\header {
  texidoc = "A warning is issued when a @code{Staff} @code{\\with} block
specifies a polymetric measure length that conflicts with the @code{Score}.  In
this case, the @code{Score} time signature is 5/4 and the @code{Staff} time
signature is 5/8."
}

#(ly:set-option 'warning-as-error #t)

\score {
  \layout {}
  \midi {}
  {
    \applyContext #(lambda (context)
                    (ly:expect-warning
                     (ly:translate-cpp-warning-scheme
                      "conflicting measure length: %s") 5/8)
                    (ly:expect-warning
                     (ly:translate-cpp-warning-scheme
                      "measure length in Timing context: %s") 5/4))

    \new Score \with { \time 5/4 } <<
      \new Staff \with { \polymetric \time 5/8 } {
        \contextPropertyCheck Score.beamExceptions #'()
        \contextPropertyCheck Score.beatBase #1/4
        \contextPropertyCheck Score.beatStructure #'(1 1 1 1 1)
        \contextPropertyCheck Score.measureLength #5/4
        \contextPropertyCheck Score.meterScalingFactor \default
        \contextPropertyCheck Score.timeSignature 5/4

        \contextPropertyCheck Staff.beamExceptions #'()
        \contextPropertyCheck Staff.beatBase #1/8
        \contextPropertyCheck Staff.beatStructure #'(3 2)
        \contextPropertyCheck Staff.measureLength \default
        \contextPropertyCheck Staff.meterScalingFactor 1
        \contextPropertyCheck Staff.timeSignature 5/8

        \repeat unfold 5 r4
      }
    >>

    \applyContext #(lambda (context) (ly:check-expected-warnings))
  }
}
