\version "2.25.32"

\header {
  texidoc = "A warning is issued when simultaneous @code{\\time} and
@code{\\polymetric \\time} commands require different measure lengths, and
setting an irregular @code{measureLength} cannot defeat the warning, because it
is based on the regular measure lengths of the given time signatures.  In this
case, the @code{Score} time signature is 2/2 and the @code{Staff} time signature
is 6/8."
}

#(ly:set-option 'warning-as-error #t)

\score {
  \layout {}
  \midi {}
  \new Staff \fixed c' {
    \applyContext #(lambda (context)
                    (ly:expect-warning
                     (ly:translate-cpp-warning-scheme
                      "conflicting measure length: %s") 3/4)
                    (ly:expect-warning
                     (ly:translate-cpp-warning-scheme
                      "measure length in Timing context: %s") 1))

    \time 2/2
    \context Staff \polymetric \time 6/8
    \set Timing.measureLength = #3/4
    \once \set Staff.measureLength = #3/4

    \contextPropertyCheck Score.beamExceptions #'((end . ((1/32 . (8 8 8 8)))))
    \contextPropertyCheck Score.beatBase #1/2
    \contextPropertyCheck Score.beatStructure #'(1 1)
    \contextPropertyCheck Score.measureLength #3/4
    \contextPropertyCheck Score.meterScalingFactor \default
    \contextPropertyCheck Score.timeSignature 2/2

    \contextPropertyCheck Staff.beamExceptions #'()
    \contextPropertyCheck Staff.beatBase #1/8
    \contextPropertyCheck Staff.beatStructure #'(3 3)
    \contextPropertyCheck Staff.measureLength #3/4
    \contextPropertyCheck Staff.meterScalingFactor 1
    \contextPropertyCheck Staff.timeSignature 6/8

    \repeat unfold 6 c8

    \applyContext #(lambda (context) (ly:check-expected-warnings))
  }
}
