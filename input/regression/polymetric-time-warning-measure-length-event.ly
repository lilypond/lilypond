\version "2.25.32"

\header {
  texidoc = "A warning is issued when simultaneous @code{\\time} and
@code{\\polymetric \\time} commands require different measure lengths.  In this
case, the @code{Score} time signature is 5/4 and the @code{Staff} time signature
is 5/8."
}

#(ly:set-option 'warning-as-error #t)

checks =
#(define-music-function () () #{
  \contextPropertyCheck Score.beamExceptions #'()
  \contextPropertyCheck Score.beatBase #1/4
  \contextPropertyCheck Score.beatStructure #'(1 1 1 1 1)
  \contextPropertyCheck Score.measureLength #5/4
  \contextPropertyCheck Score.meterScalingFactor \default
  \contextPropertyCheck Score.submeasureStructure #'(5)
  \contextPropertyCheck Score.timeSignature 5/4

  \contextPropertyCheck Staff.beamExceptions #'()
  \contextPropertyCheck Staff.beatBase #1/8
  \contextPropertyCheck Staff.beatStructure #'(3 2)
  \contextPropertyCheck Staff.measureLength \default
  \contextPropertyCheck Staff.meterScalingFactor 1
  \contextPropertyCheck Staff.submeasureStructure #'(5)
  \contextPropertyCheck Staff.timeSignature 5/8
#})

\score {
  \layout {}
  \midi {}
  \new Staff {
    %% no warning even though \time and \partial come later
    \context Staff \polymetric \time 3/4
    \partial 8*3
    \time 6/8
    \context Staff \polymetric \time 3/4 % no warning here either, of course
    r4.

    \applyContext #(lambda (context)
                    (ly:expect-warning
                     (ly:translate-cpp-warning-scheme
                      "conflicting measure length: %s") 5/8)
                    (ly:expect-warning
                     (ly:translate-cpp-warning-scheme
                      "measure length in Timing context: %s") 5/4))

    \time 5/4
    \context Staff \polymetric \time 5/8
    \checks % notwithstanding the warning, the settings take effect

    \repeat unfold 5 r4

    \applyContext #(lambda (context) (ly:check-expected-warnings))

    %% switch back to a compatible meter
    \time 6/8
    \context Staff \polymetric \time 3/4
    R2.

    \applyContext #(lambda (context)
                    (ly:expect-warning
                     (ly:translate-cpp-warning-scheme
                      "conflicting measure length: %s") 5/8)
                    (ly:expect-warning
                     (ly:translate-cpp-warning-scheme
                      "measure length in Timing context: %s") 5/4))

    %% test the same commands in the reverse of the above order
    \context Staff \polymetric \time 5/8
    \time 5/4
    \checks % notwithstanding the warning, the settings take effect

    \repeat unfold 5 r4

    \applyContext #(lambda (context) (ly:check-expected-warnings))
  }
}
