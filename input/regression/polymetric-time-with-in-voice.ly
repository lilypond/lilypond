\version "2.25.32"

\header {
  texidoc = "@code{\\polymetric \\time} may be used in a new @code{Voice}'s
@code{\\with} block to set timing properties in that context.  The
@code{\\polymetric \\default} command can later unset them.

Both staves should indicate common time.  Beaming should follow the conventions
for common time, except that the first measure of the bass voice should be
beamed for 8/8 time."
}

#(ly:set-option 'warning-as-error #t)

checkStaffNonPolymetric =
#(define-music-function () () #{
  \contextPropertyCheck Staff.beamExceptions \default
  \contextPropertyCheck Staff.beatBase \default
  \contextPropertyCheck Staff.beatStructure \default
  \contextPropertyCheck Staff.measureLength \default
  \contextPropertyCheck Staff.meterScalingFactor \default
  \contextPropertyCheck Staff.submeasureStructure \default
  \contextPropertyCheck Staff.timeSignature \default
#})

checkVoiceNonPolymetric =
#(define-music-function () () #{
  \contextPropertyCheck Voice.beamExceptions \default
  \contextPropertyCheck Voice.beatBase \default
  \contextPropertyCheck Voice.beatStructure \default
  \contextPropertyCheck Voice.measureLength \default
  \contextPropertyCheck Voice.meterScalingFactor \default
  \contextPropertyCheck Voice.submeasureStructure \default
  \contextPropertyCheck Voice.timeSignature \default
#})

\score {
  \layout {}
  \midi {}
  <<
    \new Staff \fixed c' {
      \new Voice {
        \checkStaffNonPolymetric
        \checkVoiceNonPolymetric
        \repeat unfold 8 c8
        \checkStaffNonPolymetric
        \checkVoiceNonPolymetric
        \repeat unfold 8 c8
      }
    }
    \new Staff \with {
      \clef "bass"
    } \fixed c <<
      \new Voice \with {
        \voiceTwo
        \polymetric \time 8/8
      } {
        \checkStaffNonPolymetric

        \contextPropertyCheck Voice.beamExceptions #'()
        \contextPropertyCheck Voice.beatBase #1/8
        \contextPropertyCheck Voice.beatStructure #'(3 3 2)
        \contextPropertyCheck Voice.measureLength \default
        \contextPropertyCheck Voice.meterScalingFactor 1
        \contextPropertyCheck Voice.submeasureStructure #'(8)
        \contextPropertyCheck Voice.timeSignature 8/8

        \repeat unfold 8 c8

        \polymetric \default % implicitly in Voice context already
        \checkStaffNonPolymetric
        \checkVoiceNonPolymetric

        \repeat unfold 8 c8
      }
      %% Placing voiceOne and voiceTwo in reverse order in the source is hoped
      %% to be more sensitive to bugs.  The \polymetric \time in voiceTwo above
      %% should have no affect on voiceOne below.
      \new Voice \with {
        \voiceOne
      } {
        \checkStaffNonPolymetric
        \checkVoiceNonPolymetric
        \repeat unfold 8 e8
        \checkStaffNonPolymetric
        \checkVoiceNonPolymetric
        \repeat unfold 8 e8
      }
    >>
  >>
}
