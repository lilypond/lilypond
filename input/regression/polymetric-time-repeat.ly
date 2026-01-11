\version "2.25.32"

\header {
  texidoc = "It is possible (though complicated) to write an unfoldable
@code{\\repeat volta} with an aligned polymeter change in an alternative.

The folded and unfolded renditions of this test case should agree."
}

#(ly:set-option 'warning-as-error #t)

checkScoreTwoEight =
#(define-music-function () () #{
  \contextPropertyCheck Score.beamExceptions
  #'((end . ((1/8 . (2)))))
  \contextPropertyCheck Score.beatBase #1/8
  \contextPropertyCheck Score.beatStructure #'(1 1)
  \contextPropertyCheck Score.meterScalingFactor \default
  \contextPropertyCheck Score.submeasureStructure #'(2)
  \contextPropertyCheck Score.timeSignature 2/8
#})

checkStaffNonPolymetric =
#(define-music-function () () #{
  \contextPropertyCheck Staff.beamExceptions \default
  \contextPropertyCheck Staff.beatBase \default
  \contextPropertyCheck Staff.beatStructure \default
  \contextPropertyCheck Staff.meterScalingFactor \default
  \contextPropertyCheck Staff.submeasureStructure \default
  \contextPropertyCheck Staff.timeSignature \default
#})

checkStaffScaledThreeEight =
#(define-music-function () () #{
  \contextPropertyCheck Staff.beamExceptions
  #'((end . ((1/8 . (3)))))
  \contextPropertyCheck Staff.beatBase #1/8
  \contextPropertyCheck Staff.beatStructure #'(1 1 1)
  \contextPropertyCheck Staff.meterScalingFactor #2/3
  \contextPropertyCheck Staff.submeasureStructure #'(3)
  \contextPropertyCheck Staff.timeSignature 3/8
#})

music = \fixed c' <<
  \time 2/8
  \context Staff = "RH" {
    \repeat volta 2 {
      \repeat unfold 2 c8 |
      \checkScoreTwoEight \checkStaffNonPolymetric
      \alternative {
        \volta 1 {
          \repeat unfold 2 d8 |
          \repeat unfold 2 d8 |
        }
        \volta 2 {
          \volta #'() { \checkScoreTwoEight \checkStaffNonPolymetric }
          \repeat unfold 2 e8 |
        }
      }
    }
    \repeat unfold 2 f8 |
    \repeat unfold 2 f8 |
  }
  \context Staff = "LH" {
    \scaleDurations 2/3 { \context Staff \polymetric \time 3/8 }
    \checkScoreTwoEight \checkStaffScaledThreeEight
    \repeat volta 2 {
      \scaleDurations 2/3 { \repeat unfold 3 c8 }
      \alternative {
        \volta 1 {
          \scaleDurations 2/3 { \repeat unfold 3 c8 } |
          \context Staff \polymetric \default % back to 2/8 from Timing
          \checkScoreTwoEight \checkStaffNonPolymetric
          \repeat unfold 2 c8 |
          %% return to 3/8 for repeat
          \scaleDurations 2/3 { \context Staff \polymetric \time 3/8 }
          \checkScoreTwoEight \checkStaffScaledThreeEight
        }
        \volta 2 {
          \scaleDurations 2/3 { \repeat unfold 3 c8 } |
        }
      }
    }
    \scaleDurations 2/3 { \repeat unfold 3 c8 } |
    \context Staff \polymetric \default % back to 2/8 from Timing
    \checkScoreTwoEight \checkStaffNonPolymetric
    \repeat unfold 2 c8 |
  }
>>

\score { \music }
\score {
  \unfoldRepeats {
    \context Staff = "RH" { \sectionLabel "Unfolded" }
    \music
  }
}
