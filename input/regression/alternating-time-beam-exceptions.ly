\version "2.25.33"

\header {
  texidoc = "The beat structure and beaming exceptions for a component of a
strictly alternating time signature are the same as when it appears alone."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  indent = 0
  \enablePerStaffTiming
}

\fixed c' <<
  \new Staff {
    \time 2/2 \repeat unfold 8 { 8 }
    \time 4/4 \repeat unfold 8 { 8 }
    \time 3/8 \repeat unfold 3 { 8 }

    \time 2/2 \repeat unfold 16 { 16 }
    \time 4/4 \repeat unfold 16 { 16 }
    \time 3/8 \repeat unfold 6 { 16 }

    \time 2/2 \repeat unfold 32 { 32 }
    \time 4/4 \repeat unfold 32 { 32 }
    \time 3/8 \repeat unfold 12 { 32 }
  }
  \new Staff {
    \timeAbbrev #'((2 2) (4 4) (3 8))
    \contextPropertyCheck Timing.beatBase #1/8
    \contextPropertyCheck Timing.beatStructure #'(4 4  2 2 2 2  1 1 1)
    \contextPropertyCheck Timing.submeasureStructure #'(8 8 3)
    \contextPropertyCheck Timing.measureLength #19/8
    \contextPropertyCheck Timing.timeSignature #'((2 . 2) (4 . 4) (3 . 8))
    \contextPropertyCheck Timing.beamExceptions
    #'((end . ((1/8 .  (4 4      4 4      3))
               (1/12 . (6 6      3 3 3 3  9/2))
               (1/32 . (8 8 8 8  8 8 8 8  12)))))

    \repeat unfold 8 { 8 } \allowBreak
    \repeat unfold 8 { 8 } \allowBreak
    \repeat unfold 3 { 8 } \allowBreak

    \repeat unfold 16 { 16 } \allowBreak
    \repeat unfold 16 { 16 } \allowBreak
    \repeat unfold 6 { 16 } \allowBreak

    \repeat unfold 32 { 32 } \allowBreak
    \repeat unfold 32 { 32 } \allowBreak
    \repeat unfold 12 { 32 } \allowBreak
  }
>>
