\version "2.25.35"

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
    \time 2/2 \*8 { 8 }
    \time 4/4 \*8 { 8 }
    \time 3/8 \*3 { 8 }

    \time 2/2 \*16 { 16 }
    \time 4/4 \*16 { 16 }
    \time 3/8 \*6 { 16 }

    \time 2/2 \*32 { 32 }
    \time 4/4 \*32 { 32 }
    \time 3/8 \*12 { 32 }
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

    \*8 { 8 } \allowBreak
    \*8 { 8 } \allowBreak
    \*3 { 8 } \allowBreak

    \*16 { 16 } \allowBreak
    \*16 { 16 } \allowBreak
    \*6 { 16 } \allowBreak

    \*32 { 32 } \allowBreak
    \*32 { 32 } \allowBreak
    \*12 { 32 } \allowBreak
  }
>>
