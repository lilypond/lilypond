\header {
  texidoc = "cross staff beams work with collisions."
}

\version "2.19.21"

<<
 \new Staff = "PianoRH" s4.
 \new Staff = "PianoLH" {
   d''8 [b''! \change Staff = "PianoRH"  d'' ]
 }
>>

<<
  \new Staff = up \relative {
    c'8 c c c
    c c c c
    b' b b b
  }
  \new Staff = down \relative {
    s8 c' c \change Staff = up c
    \change Staff = down c [ c s16 \change Staff = up a'16 s16 a16 ]
    \stemUp
    \change Staff = down b8 b b b \change Staff = up
  }
>>
