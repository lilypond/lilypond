\header {
  texidoc = "cross staff beams work with collisions."
}

\version "2.13.55"

<< 
 \new Staff = "PianoRH" s4.
 \new Staff = "PianoLH" {
   d''8 [b''! \change Staff = "PianoRH"  d'' ]
 }
>>
