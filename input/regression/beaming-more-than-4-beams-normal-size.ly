
\version "2.21.0"

\header{
texidoc="
Inside-staff beams should align with staff lines (sit, straddle, hang) as
smoothly as possible (standard-sized beams).  The outside-staff beams do not
interfere with staff lines, so the inside-staff beams are more important when
it comes to beam quanting/@/scoring/@/positioning."
}

\layout {
  indent = 0
  ragged-right = ##t
  \context {
    \Staff
    \omit Clef
    \omit TimeSignature
  }
}

testMusic = {
  \cadenzaOn
   a8[ 8]
   16[ 16]
   32[ 32]
   64[ 64]
   128[ 128]
   256[ 256]
   512[ 512]
   1024[ 1024]
   128[ 64 32 16 8]
   8[ 128 128 16]
}

<<
  \new Staff \testMusic
  \new Staff \transpose a c''' \testMusic
>>
