\version "1.5.68"

\header { 
texidoc = "Accidentals in different staffs don't effect the
spacing of the quarter notes here."
}

\score { \notes \relative c'' < \context Staff = SA { \time 4/4

[c8 c8 cis8 cis8]
[cis8 cis8 cis8 cis]


 }
   { \key d \major cis4 cis4 cis4 cis!4  } >

   \paper { linewidth = -1. } 
 }


