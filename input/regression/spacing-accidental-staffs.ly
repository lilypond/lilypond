
\header { 
texidoc = "Accidentals in different staffs don't effect the
spacing of the quarter notes here."
}

\score { \notes \relative c'' < \context Staff = SA { \time 8/4

c4 c4 cis4 cis4
cis4 cis4 cis4 cis


 }
   { \key d \major cis2 cis2 cis2 cis!2  } >

   \paper { linewidth = -1. } 
 }


