
\header { 
texidoc = "Accidentals in different staffs don't effect the
spacing of the quarter notes here."
}

\score { \notes \relative c'' < \context Staff = SA {
c4 c4 cis4 cis4
c4 c4 c4 c


 }
   { \key d \major c2 c2 c2 cis2  } >

   \paper { linewidth = -1. } 
 }


