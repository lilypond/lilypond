
\header {
texidoc = "If possible clef changes and the like 
should be folded under notes of the different staff
"
}
 
\score { \notes \relative c'' < \context Staff = SA {
c4 c16 c c c  cis4 cis4
c4 c4 c4 c


 }
   { \key d \major c2
     \clef bass
     c,2 c cis2  } >

   \paper { linewidth = -1. } 
 }


