\version "1.7.18"
\header {
texidoc = "Oppositely stemmed chords,  meshing into each other,
are resolved."
}

\score {
  \context Staff \notes \transpose c c' {
     \time 3/4
     % Here's what I was trying to do:
     < \context Voice  = VI {\stemDown
       <<g b g'>>4
       <<g b g'>>4.       
       <<g b d'>>4
       d'4
       d'4.       
       }
       
       \context Voice=VII {\stemUp
         d'4
         d'4.	 
	 <<a c'>>4
	 <<g b g'>>4
	 <<g b g'>>4.	 	 
	   } >

  }
}





%% new-chords-done %%