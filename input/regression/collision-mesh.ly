\version "1.5.68"
\header {
texidoc = "Oppositely stemmed chords,  meshing into each other,
are resolved."
}

\score {
  \context Staff \notes \transpose c'' {
     \time 3/4
     % Here's what I was trying to do:
     < \context Voice  = VI {\stemDown
       <g4 b g'>
       <g4. b g'>       
       <g4 b d'>
       d'4
       d'4.       
       }
       
       \context Voice=VII {\stemUp
         d'4
         d'4.	 
	 <a4 c'>
	 <g4 b g'>
	 <g4. b g'>	 	 
	   } >

  }
}





