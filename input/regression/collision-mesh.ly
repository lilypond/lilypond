\header {
texidoc = "Oppositely stemmed chords,  meshing into each other,
are resolved."
}

\score {
  \context Staff \notes \relative c' {
     \time 3/4
     % Here's what I was trying to do:
     < \context Voice  = VI {\stemDown <g4 b g'> 
     r4 r4 }
       \context Voice=VII {\stemUp d'2.} >

     % Or:

     <\context Voice = VI {\stemUp <g4 b g'>  r r}
       \context Voice= VII {\stemDown d'2.} >
  }
}





