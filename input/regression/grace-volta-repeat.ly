
\header {

    texidoc = "Repeated music can start with grace notes.  Bar checks
    preceding the grace notes do not cause synchronization effects.  "

}

\score{
   \notes\relative c'''{\key a \minor \time 2/4
     \repeat "volta" 2 {
     \grace { [a16( c] }  c,4 c4  |
     \grace { [e'16( gis] } c,4 c4 |
     
     }
   }
   \paper { linewidth = -1. }
 }


