\version "1.3.146"
\header {
texidoc = "Measure and beat repeats are supported."
}
	
\score { \notes \relative c' \context Voice { \time 4/4
   \repeat "percent" 2 { c2 }

   % the chairman dances
   \repeat "percent" 2 { g'8 g c, c  }   
   \repeat "percent" 4 { b8 b8  }
   \repeat "percent" 2 { c8 d es f g4 r4  }   
   
   % riff
   \repeat "percent" 2 { r8. a16 g8. a16 bes8. a16 f8 d |  a c8 ~ c8 d8 ~ d8 r8 r4 }
   


     }}
