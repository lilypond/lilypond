
\version "1.9.8"
\header{
texidoc = "Here startGraceMusic should set no-stem-extend to true;
the two grace beams should be the same here.
"
}


\score { 
  \context Voice \notes\relative c {
    \grace { 
       f8[ e8] 
      \property Voice.Stem \override #'no-stem-extend = ##t
       f8[ e8] 
      \property Voice.Stem \revert #'no-stem-extend
    }
    a4
	
  }
  \paper {
    raggedright = ##t
  }  
  \midi { }
}

