\header{
texidoc="startGraceMusic should no-stem-extend to true, but there's no effect:
the two grace beams should be the same here.
"
}


\score { 
  \context Voice \notes\relative c {
    \grace { 
      [f8 e8] 
      \property Voice.Stem \override #'no-stem-extend = ##t
      [f8 e8] 
      \property Voice.Stem \revert #'no-stem-extend
    }
    a4
	
  }
  \paper {
    linewidth=-1.0
  }  
  \midi { }
}
