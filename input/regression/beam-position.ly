\version "1.5.68"
\header{
texidoc="
Beams should always reach the middle staff line.  The second beam
counting from the note head side, should never be lower than the
second staff line.  This does not hold for grace note beams.
Override with @code{noStemExtend}.
"
}


\score { 
  \context Voice \notes\relative c {
    [f8 f]  [f64 f] 
    \grace { 
      [f8 e8] 
      \property Voice.Stem \override #'no-stem-extend = ##f
      [f8 e8] 
      \property Voice.Stem \revert #'no-stem-extend
    }
	[f8 f]
	
  }
  \paper {
    linewidth=-1.0
  }  
  \midi { }
}
