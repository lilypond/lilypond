\score { 
  \context Voice \notes\relative c {
    [f8 f]  [f64 f] \grace { [f8 e8] }
	\property Voice.noStemExtend = ##t
	[f8 f]
	
  }
  \paper {
    linewidth=-1.0;
  }  
  \midi { }
}