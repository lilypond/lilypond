
\version "1.3.110";

\score { 
  \context Voice \notes\relative c {
    [f8 f]  [f64 f] \grace { [f8 e8] }
	\property Grace.Stem \override #'no-stem-extend = ##t
	[f8 f]
	
  }
  \paper {
    linewidth=-1.0;
  }  
  \midi { }
}
