\score { 
  \context Voice \notes\relative c {
	\repeat "tremolo" 8 { c16 d16 }
	\repeat "tremolo" 4 { c16 d16 }    
  }
  \paper {
    linewidth=-1.0;
  }  
  \midi { }
}
