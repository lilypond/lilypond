\score { 
  \context Voice \notes\relative c {
    
	\context Voice {
	\property Voice.sparseTies = ##t
	c''  <c e g> ~ <c e g>  }
	
  }
  \paper {
    linewidth=-1.0;
  }  
  \midi { }
}
