\score { 
  \context Voice \notes\relative c {
    % to see the magic: uncomment size stuff in init/paper20.ly
	
	c'4 c4
	
	\property Voice.fontsize= -2
	b16 * 1 / 2 (
	\property Voice.fontsize= 0 )
	g4 *31/32
	
	a a g2
	
  }
  \paper { }  
  \midi { }
}
