\version "1.3.110";
\score { 
  \context Voice \notes\relative c {
    c'1:16 
	\stemUp
	c4:8 c4:16 [c8:16 c:] [c,8:16 c'':]
	\stemBoth
	[c,,8:16 c'':]
	
  }
  \paper { }  
  \midi { }
}
