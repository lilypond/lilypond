\version "1.3.110";
\score { 
  \context Voice \notes\relative c {
    
	\relative c'' \context Staff <
	\context Voice = one {
		\property Staff.collisionMergeDotted = ##t
		\stemUp
		[c8 f g a]
	}
	\context Voice = two {
		\stemDown c,4. f8
	
	}>
	
  }
  \paper { }  
  \midi { }
}
