\score { 
  \context Voice \notes\relative c {
    % test noStemExtend
	\context Staff <
		\context Voice = "a" { 
			f2 f8 g a b 
			\property Voice.noStemExtend = ##t
		 	f2 f8 g a b
		}
		\context Voice = "b" { 
			c''2 c8 b a g
			\property Voice.noStemExtend = ##t
			c2 c8 b a g
		}
	>
	
  }
  \paper { }  
  \midi { }
}
