\version "2.1.26"

\header { texidoc = "@cindex Stem Extend
You can stop LilyPond from extending stems to the center line. "
}

\score { 
  \context Voice \notes\relative c {
	\context Staff <<
		\new Voice { 
			f2 f8 g a b 
			\override Stem  #'no-stem-extend = ##t
		 	f2 f8 g a b
		}
		\new Voice { 
			c''2 c8 b a g
			\override Stem  #'no-stem-extend = ##t
			c2 c8 b a g
		}
	>>
	
  }
  \paper { raggedright = ##t}  
}

