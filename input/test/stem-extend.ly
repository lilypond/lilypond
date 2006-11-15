\version "2.10.0"

\header { texidoc = "@cindex Stem Extend
Extending stems to the center line may be prevented using @code{no-stem-extend}.
"
}

\score { 
  \context Voice \relative c {
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
  \layout { ragged-right = ##t}  
}

