\version "1.5.68"
\header {
 texidoc = "Don't extend stems to cetner line."
}

\score { 
  \context Voice \notes\relative c {
	\context Staff <
		\context Voice = "a" { 
			f2 f8 g a b 
			\property Voice.Stem \set #'no-stem-extend = ##t
		 	f2 f8 g a b
		}
		\context Voice = "b" { 
			c''2 c8 b a g
			\property Voice.Stem \set #'no-stem-extend = ##t
			c2 c8 b a g
		}
	>
	
  }
  \paper { }  
  \midi { }
}
