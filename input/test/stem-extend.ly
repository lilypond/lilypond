#(ly:set-option 'old-relative)
\version "1.9.8"

\header { texidoc = "@cindex Stem Extend
You can stop LilyPond from extending stems to the center line. "
}

\score { 
  \context Voice \notes\relative c {
	\context Staff <<
		\new Voice { 
			f2 f8 g a b 
			\property Voice.Stem \set #'no-stem-extend = ##t
		 	f2 f8 g a b
		}
		\new Voice { 
			c''2 c8 b a g
			\property Voice.Stem \set #'no-stem-extend = ##t
			c2 c8 b a g
		}
	>>
	
  }
  \paper { raggedright = ##t}  
}

