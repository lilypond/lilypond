\version "1.7.18"

\header { texidoc = "@cindex Stem Extend
You can stop Lilypond from extending stems to the center line. "
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
  \paper { raggedright = ##t}  
}
%% new-chords-done %%
