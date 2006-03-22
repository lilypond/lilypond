
\version "2.8.0"

\header { texidoc = "@cindex Slur Minimum Length
By setting the minimum length of a slur, notes are more separated. " 
}

\score{
	\relative c''{
		\time 2/4
		\override Slur  #'minimum-length = #40
		c(c)
		c~c\break
		}
}

