
\version "2.1.7"

\header { texidoc = "@cindex Slur Minimum Length
You can set the minimum length of a slur. " 
}

\score{
	\notes\relative c''{
		\time 2/4
		\property Voice.Slur \set #'minimum-length = #40
		c(c)
		c~c\break
		}
}

