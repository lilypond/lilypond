\version "1.5.68"
\header {
texidoc = "" 
}

\score{
	\notes\relative c''{
		\time 2/4
		\property Voice.Slur \set #'minimum-length = #40
		c()c
		c~c\break
		}
}
