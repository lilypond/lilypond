\version "1.5.68"
\header{
texidoc="The partcombiner should not combine two small slurs into a big one."
}

\score{
	\context Staff <
		\context Voice=one\skip 1
		\context Voice=two\skip 1
		\context Voice=one \partcombine Voice
			\context Thread=one \notes\relative c'' {
				c4( c b )a 
				a( b c )d
			}
			\context Thread=two \notes\relative c'' {
				a4( c b )a
				a( b c )a
				
			}
	>
	\paper{
		linewidth=140.\mm
	}
}


