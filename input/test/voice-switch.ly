\version "1.5.68"

\score{
	\context Staff <
		\context Voice=one\skip 1
		\context Voice=two\skip 1
		\context Voice=one \partcombine Voice
			\context Thread=one \notes\relative c'' {
				c2 c2
				c2 c2
			}
			\context Thread=two \notes\relative c'' {
				b2 a4 () a
				a2 a4 () a
				
			}
	>
	\paper{
		linewidth=140.\mm
	}
}


