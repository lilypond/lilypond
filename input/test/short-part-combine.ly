\version "1.5.68"

\score{
	\context Staff <
		\context Voice=one \partcombine Voice
			\context Thread=one \notes\relative c''
				{
					d e f
				}
			\context Thread=two \notes\relative c''
				{
					d d d
				}
		>
}
