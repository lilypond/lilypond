\score{
	<
	\context Staff = flauti <
		\context Voice=one \partcombine Voice
			\context Thread=one \notes\relative c''
				{
					%c1
					c2 c2
				}
			\context Thread=two \notes\relative c''
				{
				}
		>
	>
	\paper{
		
	}
}
