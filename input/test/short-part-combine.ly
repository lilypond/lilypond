\score{
	\context Staff = one <
		\context Voice=one { \skip 1; }
		\context Voice=two { \skip 1; }

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
