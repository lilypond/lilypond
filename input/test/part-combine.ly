\score{
	\context Staff = one <
		\context Voice=one { \skip 1; }
		\context Voice=two { \skip 1; }

		\context Voice=one \partcombine Voice
			\context Thread=one \notes\relative c''
				{
					c4 d e f
					b,4 d c d
					r2 e4 f
					c4 d e f
				}
			\context Thread=two \notes\relative c''
				{
					g b d f
					r2 c4 d
					a c c d
					a4. b8 c4 d
				}
		>
}
