\score{
	\context Staff = first <
		\context Voice=first { \skip 1; }
		\context Voice=second { \skip 1; }

		\context Voice=first \partcombine Voice
			\context Thread=first \notes\relative c''
				{
					c4 d e f
					b,4 d c d
					r2 e4 f
					c4 d e f
				}
			\context Thread=second \notes\relative c''
				{
					a b c d
					r2 c4 d
					a c c d
					a4. b8 c4 d
				}
		>
}
