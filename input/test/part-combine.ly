\score{
	\context Staff = first <
		\context Voice=first { \skip 1; }
		\context Voice=second { \skip 1; }

		\context Voice=first \partcombine Voice
			\context Thread \notes\relative c''
				{
					c4 d e f
					c4 d e f
				}
			\context Thread \notes\relative c''
				{
					a b c d
					a4. b8 c4 d
				}
		>
}
