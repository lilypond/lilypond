\score{
	\context Staff <
		\context Voice=one { \skip 1; }
		\context Voice=two { \skip 1; }

		\context Voice=one \partcombine Voice
			\context Thread=one \notes\relative c'' {
				a4 c4.()g8 a4
			}
			\context Thread=two \notes\relative c'' {
				g4 e4.()d8 c4
			}
	>
	\paper{
		linewidth=60.\mm;
	}
}

