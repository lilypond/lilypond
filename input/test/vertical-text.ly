\score{
	<
		\type Staff = o \notes\relative c,,, { 
			\stemdown [c8 c c c] c4 c4 \break [c8 c c c] c4 c4
		}
		\type Lyrics \lyrics{
			BLA gag _2 < { BLA gag } { BLA gag la } > _2 
		}
		\type Staff = t \notes\relative c''''' { 
			\stemup [c8 c c c] c4 c [c8 c c c] c4 c
		} 
	>
	\paper{
		castingalgorithm = \Wordwrap;
	}
}
