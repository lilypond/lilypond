\score{
	\notes \relative c''{
		\property Voice.slurVerticalDirection = #1 c()a d()g,\break
		\property Voice.slurVerticalDirection = #-1 c()a d()g,\break
		\property Voice.slurVerticalDirection = #1 a()c d()g,\break
		\property Voice.slurVerticalDirection = #-1 a()c d()g,\break
		\property Voice.slurVerticalDirection = #-1 a()c d()g,\break
	}
	\paper{
		indent = 0.0;
		linewidth = 60.0\mm;
	}
}
