\score{
	\notes \relative c''{
		\property Voice.slurVerticalDirection = #1
		\property Voice.slurBeginAttachment = #'head
		\property Voice.slurEndAttachment = #'head
		g16()g()g()g()d'()d()d()d
	}
	\paper{
		indent = 0.0;
		linewidth = 60.0\mm;
	}
}
