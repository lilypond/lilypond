\score{
	\notes \relative c''{
		\property Voice.slurVerticalDirection = #1 c8(()c())c4
		\property Voice.slurVerticalDirection = #-1 a8(()a)a4
		\break

		\property Voice.slurVerticalDirection = #-1 a8(()a())a4
		\property Voice.slurVerticalDirection = #1 c8(()c)c4
		\break

		\property Voice.slurVerticalDirection = #-1 e8(()e())e4
		\property Voice.slurVerticalDirection = #1 f,8(()f)f4
		\break

		\property Voice.slurVerticalDirection = #1 e8(()e())e4
		\property Voice.slurVerticalDirection = #-1 f'8(()f)f4
		\break

	}
	\paper{
		indent = 0.0;
		linewidth = 60.0\mm;
	}
}
