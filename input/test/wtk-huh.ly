\score{
	\type Staff \notes\relative c'''{
		< \type Voice = one {
		\stemup
		\voiceone
		[g8 a g f]
		}
		\type Voice=two
		{
		\stemdown
		\voicetwo
		[c8 f e d]
		}>
	}
	\paper{
		linewidth=-1.;
	}
}
