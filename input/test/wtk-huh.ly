
%{
What's supposed to be demonstrated here?
%}
\score{
	\context Staff \notes\relative c'''{
		< \context Voice = one {
		\stemup
		\voiceone
		[g8 a g f]
		}
		\context Voice=two
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

\version "1.0.21"; 
