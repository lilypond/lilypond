% \midi.ly
% test source for lilypond-s \midi \output

melodie = \melodic{
	\clef\violin
	\meter{ 2/4 }
	c c | g g | a a | g2 
	f f | e e | d d8.( e16 | )c2 | % :|
	
}

begeleiding = \melodic{
	\clef\bass
	\meter{ 2/4 }
	\octave{'c}
	c c' | e' c' | f' c' | e' c' 
	d' b | c' a | f g | c2 | 
	
}

\score{
	\staff{ melodie }
	\staff{ begeleiding }
	\midi{
		\tempo 4:120
	}
}

