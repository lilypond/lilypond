% midi.ly
% test source for lilypond-s midi output

melodie = music {
	$\clef\violin
	c c | g g | a a | g2 
	f f | e e | d d8.( e16 | )c2 | % :|
	$
}

begeleiding = music {
	$
	\clef \bass
	\octave{`}
	c 'c | 'e 'c | 'f 'c | 'e 'c 
	'd b | 'c a | f g | c2 | 
	$
}

mstaf = staff {
	melodic
	music { melodie }
}

bass_staf = staff {
	melodic	
	music { begeleiding }
}

score {
	staff { mstaf }
	staff { bass_staf }
	midi {
		tempo 4:120
	}
	commands {
		meter { 2 * 4 }
	}
}

