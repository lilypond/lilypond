% midi.ly
% test source for lilypond-s midi output


% BUG: 2e noot van begeleiding bij g2 niet gespeeld.
% BUG: rust (5e maat) niet gespeeld.
melodie = music {
	$\clef\violin
	c c | g g | a a | g2 | r2
	f f | e e | d d8.( e16 | )c2 | % :|
	$
}

begeleiding = music {
	$
	\clef \bass
	\octave{`}
	c 'c | 'e 'c | 'f 'c | 'e 'c | r2
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
%	staff { mstaf }
%	staff { bass_staf }
%	paper { unitspace 2.5cm }
	% in own audio {  } block iso score { } ?
	% allow no score { } block ?

	staff { midi music { melodie } }
	staff { midi music { begeleiding } }
%	staff { midi music { bla } }
	midi {
		tempo 4:120
	}
	commands {
		meter { 2 * 4 }
	}
}

