mwa = music {
	$
	ab % ok, warning, still output?
%	\bla % ok, fatal
%	&   %  ok, fatal
%	c d ) ] % ok, fatal
	c [ d 
%	c ) d ] e % no location :-(
	
	$
}

bla = music {
	@ 
	These Gates will open just like windows.
	@
}

score {
	staff { 
		lyric music { bla }
	}
	staff { 
		melodic music { mwa }
	}
	commands {
		meter 2*4
	}
}
