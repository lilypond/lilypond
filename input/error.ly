include "this-is-hopefully-a-nonexisting-file"

mwa = \music {
	\meter{3/4}
	[ a8 a8 a8 a8 a8 ]	% 
	[ a8 ]	% 
%       segfault
	< { [ bes^"1"( )a bes-. g-. ] a4-.^"4" } { [ e ] } { [ c ] } > |
	a ) b ] c 	% warning
	&   		% ok; error
	a b ) ] 	% ok; error
%        a.........	% fatal error -- assert
	a b c
	|||		% 
	abc		% ok; print error, no \output
	\bla 		%  STRING, reduced to lyric_elt.
	a [ b 		% warning, still \output
        < a( b >	% warning, still \output
	a b ,c
	a b c-*
	a b c&
	< a-. b-. >
	
}

bla = \lyrics {
	These Gates will open just like windows. % ok; warning
}


include "this-is-hopefully-a-nonexisting-file"
\score {
	\staff { 
		lyric \music { bla }
	}
	\staff { 
		\melodic \music { mwa }
	}
}
