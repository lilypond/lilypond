
melanie = music {
	$
	\duration{8}
	[ c d ] { [ d 'd } { c 'c ] } |
	[ c d ] [ e { d g } c ]2/3 |
	[ c d ] [ c16 d16 'e16 'e16 d16 c16 ]4/6 |
	[ c d e e d c ]4/6 |
	[c d ] { \music{ [ c d e ]2/3 } \music{ [ f g a ]2/3 } } |
	[ c d ] [ d c ] |
        $
}

michelle = music {
	$
	\duration{8}
	[ c c c c ] |
	[ c c c c ] |
	[ c c c c ] |
	[ c c c c ] |
	[ c c c c ] |
	[ c c c c ] |
	$
}

mireille = music {
	@
	\duration{8}
	o o o o |
	o o [ o o o ]2/3 |
	[ o o o ]2/3 [ o16 o16 o16 o16 o16 o16 ]4/6 |
	[ o o o o o o]4/6 |
	o o [ o o o ]2/3 |
	o o o o |
	@
}

score {
	staff { lyric music { mireille } }
	staff { melodic music { melanie } }
	staff { melodic music { michelle } }
	commands{ 
		meter {2*4}
	}
}
