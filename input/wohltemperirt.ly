% JS Bach, Das Wohltemperirtes Clavier I, Fuga II (c-minor)
%
% a 7 bar fragment
%
%	Public Domain -- by HWN
%

dux = music { $
	\duration { \last } 
	\stem{-1}  r8 ['c16 b] ['c8 g] [as 'c16 b] ['c8 'd] |
	[g 'c16 b] ['c8 'd] [f16 g] as4 [g16 f]	|
	[es 'c b a] [ g f es d] [c8 'es 'd 'c] |
	[bes a bes 'c] [fis g a fis] |
	g4 r16 [c d es] [f g as8(] [)as16 d es f ]|
	[g a bes8(] [)bes16 es f g ] [as g f es] [d8 'c16 b]|
	'c4 r4 r8 ['f 'es 'd] |
	r8 [as g f] [g f16 es] [f8 d] | 
$}

comes = music { $
	r1 r1
	\stem{1}
	\octave {'}
	r8 [g16 fis] [g8 c] [es g16 f] [g8 a]|
	[d8 g16 fis] [g8 a] [c16 d] es4 [d16 c] |
	[`bes8 es16 d] [es8 `g8] [`as f16 es] [f8 `a] |
	[`bes8 g16 f] [g8 `b] [c8 d16 es] f4( |
	[) f8 es16 d] [c16 `bes `as `g] [`f8 as g f] |
	[es d es f] [`b c d `b] |
	
$}

bassdux = music { $
	r1 r1 r1 r1 r1 r1
	\stem{1}	\octave { }
	r8 [c16 B] [c8 G] [As c16 Bes] [c8 d] |
	[G c16 B] [c8 d] [F16 G] As4 [G16 F] | $
}

% will make it look even uglier
basstaf = staff { melodic
	music { bassdux }
	commands {
		clef bass
		key $bes es as$
	}
}

score {
	staff { melodic

		% every "music {} " in a staff has its own "voicegroup"
		music { dux }
		music { comes }
		
		commands {
			key $bes es as$
		}
	}
	staff { basstaf }
	commands { 
		meter 4*4
		skip 8:0
	}
	paper { unitspace 2.5cm
		geometric 1.1
	}
}