%  "Ah, vous dirais-je, maman" 
%  "Altijd is Kortjakje ziek"
%  "Twinkle twinkle little star"
% 


melodie = music {
	$
	c c | g g | a a | g g |
	f f | e e | d d8.( e16 | )c2 | % :|

	g g | f f | e e | d d |
%	g g | f f | e( [f0*32 e0*32 d0*32] e8. f16 | e )d | c c | 
	g g | f f | e( e8. f16 | e )d |

	c c | g g | a a | g g |
	f f | e e | d d8.( e16 | )c2 % :|
	$
}

begeleiding = music {
	$
	\octave{`}
	c 'c | 'e 'c | 'f 'c | 'e 'c | 
	'd b | 'c a | f g | c2 | 

	'e g | d g | 'c g | b g | 
%	'e g | 'd g | 'c 'c8.( 'd16 | \voice{ 'c )b } \voice{ g2 } |
	'e g | 'd g | 'c 'c8.( 'd16 | 'c )b |

	c 'c | 'e 'c | 'f 'c | 'e 'c | 
	'd b | 'c a | f g | c2 
	$
}

tekst = music { 
	@ 
 	Al- tijd is Kort- jak- je ziek,2
	midden in_de week maar 's_zon- dags niet.2
	's_Zon- dags gaat ze naar de kerk,2
	met een boek vol zil- ver werk.2
	Al- tijd is Kort- jak- je ziek,2
	midden in_de week maar 's_zon- dags niet.2
	@
}

he_gedraagje_tekst = music { 
	@ 
 	Al- tijd zuigt Bill Gates mijn piek,2
	"\TeX" is slecht- ser dan mu- ziek.2
	's_Zon- dags gaat het door een raam,2
	Weet dat ik me er- voor schaam.2
 	Al- tijd zuigt Bill Gates mijn piek,2
	"\TeX" is slecht- ser dan mu- ziek.2
	@
}

texte = music { 
	@ 
	\textstyle "italic" 
 	Ah! vous dir- ai_- je ma man2
	Ce qui cau- se mon tour- ment2
	Pa- pa veut que je rai- sonne2
	Comme un- e grand- e per- sonne2
	Moi je dis que les bon- bons2
	Val- ent mieux que la rai- son2
	@
}

text1 = music {
	@
	\textstyle "roman"
	Twin- kle, twin- kle, lit- tle star,2
	How I won- der what you are.2
	Up a- bove the world so high,2
	Like a dia- mond in the sky.2
	Twin- kle, twin- kle, lit- tle star,2
	How I won- der what you are!2
	@
}

text2 = music {
	@
	\textstyle "roman"
	When the bla- zing sun is gone,2
	When he no- thing shine- s upon,2
	Then you show your lit- tle light,2
	Twin- kle, twin- kle, all the night.2
	Twin- kle, twin- kle, lit- tle star,2
	How I won- der what you are!2
	@
}

text3 = music {
	@
	\textstyle "roman"
	Then the tra- veler in the dark2
	Thanks you for your ti- ny spark;2
	He could not see which way to_go,2
	If you did not twin- kle so.2
	Twin- kle, twin- kle, lit- tle star,2
	How I won- der what you are!2
	@
}

mstaf = staff {
	melodic
	music { melodie }
	commands { clef violin }
}

bass_staf = staff {
	melodic
	music { begeleiding }
	commands { clef bass }
}


dutch_staf = staff {
 	lyric 
	music { tekst }
	music {he_gedraagje_tekst}
}

french_staf = staff {
 	lyric 
	music { texte }
}

english_staf = staff {
 	lyric 
	music { text1 }
	music { text2 }
	music { text3 }
}

tstaf = staff { 
	lyric 
	music { tekst }
	music { texte }
}


score {
	staff { mstaf }
	staff { dutch_staf }
	staff { french_staf }
	staff { english_staf }
	staff { bass_staf }
	paper {
		unitspace 2.5cm	% a whole note takes 2.5 cm ideally.
	}
	commands {
		meter 2 * 4
		skip 24:0
		bar "||"
	}
}

