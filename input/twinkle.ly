%  "Ah, vous dirais-je, maman" 
%  "Altijd is Kortjakje ziek"
%  "Twinkle twinkle little star"
% 
% Copyright: none

melody = \melodic{
	\clef\violin
	c4 c | g g | a a | g g |
	f f | e e | d d8.( e16 | )c2 | % :|

	g g | f f | e e | d d |
	g g | f f | e( e8. f16 | e )d |

	c c | g g | a a | g g |
	f f | e e | d d8.( e16 | )c2 % :|
	\bar ":|"
}

accompany = \melodic{
	\clef \bass
	\octave{'c}\duration{4}
	c4 c' | e' c' | f' c' | e' c' | 
	d' b | c' a | f g | c2 | 

	e' g | d g | c' g | b g | 
	e' g | d' g | c' c'8.( d'16 | c' )b |

	c c' | e' c' | f' c' | e' c' | 
	d' b | c' a | f g | c2 
	\bar ":|"
}

global = \melodic{
		\meter {2 / 4}
		\skip {2*24}
%		\bar "||"
	}

tekst = \lyric{ 
 	Al- tijd is Kort- jak- je ziek,2
	midden in_de week maar s'_zon- dags niet.2
	s'_Zon- dags gaat ze naar de kerk,2
	met een boek vol zil- ver werk.2
	Al- tijd is Kort- jak- je ziek,2
	midden in_de week maar s'_zon- dags niet.2
}

hegedraagjetekst = \lyric{ 
 	Al- tijd zuigt Bill Gates mijn piek,2
	"\TeX" is slecht- ser dan mu- ziek.2
	s'_Zon- dags gaat het door een raam,2
	Weet dat ik me er- voor schaam.2
 	Al- tijd zuigt Bill Gates mijn piek,2
	"\TeX" is slecht- ser dan mu- ziek.2
}

texte = \lyric{ 
	 
	\textstyle "italic" 
 	Ah! vous dir- ai_- je ma man2
	Ce qui cau- se mon tour- ment2
	Pa- pa veut que je rai- sonne2
	Comme un- e grand- e per- sonne2
	Moi je dis que les bon- bons2
	Val- ent mieux que la rai- son2
	
}

texti = \lyric{
	
	\textstyle "roman"
	Twin- kle, twin- kle, lit- tle star,2
	How I won- der what you are.2
	Up a- bove the world so high,2
	Like a dia- mond in the sky.2
	Twin- kle, twin- kle, lit- tle star,2
	How I won- der what you are!2
}

textii = \lyric{
	\textstyle "roman"
	When the bla- zing sun is gone,2
	When he no- thing shines up- on,2
	Then you show your lit- tle light,2
	Twin- kle, twin- kle, all the night.2
	Twin- kle, twin- kle, lit- tle star,2
	How I won- der what you are!2
	
}

textiii = \lyric{
	
	\textstyle "roman"
	Then the tra- veler in the dark2
	Thanks you for your ti- ny spark;2
	He_could not see which way to go,2
	If you did not twin- kle so.2
	Twin- kle, twin- kle, lit- tle star,2
	How I won- der what you are!2
	
}

\score{
	\staff{ melodicregs global melody }
	\staff{ lyricregs global tekst hegedraagjetekst }
	\staff{ lyricregs global texte }
	\staff{ lyricregs global texti textii textiii }
	\staff{ melodicregs global accompany }
	\paper{
		\unitspace 2.5\cm
	}
	\midi{ 
		\tempo 4:120 
	}
}

