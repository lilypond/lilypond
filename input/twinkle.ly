\header{
filename =	"twinkle.ly";
title =		"Twinkle Twinkle Little Star";
composer =	"Traditional";
enteredby =	"hwn and jcn";
copyright =	"public domain";
}

%{
DESCRIPTION

traditional song in various languages.
  "Ah, vous dirais-je, maman" 
  "Altijd is Kortjakje ziek"
  "Twinkle twinkle little star"
%}

%{
Tested Features: lyrics, interleaving lyrics and staffs, repeats
%}

\version "1.1.52";

melody = \notes \relative c'' {
	\clef violin;
	\property Staff.instrument = "alto sax"
	
	c4 c | g' g | a a | g g |
	f f | e e | d d8.( e16 | )c2 |

	g'4 g | f f | e e | d d |
	g g | f f | e( e8. f16 | e4 )d |

	c c | g' g | a a | g g |
	f f | e e | d d8.( e16 | )c2 |
}

accompany = \notes \relative c {
	\clef "bass";
	c4 c' | e c | f c | e c | 
	d b | c a | f g | c,2 | 

	e'4 g, | d' g, | c g | b g | 
	e' g, | d' g, | c c8.( d16 | c4 )b |

	c, c' | e c | f c | e c | 
	d b | c a | f g | c,2 
}

global = \notes {
	\time 2/4;
}

tekst = \lyrics{ 
 	Al-4 tijd is Kort- jak- je ziek,2
	midden4 "in de" week maar "'s zon-" dags niet.2
	"'s Zon-"4 dags gaat ze naar de kerk,2
	met4 een boek vol zil- ver werk.2
	Al-4 tijd is Kort- jak- je ziek,2
	mid-8 den  in de week4 maar "'s zon-" dags niet.2
}


%{
Ja inderdaad. Dit is geen educatieve danwel muzikaal verantwoorde
tekst. Mogen wij ook af en toe ergens op afgeven?
%}

hegedraagjetekst = \lyrics{ 
 	Al-4 tijd zuigt Bill Gates mijn piek,2
	"\TeX"4 is slecht- ser dan mu- ziek.2
	"'s Zon-"4 dags gaat het door een raam,2
	Weet4 dat ik me er- voor schaam.2
 	Al-4 tijd zuigt Bill Gates mijn piek,2
	"\TeX"4 is slecht- ser dan mu- ziek.2
}

texte = \lyrics{ 
	\property Lyrics . textStyle" =  "italic" 
%	\property Lyrics . textStyle" =  "roman" 
 	Ah!4 vous dir- ai- je ma- man2
	Ce4 qui cau- se mon tour- ment2
	Pa-4 pa veut que je rai- son- ne
	Comm' u- ne gran- de per- son- ne
	Moi je dis que les bon- bons2
	Va-4 lent mieux que la rai- son2
}

texti = \lyrics{
	\property "Lyrics"."textStyle" =  "roman"
	Twin-4 kle, twin- kle, lit- tle star,2
	How4 I won- der what you are.2
	Up4 a- bove the world so high,2
	Like4 a dia- mond in the sky.2
	Twin-4 kle, twin- kle, lit- tle star,2
	How4 I won- der what you are!2
}

textii = \lyrics{
	When4 the bla- zing sun is gone,2
	When4 he no- thing shines up- on,2
	Then4 you show your lit- tle light,2
	Twin-4 kle, twin- kle, all the night.2
	Twin-4 kle, twin- kle, lit- tle star,2
	How4 I won- der what you are!2
	
}

textiii = \lyrics{
	Then4 the tra- veler in the dark2
	Thanks4 you for your ti- ny spark;2
	He4 could not see which way8 to8 go,2
	If4 you did not twin- kle so.2
	Twin-4 kle, twin- kle, lit- tle star,2
	How4 I won- der what you are!2
}

\score{
	<
		\context Staff=i \repeat semi 2 < \global\melody >
		\context Lyrics=top \context LyricVoice \repeat fold 2 {} \alternative { \tekst \texte }
		\context GrandStaff <
			\context Staff=ii \repeat semi 2 < \global\melody >
			\context Staff=iii \repeat semi 2 < \global\accompany >
		>
		\context Lyrics =bottom \context LyricVoice \repeat fold 3 {} 
			\alternative { \texti \textii \textiii }
	>
	\paper{
		gourlay_maxmeasures = 14.0;
	}
	\midi{ 
		\tempo 4 = 120 ;
	}
}

