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
Tested Features: lyrics, interleaving lyrics and staffs, repeats,
	auto beaming, adding lyrics to notes, hyphens
%}

\version "1.3.42";

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


tekst = \lyrics{ 
 	Al -- tijd is Kort -- jak -- je ziek, " "
	midden "in de" week maar "'s zon" -- dags  " " niet.
	"'s Zon" -- dags gaat ze naar de kerk, " "
	met een boek vol zil -- ver  " " werk. " "
	Al -- tijd is Kort -- jak -- je ziek, " "
	midden  "in de" week maar "'s zon" -- dags  " " niet.
}


%{
Ja inderdaad. Dit is geen educatieve danwel muzikaal verantwoorde
tekst. Mogen wij ook af en toe ergens op afgeven?
%}

hegedraagjetekst = \lyrics{ 
 	Al -- tijd zuigt Bill Gates mijn piek, " "
	"\TeX" is slecht -- ser dan mu --  " " ziek.
	"'s Zon" -- dags gaat het door een raam, " "
	Weet dat ik me er -- voor  " " schaam.
 	Al -- tijd zuigt Bill Gates mijn piek, " "
	"\TeX" is slecht -- ser dan mu --  " " ziek.
}

texte = \lyrics{ 
	\property Lyrics . textStyle" =  "italic" 
%	\property Lyrics . textStyle" =  "roman" 
 	Ah! vous dir -- ai -- je ma -- man " "
	Ce qui cau -- se mon tour --  " " ment
	Pa -- pa veut que je rai -- son -- ne
	Comm' u -- ne gran -- de per -- " " son -- ne
	Moi je dis que les bon -- bons " "
	Va -- lent mieux que la rai --  " " son
}

texti = \lyrics{
	\property "Lyrics"."textStyle" =  "roman"
	Twin -- kle, twin -- kle, lit -- tle star, " "
	How I won -- der what you  " " are.
	Up a -- bove the world so high, " "
	Like a dia -- mond in the  " " sky. " "
	Twin -- kle, twin -- kle, lit -- tle star, " "
	How I won -- der what you  " " are!
}

textii = \lyrics{
	When the bla -- zing sun is gone, " "
	When he no -- thing shines up --  " " on,
	Then you show your lit -- tle light, " "
	Twin -- kle, twin -- kle, all the  " " night. " "
	Twin -- kle, twin -- kle, lit -- tle star, " "
	How I won -- der what you  " " are!
}

textiii = \lyrics{
	Then the tra -- veler in the dark " "
	Thanks you for your ti -- ny  " " spark;
	He could not see which way to go,
	If you did not twin -- kle  " " so. " "
	Twin -- kle, twin -- kle, lit -- tle star, " "
	How I won -- der what you  " " are!
}

\score{
	\notes <
		\context Staff=i s1
		\context Lyrics=top s1
		\context GrandStaff <
			\context Staff=ii \repeat volta 2 <
			  \time 2/4;
			  \melody >
			\context Staff=iii \repeat volta 2 <
			  \accompany >
		>
		\context Lyrics=bottom s1
		% ugh, \repeat in \addlyrics dumps core
		\addlyrics
			\context Staff = i < \melody>
			< 
				%\repeat fold 2 {} 
				%\alternative { 
					\context Lyrics = top \tekst
					\context Lyrics = top \texte
				%}
				%\repeat fold 3 {} 
				%\alternative { 
					\context Lyrics = bottom \texti
					\context Lyrics = bottom \textii
					\context Lyrics = bottom \textiii
				%}
			>
	>
	\paper{
		gourlay_maxmeasures = 14.0;
	}
	\midi{ 
		\tempo 4 = 120 ;
	}
}

