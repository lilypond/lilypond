\header {
title = "Laudate Dominum";
composer = "Parry";
metre = "10 10 . 11 11";
}

\version "1.3.42";

sop = \notes \transpose c''{
	f4 | d' bes c' | f2 \bar "||";
	bes4 | g f ees | d2 \bar "||";
	f4 | d' c' f | bes2 \bar "||";
	a4 | c' bes g | f2 \bar "||";
	f4 | bes bes bes | bes [a8 ~ g8] \bar "||";
	[a8~bes8] | c'4 bes c' | f2 \bar "||";
	a4 | d' d' d' | [bes8 ~ a8] g4 \bar "||"; 
	ees'4 | d' bes c' | bes2 \bar "||";
}

alto = \notes \transpose c'' {
	f4 | f f g | f2 \bar "||";
	f4 | bes, d c | bes,2 \bar "||";
	f4 | bes a f | f ~ g \bar "||"; 
	f4 | fis g e | f2 \bar "||";
	c4 | bes, ees f | g ees \bar "||";
	f4 | g f g | f2 \bar "||";
	f4 | fis g a | g g \bar "||";
	g | f4. g8 a4 | bes2 \bar "||";
}

tenor = \notes \transpose c' {
	f4 | bes d' [c'8 ~ bes] | a2 \bar "||";
	f4 | g g a | bes2 \bar "||";
	d'4 | e' f' c' | d' ~ c' \bar "||";
	c'4 |  ees' d' bes | a2 \bar "||";
	a4 | bes bes aes | g bes \bar "||";
	bes | ees' d' [c'8 ~ bes8] | a2 \bar "||";
	c'4 | c' bes a | [d'8 ~ c'8] bes4 \bar "||";
	bes4 | bes d' ees' | d'2 \bar "||";
}

bass = \notes \transpose c'{
	f4 | bes d ees | f2 \bar "||";
	d4 | ees c f | bes,2 \bar "||";
	bes4 | g a a, | d ~ e \bar "||";
	f | a, bes, c | f2 \bar "||";
	ees4 | d c d | ees ees \bar "||";
	d | c d ees | f2 \bar "||";
	ees4| d e fis | g g \bar "||";
	c4 | f f f | bes,2 \bar "||";
}

global = \notes {
	\time 3/4;
	\partial 4;
	\key bes;
}

\score {
	<
	  \context Staff=top { 
		\global \clef "G";
		\context Staff <
			\notes{\voiceone \sop }
			\notes{\voicetwo \alto }
			>
	  }
	  \context Staff=bottom { 
		\global \clef "F";
		\context Staff <
		      \notes{\voiceone \tenor}
		      \notes{\voicetwo \bass }
		>
	  }
	>
}
