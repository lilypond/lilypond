\header {
filename = "ode.ly";
enteredby = "Peter Chubb";
composer = " Beethoven";
date = "1800";
title = "Ode to Joy";
metre = "8 7 8 7 D";
}

\version "1.0.14";

sop=\notes \relative c'' {
	b4 b c d | d c b a | g g a b | b4. a8 a2 \bar "||";
	b4 b c d | d c b a | g g a b | a4. g8 g2 \bar "||";	
	a4 a b g | a [b8 ~ c] b4 g | a [b8 ~ c] b4 a | g a d,2 \bar "||";
	b'4 b c d | d c b a | g g a b | a4. g8 g2 \bar "||";
}

alto=\notes  \relative c'' {
	g4 g a g | g4. a8 g4 fis | d d fis g| g4. fis8 fis2 \bar "||";
	g4 g a g | g4. a8 g4 fis | d d fis g| g4 fis d2 \bar "||";
	d4 d d d | d d d d | d d fis fis | e cis d2 \bar "||";
	d4 g a a b [g8~a] g4 e | e d fis g |g fis g2 \bar "||";
}

tenor=\notes \relative c'{
	d4 d c b | e4. d8 d4 d | b b d d | d4. d8 d2 \bar "||";
	d4 d c b | e4. d8 d4 d | b b d d | d4 d b2 \bar "||";
	fis4 fis g  e | fis [g8 ~ a8] g4 e | fis fis fis b | b a g ~ fis \bar "||";
	g4 d' f, f | g4. d'8 d4 c | g g c c | d d b2 \bar "||";
}

bass= \notes \relative c' {
      g4 g g g | e4. fis8 g4 d | b b a g | d'4. d8 d2 \bar "||";
      g4 g g g | e4. fis8 g4 d | b b a g | d' d g,2 \bar "||";
      d'4 d d d | d d d d | d d dis dis | e a, d2 \bar "||";
      g4 g f f  | e4. fis8 g4 c, | c b a g | d' d g,2 \bar "||";
}


global=\notes{
	\time 4/4;
	\property Staff.timeSignatureStyle="C"
	\key G;
}

$upper_staff = \type Staff = upper \notes {
	\global
	\clef "G";
	\type Staff <
	    {\voiceone \sop}
	    {\voicetwo \alto}
	>
	\bar "|.";
}

$lower_staff = \type Staff = lower \notes {
	\global
	\clef "F";
	\type Staff <
	    {\voiceone \tenor}
	    {\voicetwo \bass }
	>
	\bar "|.";
}

\score{
	<
		\$upper_staff
		\$lower_staff
	>
	\paper {
	}
}
