\header{
filename =	 "timpani.ly";
%title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1827)";
enteredby =	 "JCN";
copyright =	 "public domain";


}

\version "1.0.7";

timpani = \notes \relative c {
	R1 *2 | c4-.\ff r r2 | R1 *3 | c4-. r r2 | R1 *3 |
	c4-. r r2 | r1 | g4-. r r2 | g4-. r r2 | R1 *5 |
	%20
	r2 r4 g4-. | R1*6 | r4 c-.\f r2 | R1*2 | r2 c4-.\p_"\ \ \ cresc." r |
	r2 c4-. r | r2 c4-. r | c r c r | c1\ff-\trill |
	%35
}

$timpani_staff = \type Staff = timpani <
	\property Staff.midi_instrument = "timpani"
	\property Staff.instrument = "2 Timpani (C-G)"
	\property Staff.instr = "Timp."
	\clef "bass";
	\notes< 
%		\global
		\time 4/4;
		\$timpani
	>
>

