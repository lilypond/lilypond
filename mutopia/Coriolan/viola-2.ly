\header{
filename =	 "viola-2.ly";
%title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1827)";
enteredby =	 "JCN";
copyright =	 "public domain";
}

\version "1.0.7";

viola2 = \notes \relative c {
	% starts on (actualy, most part is on) same staff as viola1
	%urg, parse error
%urg	\translator Staff=viola1
	\translator Staff=violai
	\type Voice=one
	c1 ~ | c | s1*2 |
	% copied 3 bars from three back...
	c ~ | c | s1*2 % |
%	\translator Staff=viola
	c ~ | c | s1*2 | 
	g'4-. r r2 | g4-. r r2 |
	%15
	s1*14
	% same as viola-1, octave lower
	r8 c,-.\p c4. c8-. c4~ | [c8-"cresc." c-.]c2.~ |
	[c8 c-.]c2.~ | [c8 c-.] c4. c8-. c4~ | [c8 c8-.] c4. c8-. c4 |
	%35
	[as'16\ff as a as][as as as as]as2:16 |
}

% $viola2_staff = \type Staff = viola_2 <
$viola2_staff = \type Staff = violaii <
	\property Staff.midi_instrument = "viola"
	\property Staff.instrument = "Viola II"
	\property Staff.instr = "Vla. II"
	\clef "alto";
	\notes \type Voice=one< 
		\global
		\$viola2
	>
>

