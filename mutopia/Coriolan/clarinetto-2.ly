\header{
filename =	 "clarinetto-2.ly";
%title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1792)";
enteredby =	 "JCN";
copyright =	 "public domain";


}

\version "0.1.10";

clarinetto2 = \melodic{
	\octave c';	
% koor: clarinetto-part.ly
% lilypond: ../../current/lily/dynamic-grav.cc:58: void Dynamic_engraver::do_process_requests(): Assertion `!dynamic_p_' failed.
% Aborted (core dumped)

% huh?

%	r1 | r1 | as4-.\ff r r2 | r1 |
	R1 *2 | as4-. r r2 | R1 *3 |
	b4-. r r2 | R1 *3 |
	c'4-. r r2 | r1 |
	c'4-. r r2 | b4-. r r2 |
	R1 *5 |
	r2 r4 'b4-. |
}

