
\header{
filename =	 "kortjakje.ly";
title =	 "Ah,  vous dirais-je, maman (variations)";
description =	 "bare bones version. (written down from memory :-)";
composer =	 "Mozart, KV 265";
enteredby =	 "HWN";
copyright =	 "public domain";
}
%{
Tested Features: example file with comments

%}
\version "0.1.10";


% the % is a comment.


% declare melody (which will be in *one* staff ) for the lead voice
melodie = \melodic {
	 			% switch Lilypond in note-mode
	\clef"violin";
	\meter 2/4 ;
	\octave  c'; 		% set the default octave
	% the default note duratino is 4
	%%% theme
	c4 c |			% the '|' checks if there is a new measure here.
	g g|
	a a|

	g2|	% g2 means a "g-1" pitched half-note 
	f4 f|	e e|	d d|  c2
	g4 g|	f f|	e e| 	d d|	g g|	f f|	e e| 	d d|
 	%%% var 1
	  c4 r8 c		% r8 means an 8th rest.
		  (|		% start a slur
				% NOTE: the slurstart should be directly after the note
			) 	% end a slur.
			  g4 r8 g8 () 	 a4 r8 a8 () 	 g4 r4 
	  f4 r8 f8 () 	 e4 r8 e8 (|)  d4 r8 d8 () 	 c4 r4 
}

				% more of this.
begeleiding = \melodic{ 
	\clef "bass";
	\meter 2/4 ;
	\octave  c'; 		% default octave: 1 below the first octave.

	%%% theme
	'c4			% ' before note means one octave lower.
				% Similarly: ' after means one higher.
	   c	e c	f c	e c	d 'b	c 'a	'f 'g	'c2 |
	\octave  c ;
	e4 'g	d 'g	c 'g	'b 'g	e 'g	d 'g	c 'g	'b 'g
	%%%% var 1
	r8 e8()  c4  	r8 e8()  c4  	r8 f8() c4 	r8 e8() c4
	r8 d8() 'b4 	r8 c8() 'a4 	r8 'a8() 'f4 	r8 'e8() 'c4
}

\score{
	\melodic \type Grand_staff <
		\melodie
		\begeleiding 
	>
	\paper{}
	\midi{}
}

