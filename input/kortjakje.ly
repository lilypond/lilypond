% "Ah,  vous dirais-je, maman" (Mozart, KV 265)
% 
% bare bones version. (written down from memory :-)
% for lyrics, see twinkle.ly

% the % is a comment.
%
% copyright: None
%
% declare melody (which will be in *one* staff ) for the lead voice
\version "0.0.54";

melodie = \melodic {
	 			% switch Lilypond in note-mode
	\clef\violin;
	\meter 2/4 ;
	\octave  c'; 		% set the default octave
	% the default note duratino is 4
	%%% theme
	c c |			% the '|' checks if there is a new measure here.
	g g|
	a a|

	g2|	% g2 means a "g-1" pitched half-note 
	f f|	e e|	d d|  c2
	g g|	f f|	e e| 	d d|	g g|	f f|	e e| 	d d|
 	%%% var 1
	  c r8 c8		% r8 means an 8th rest.
		  (|		% start a slur
				% NOTE: the slurstart should be directly after the note
			) 	% end a slur.
			  g r8 g8 (|	) a r8 a8 (|	) g r4 
	  f r8 f8 (|	) e4 r8 e8 (	|) d4 r8 d8 (|	) c4 r4 
}

				% more of this.
begeleiding = \melodic{ 
	\clef "bass";
	\meter 2/4 ;
	\octave  c'; 		% default octave: 1 below the first octave.

	%%% theme
	'c			% ' before note means one octave lower.
				% Similarly: ' after means one higher.
	   c	e c	f c	e c	d 'b	c 'a	'f 'g	'c2
	\octave  'c ;
	e 'g	d 'g	c 'g	'b 'g	e 'g	d 'g	c 'g	'b 'g
	%%%% var 1
	r8 e8() c  	r8 e8() c  	r8 f8()c 	r8 e8()c
	r8 d8()'b 	r8 c8()'a 	r8 'a8()'f 	r8 'e8()'c
}

\score{
	\staff{ melodicregs melodie }
	\staff{ melodicregs begeleiding }
	\paper{
		\unitspace 2.5\cm	% a whole note takes 2.5 \cm ideally.
	}
}

