%{MudelaHeader

 filename: slurs.ly
 title: 
 description: 
 composer(s): HWN
 entered-by: HWN
 copyright: GPL

 Tested Features: test slurs and ties
EndMudelaHeader
%}

\version "0.0.60";

\score{
	\melodic{\octave c';
		\duration "last";
		'c4 ~ 'c c'' ~ c'' ~ g ~ c ~
		d ~ e ~ f ~ g ~
		a ~ b ~ 'c ~ c

		< c e ( > <)d f> 
		\octave c'';
		< c e ( > <)d f>
		\octave c';
		< c e ( > <)d' f'>
		< { c ~ d }
		  { e ~ f }
		>
	< \multi 2;
	{ \stem 1; c'8 ~g ~e( ~c~ [c'~ g' e' )c'] c'' ~ c'' c~c }
	{ \stem -1; c (e g )c'~( [b a g )d]   	r2 } 
	>

	}
}
