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

\version "0.1.0";

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
	{ \stem \up; c'8 ~g ~e( ~c~ [c'~ g' e' )c'] c'' ~ c'' c~c }
	{ \stem \down; c (e g )c'~( [b a g )d]   	r2 } 
	>

	}
}
