
\version "0.1.9";

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
%	\multi 2 <
%	{ \stemup c'8 ~g ~e( ~c~ [c'~ g' e' )c'] c'' ~ c'' c~c }
%	{ \stemdown c (e g )c'~( [b a g )d]   	r2 } 
%	>

	{
%		\octave c';
%		'c16( 'e( 'g( 'b( d( f( a( c'~ )))))))c4 c4 |
%		\octave c''';
%		'c16( a( f( d( 'b( 'g( 'e( 'c~ )))))))c4 c4 |
%		\octave c';
%		c'1() g' () c () g () c |

		c4()c( c c  |
		c c c c |
		c c c c |
		c c c )c |
	}
	}
	}
}
