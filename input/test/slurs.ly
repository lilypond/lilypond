
\version "1.0.7";

\score{
	\notes\transpose c'{

		c,4 ~ c, c'' ~ c'' ~ g ~ c ~
		d ~ e ~ f ~ g ~
		a ~ b ~ c, ~ c

		< c e ( > <)d f> 
		\transpose c''
		< c e ( > <)d f>
		\transpose c'
		< c e ( > <)d' f'>
		< { c ~ d }
		  { e ~ f }
		>
%	\type Staff <
%	{ \stemup c'8 ~g ~e( ~c~ [c'~ g' e' )c'] c'' ~ c'' c~c }
%	{ \stemdown c (e g )c'~( [b a g )d]   	r2 } 
%	>

	{

		c4()c( c c  |
		c c c c |
		c c c c |
		c c c )c |
	}
	}
}
