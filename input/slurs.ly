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

\version "0.0.57";

\score{\staff{
	melodicregs 
	\melodic{\octave c';
		\duration 4;
		c ~ c ~ g ~ c ~
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



	}

	}
}
