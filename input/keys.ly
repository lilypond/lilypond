%{MudelaHeader

 filename: keys.ly
 title:
 description: 
 composers:
 entered-by:
 copyright:

 Tested Features: local key, key
EndMudelaHeader
%}
\version "0.0.57";


blah = \melodic{
	\duration 4;
	\meter 4/4;
	\octave c';
	cis c cis cis |
	<cis dis eis fes ges> cis dis2 ~ | 
		\meter 2/4 ;
	dis dis ~ | c cis~ | c 
}

\score{
	\staff { 
		melodicregs
		blah
	}
}
