\version "0.1.15";


blah = \melodic{
	\meter 4/4;
	\octave c';
	\key bes es as;
	[cis8 d e f] [e! a b cis'] |
	es e f e a a' a  a' |
 \clef "bass";       \octave c; [es16 c' b a] [g f es d] [c d es d] [c Bes As G] |

\clef "violin";
	\key ;
	cis4 c cis cis |
	<cis dis eis fes ges> cis dis2 ~ | 
		\meter 2/4 ;
	dis4 dis ~ | c cis~ | c
}

\score{
	\blah
	
}
