\version "1.0.7";


blah = \notes {

\transpose c'' {
	\time 4/4;
	\keysignature  es;
	[cis8 d e f] [e! a b cis'] |
	es e f e a a' a  a' |
 \clef "bass";  }
  [es16 c' b a] [g f es d] [c d es d] [c Bes As G] |

\clef "violin";
	\keysignature  c;
	cis4 c cis cis |
	<cis dis eis fes ges> cis dis2 ~ | 
		\time 2/4 ;
	dis4 dis ~ | c cis~ | c
}

\score{
	\type Staff \blah
	
}
