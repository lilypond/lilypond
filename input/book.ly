%{
MudelaHeader

 filename: book.ly
 title:
 description: demonstrate inclusion of Lily output into (La)TeX
documents
 composers:
 entered-by:
 copyright:

 Tested Features: natural width
EndMudelaHeader
%}
\version "0.0.61";



\score {\melodic { \clef "bass";
	[c8 g e' d'] [e' g e' g]
	[c  a f' e'] [f' a f' a]
	}
	
	\paper { 
		\output "bach1.out";
		linewidth =-1.0\cm;
	}
}

\score {\melodic { \clef "bass";
	[c8() g e'() d'] [e'( g e') g]
	[c()  a f' ()e'] [f'( a f') a]
		}
	\paper { 
		\output "bach2.out";
		linewidth= -1.0\cm;
	}
}

\score {\melodic { \clef "bass";
	[c8 g( e' d'] [)e' g( e' )g]
	[c  a( f' e'] [)f' a( f' )a]
		}
	\paper { 
		\output "bach3.out";
		linewidth = -1.0\cm;
		unitspace = 2.0\cm;
	}
}
