%% run this through ly2dvi (afterwards -- urg ly2dvi should tee lily output)
%% on my printer, I use dvips -O 5mm,0mm -o x.ps coriolan

\header{
filename = 	 "coriolan.ly";
%title = 	 "Ouverture"; 
subtitle =  "Coriolan";
opus =  "Op. 62";
composer = 	 "Ludwig van Beethoven (1770-1827)";
enteredby = 	 "JCN";
copyright = 	 "public domain";
}

\version "1.3.120";

\include "global.ly"
\include "paper16.ly"

\include "bassi.ly"
\include "clarinetti.ly"
\include "corni.ly"
\include "fagotti.ly"
\include "flauti.ly"
\include "oboi.ly"
\include "timpani.ly"
\include "trombe.ly"
\include "violi.ly"
\include "violino-1.ly"
\include "violino-2.ly"


legniGroup =  \context StaffGroup = legni_group <
	\flautiStaff
	\oboiStaff
	\clarinettiStaff
	\fagottiStaff
>

ottoniGroup =  \context StaffGroup = otonni_group <
	\corniStaff
	\trombeStaff
>

timpaniGroup =  \context StaffGroup = timpani_group <
	\timpaniStaff
	% Force a staff bracket (?)
	\context Staff = timpany { \skip 1*314; }
>

violiniGroup =  \context GrandStaff = violini_group <
	\violinoIStaff
	\violinoIIStaff
>

archiGroup =  \context StaffGroup = archi_group <
	\violiniGroup
	\violiGroup
	\bassiGroup
>


\score{
	<
		\legniGroup
		\ottoniGroup
		\timpaniGroup
		\archiGroup
	>
	\header{
		title = "Coriolan";
		subtitle = "Ouverture"; 
		opus = "Opus 62";
		composer = "Ludwig van Beethoven (1770-1827)";
		enteredby = "JCN";
		copyright = "public domain";
	}
	\include "coriolan-paper.ly"
%	\include "coriolan-midi.ly"
}

