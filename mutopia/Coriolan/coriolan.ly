\version "1.3.120";

instrument = "Orchestra"
% instrument = ""

\include "header.ly"
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
\include "viole.ly"
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
	\context Staff = timpany \End
>

violiniGroup =  \context GrandStaff = violini_group <
	\violinoIStaff
	\violinoIIStaff
>

archiGroup =  \context StaffGroup = archi_group <
	\violiniGroup
	\violeGroup
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
		instrument = "";
	}
	\include "coriolan-paper.ly"
	\include "coriolan-midi.ly"
}

