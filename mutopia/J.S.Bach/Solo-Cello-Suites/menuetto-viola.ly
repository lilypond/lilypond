


\version "1.3.117";

\include "menuetto-urtext.ly";

menuettoIViolaGlobal =  \notes{
	\time 3/4;
	\key f \major;
	\clef alto;
	\repeat "volta" 2 {
		\skip 2.*8;
		\clef violin;
		\skip 2.*1;
	} \repeat "volta" 2 {
		\slurDotted
		\skip 2.*3;
		\clef alto;
		\skip 2.*11;
		\emptyText
		s2._"Fine"
	}
}

menuettoIViolaScripts =  \notes{
	\context Voice=i
	s2.
	s8^"~"^1_2_4 s8*5
	s2.*5
	s4 s-\upbow s-\downbow
	s2.-\upbow
	s2.*5
	s2 s4-\upbow
	s4-\downbow s2
	s2.*1
	s2^0 s4
	s2.*1
	s4-\downbow s4-\upbow
}

menuettoIViolaStaff =  \context Staff <
	\notes \transpose c' \menuettoI
	\menuettoIViolaGlobal
%	\menuettoIViolaScripts
>

\score{
	\menuettoIViolaStaff
	\paper{
		\include "scs-paper.ly";
		gourlay_maxmeasures = 7.0;
		\translator{
			\VoiceContext
			autoBeamSettings \override #'(end 1 8 * *) = #(make-moment 3 4)

		}
	}
	\midi{ \tempo 4 = 110; }
	\header{
		opus= "" ; 
		piece ="Menuetto I";
	}
}

menuettoIiViolaGlobal =  \notes{
	\time 3/4;
	\key d \major;
	\clef alto;
	\repeat "volta" 2 {
		\skip 2.*8;
	} \repeat "volta" 2 {
		\skip 2.*1;
		\slurDotted
		\skip 2.*14;
		\emptyText
		s2._"Menuetto I da Capo"
	}
}

menuettoIiViolaStaff =  \context Staff <
	\notes \transpose c' \menuettoIi
	\menuettoIiViolaGlobal
%	\menuettoIiViolaScripts
>

\score{
	\menuettoIiViolaStaff
	\paper{
		\translator{
			\VoiceContext
			autoBeamSettings \override #'(end 1 8 * *) = #(make-moment 3 4)

		}
	}
	\midi{ \tempo 4 = 130; }
	\header{
	opus= "" ; 
	piece ="Menuetto II"; }
}

