


\version "1.3.110";

\include "menuetto-urtext.ly";

menuetto_i_cello_global = \notes{
	\time 3/4;
	\key f \major;
	\clef bass;
	\repeat "volta" 2 {
		\skip 2.*8;
	}
	\repeat "volta" 2 {
		\skip 2.*1;
		\slurDotted
		\skip 2.*14;
		\emptyText
		s2._"Fine"
	}
}

menuetto_i_cello_scripts = \notes{
	\context Voice=i
	s2.
	s8^"~"^1_2_4 s8 s4 s^4
	s4^0_1 s_4 s
	s2.*5
	s2^3 s4
	s4 s8_1 s s4
	s2.
	s2 s8^4 s
	s2.
	s8 s^2 s^4
	s_2 s s s_0 s_4 s_1
	s2.*2
	s4^3_1
	s^1_3 s4
	s2.
	s4_2 s2
	s8^2_3 s s s^1 s4^1
}

menuetto_i_cello_staff = \context Staff <
	\$menuetto_i
	\$menuetto_i_cello_global
%	\$menuetto_i_cello_scripts
>

\score{
	\$menuetto_i_cello_staff
	\paper{
		\translator{
			\VoiceContext
			autoBeamSettings \override #'(end 1 8 * *) = #(make-moment 3 4)
		}
	}
	\midi{ \tempo 4 = 110; }
	\header{
	opus= "" ; 
	piece ="Menuetto I"; }
}

menuetto_ii_cello_global = \notes{
	\time 3/4;
	\key d \major;
	\clef bass;
	\repeat "volta" 2 {
		\skip 2.*8;
	}
	\repeat  "volta" 2 {
		\skip 2.*1;
		\slurDotted
		\skip 2.*14;
		\emptyText
		s2._"Menuetto I da Capo"
	}

}

menuetto_ii_cello_staff = \context Staff <
	\$menuetto_ii
	\$menuetto_ii_cello_global
%	\$menuetto_ii_cello_scripts
>

\score{
	\$menuetto_ii_cello_staff
	\paper{
		\translator{
			\VoiceContext
			autoBeamSettings \override #'(end 1 8 * *) = #(make-moment 3 4)

		}
	}
	\midi{ \tempo 4 = 130; }
	\header{
		piece = "Menuetto II";
		opus = "";
	}
}

