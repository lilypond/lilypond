
\version "1.0.14";

\score{
	\notes\relative c'' {
		% set either property -> core dump: two autobeams?
%		\property Voice.fontsize= "-2"
		\property Voice.pletvisibility = 0

		% strange staffs, but no core dump
		\times 2/3 { [ c16 c c] } c4 c2
	}
	\paper {
		\translator { \VoiceContext
%			\remove Auto_beam_engraver;
		}
	}
}

