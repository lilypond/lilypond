
\version "1.0.21";

\score{
	<
	\context Staff = a \notes\relative c <
%		{\grace b''8 \graceat a4 \ecarg g fis2 | a2 a }
		{\tiny b''8*1/16 \normalsize a4*31/32 g fis2 | a2 a }
	>
	\context Staff = b \notes\relative c <
%		{\grace g''16 b16 \graceat a4 \ecarg g fis2 | }
		{\tiny g''16*1/16 b16*1/16 \normalsize a4*31/32 g fis2 | a1 }
	>
	\context Staff = c \notes\relative c <
%		{\grace \times 2/48 { [ g''16 b g] } \graceat a4 \ecarg g fis2 | a1 }
		{\tiny \times 2/48 { [ g''16 b g] } \normalsize a4*31/32 g fis2 | a1 }
	>
	>
	\paper {
		linewidth = 120.0\mm;
		\translator { \VoiceContext  }
	}
}

