
\version "1.0.7";

\include "table13.ly";
\include "table16.ly";

\score{
	<
	\type Staff = a \notes\relative c <
%		{\grace b''8 \graceat a4 \ecarg g fis2 | a2 a }
		{\tiny b''8*1/16 \normalsize a4*31/32 g fis2 | a2 a }
	>
	\type Staff = b \notes\relative c <
%		{\grace g''16 b16 \graceat a4 \ecarg g fis2 | }
		{\tiny g''16*1/16 b16*1/16 \normalsize a4*31/32 g fis2 | a1 }
	>
	\type Staff = c \notes\relative c <
%		{\grace [2/48 g''16 b g]/1 \graceat a4 \ecarg g fis2 | a1 }
		{\tiny [2/48 g''16 b g]/1 \normalsize a4*31/32 g fis2 | a1 }
	>
	>
	\paper {
		linewidth = 120.0\mm;
		-2 = \symboltables { \table_thirteen }	
		-1 = \symboltables { \table_sixteen }

	}
}

