

\include "paper-as9.ly"

\score {
	\context GrandStaff <
		\context Staff=upper \notes\relative c{
			\key f \major;
			\time 3/4;
			r8^"Moderato" %\pp 
			<g'-. c-.> <c-. es-.> <g-. c-.> <c-. es-.> <g-. c-.> |
			r8 <as-. c-.> <c-. es-.>
		}
		\context Staff=lower \notes\relative c{
			\key f \major;
			\time 3/4;
			\clef "bass";
			<c,2 c'> r4 
			<as2 as'> r4
		}
	>
	\paper {
%		\paperAs_nine
		indent=4.0\char;
		linewidth=78.0\char;
    		%\translator { \StaffContext barSize = #9 }
		%\translator { \VoiceContext beamHeight = #0 }
		\translator { 
			\VoiceContext 
			beamHeight = ##f 
			autoBeamSettings \override (begin * * * *) = #(make-moment 0 1)
			textNonEmpty = ##f
		}
	}

}

