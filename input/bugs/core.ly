% core dumps 

global = \notes {
	\key a \minor;
	\time 6/4;
}

\score{
	\notes \context PianoStaff <
		\global
		\context Staff=up { c }
		%\context Staff=down { \autochange Staff c }
		\context Staff=down { c }
	>
	\paper {
		\translator{ 
			\StaffContext

			% other core dump when this is removed?
			\remove "Time_signature_engraver";
		}
	}
}

