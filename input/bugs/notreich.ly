
% GENERATED AUTOMATICALLY

\header {
  title = "Not Clapping Music" ;
  instrument = "For four hands";
  composer = "Not Steve Reich";
  year = "1972";
}


beaming = \notes \repeat unfold 13 {  \repeat unfold 3 { [ s2 ] } \bar ":|:"; }

\score {
	\notes <
%	        \property Score.midiInstrument = "woodblock"
	        \property Score.midiInstrument = "melodic tom"
		\context RhythmicStaff = SA 
		  \context Voice = VA <
		  \time 12/8 ;
		       { c8 c8 c8 r c8 r c8 c8 r c8 c8 r c8 c8 c8 r c8 r c8 c8 r c8 c8 r c8 c8 c8 r c8 r c8 c8 r c8 c8 r c8 c8 c8 r c8 r c8 c8 r c8 c8 r c8 c8 c8 r c8 r c8 c8 r c8 c8 r c8 c8 c8 r c8 r c8 c8 r c8 c8 r c8 c8 c8 r c8 r c8 c8 r c8 c8 r c8 c8 c8 r c8 r c8 c8 r c8 c8 r c8 c8 c8 r c8 r c8 c8 r c8 c8 r c8 c8 c8 r c8 r c8 c8 r c8 c8 r c8 c8 c8 r c8 r c8 c8 r c8 c8 r c8 c8 c8 r c8 r c8 c8 r c8 c8 r c8 c8 c8 r c8 r c8 c8 r c8 c8 r }  \beaming
			s8^"No accents whatsoever"
			{ s1.  \mark "8$\\times$"; }


		  > 
		\context RhythmicStaff = SB
		   \context Voice = VA < { c8 c8 c8 r c8 r c8 c8 r c8 c8 r c8 c8 r c8 r c8 c8 r c8 c8 r c8 c8 r c8 r c8 c8 r c8 c8 r c8 c8 r c8 r c8 c8 r c8 c8 r c8 c8 c8 c8 r c8 c8 r c8 c8 r c8 c8 c8 r r c8 c8 r c8 c8 r c8 c8 c8 r c8 c8 c8 r c8 c8 r c8 c8 c8 r c8 r c8 r c8 c8 r c8 c8 c8 r c8 r c8 r c8 c8 r c8 c8 c8 r c8 r c8 c8 c8 c8 r c8 c8 c8 r c8 r c8 c8 r c8 r c8 c8 c8 r c8 r c8 c8 r c8 r c8 c8 c8 r c8 r c8 c8 r c8 c8 c8 c8 c8 r c8 r c8 c8 r c8 c8 r }  \beaming >
	>
	\paper{ \translator { \ScoreContext \consists Mark_engraver; }}
	\midi {\tempo 4 = 130;}
	} 
