\header{
filename =	 "rhythm.ly";
enteredby =	 "HWN";
copyright =	 "public domain";
TestedFeatures =	 "multiple meters, beaming, unsynced bars, userdefd engravers";
}



\version "1.0.0";

ritme = \melodic\transpose c'' {
	\partial 8;

	\time  4/4;
	c8					|
	
	[a8~  a8. a8 a16 a16 a16] c4.		|	% watch the beams!
	 r32 r32 r16 r8 r4 r2			|
	\time   5/16;

	% divide measure in 5 equal parts. Usually it 2+3 or 3+2
	\grouping  16*5 ;	
	[c8 c16 c8 ]				|	% watch THIS!
	 [5/4 c16 c16 c16 c16]1/1 |
	\time   2/8;
	c4 				|
	c4	c4	c4	c4
	\time 4/4;
	c1 c1 c1
	
%	[c16 c16 c16 c16 ]			|
%	[c16 c16 c16 c16 c16 ]			|
%	[c16 c16 c16 c16 c16 ]			|	
	
	 }
	

another = 
	\melodic{ \time 4/4; 
		c1.  c1. c4 c4 c4 c4  \time  4/4; c1 c1 c1
	 }


yanother = 
	\melodic{ \time 4/4; 
		c1 c1 c1 c4 c4 c4 c4  c1 c1 c1
	 }


\score{
	\type StaffGroup <
		\ritme
		\another
		\type RhythmicStaff {
			\yanother
		}
	>
	
	\paper{

	%% remove Timing_engraver and Bar_number_engraver
	Score = \translator {
	\type Score_engraver;

	%\consists "Timing_engraver";

	\consists "Span_score_bar_engraver";
	\consists "Score_priority_engraver";
	\consists "Priority_horizontal_align_engraver";
	\consists "Vertical_align_engraver";


	\accepts "StaffGroup";
	\accepts "Staff";
	\accepts "Lyrics";
	\accepts "GrandStaff";
}

RhythmicStaff = \translator
{
	  \type "Engraver_group_engraver";
	nolines  = "1";
	  \consists "Pitch_squash_engraver";

	\consists "Bar_column_engraver";
	\consists "Bar_number_engraver";
	  \consists "Bar_engraver";
	  \consists "Meter_engraver";
	  \consists "Staff_sym_engraver";
	  \consists "Line_group_engraver";
	  \consists "Timing_engraver";
	  \accepts "Voice";
}
	%% add Timing_engraver to the staff
	Staff = \translator {
	  \type "Engraver_group_engraver";
	defaultclef=	violin;

	\consists "Bar_column_engraver";
	\consists "Bar_number_engraver";
	\consists "Timing_engraver";
	  \consists "Bar_engraver";
	  \consists "Clef_engraver";
	  \consists "Key_engraver";
	  \consists "Meter_engraver";
	  \consists "Local_key_engraver";
	  \consists "Staff_sym_engraver";
	  \consists "Collision_engraver";
	  \consists "Rest_collision_engraver";

	  \consists "Line_group_engraver";
	  \accepts "Voice";
	}
	}
}
