\header{
filename =	 "rhythm.ly";
enteredby =	 "HWN";
copyright =	 "public domain";
TestedFeatures =	 "multiple meters, beaming, unsynced bars, userdefd engravers";
}



\version "0.1.9";

ritme = \melodic{ %\octave ;
	\partial 8;
	\octave c';
	\meter  4/4;
	c8					|
	
	[a8~  a8. a8 a16 a16 a16] c4.		|	% watch the beams!
	 r32 r32 r16 r8 r4 r2			|
	\meter   5/16;

	% divide measure in 5 equal parts. Usually it 2+3 or 3+2
	\grouping  16*5 ;	
	[c8 c16 c8 ]				|	% watch THIS!
	 [5/4 c16 c16 c16 c16]1/1 |
	\meter   2/8;
	c4 				|
	c4	c4	c4	c4
	\meter 4/4;
	c1 c1 c1
	
%	[c16 c16 c16 c16 ]			|
%	[c16 c16 c16 c16 c16 ]			|
%	[c16 c16 c16 c16 c16 ]			|	
	
	 }
	

another = 
	\melodic{ \meter 4/4; 
		c1.  c1. c4 c4 c4 c4  \meter  4/4; c1 c1 c1
	 }


yanother = 
	\melodic{ \meter 4/4; 
		c1 c1 c1 c4 c4 c4 c4  c1 c1 c1
	 }


\score{
	\type Staff_group <
		\ritme
		\another
		\yanother
	>
	
	\paper{

	%% remove Timing_engraver and Bar_number_engraver
	Score = \translator {
	\type Score_engraver;

	%\consists "Timing_engraver";
	%\consists "Bar_column_engraver";
	%\consists "Bar_number_engraver";

	\consists "Span_score_bar_engraver";
	\consists "Score_priority_engraver";
	\consists "Priority_horizontal_align_engraver";
	\consists "Vertical_align_engraver";


	\accepts "Staff_group";
	\accepts "Staff";
	\accepts "Lyrics";
	\accepts "Grandstaff";
}

	%% add Timing_engraver to the staff
	Staff = \translator {
	  \type "Engraver_group_engraver";
	defaultclef=	violin;

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
