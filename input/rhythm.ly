\header{
filename =	 "rhythm.ly";
enteredby =	 "HWN";
copyright =	 "public domain";
TestedFeatures =	 "multiple meters, beaming, unsynced bars, userdefd engravers";
}



\version "1.0.7";

ritme = \notes\transpose c'' {
	\time  4/4;
	\partial 8;
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
	\notes{ \time 6/4; 
		c1.  c1. \time 4/4;c4 c4 c4 c4  c1 c1 c1
	 }


%
% Beams are incorrect for the next staff.  They cut through the whole notes.
%

yanother = 
	\notes{ \time 4/4; 
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
 \translator {
 \ScoreContext
 \remove "Timing_engraver";
  }
  \translator {
  \RhythmicStaffContext
  \consists "Timing_engraver";
  }
  \translator{
  \StaffContext
  \consists "Timing_engraver";
  }
	}
}
