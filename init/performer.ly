%
% setup for Request->Element conversion. Guru-only
%
\translator {
	\type "Staff_performer";
	\accepts Voice;
	\name Staff;
	\consists "Key_performer";
	\consists "Time_signature_performer";
}

\translator
{
	\type "Performer_group_performer";
	\name Thread ;
	\consists "Note_performer";
}
\translator
{
	\type "Performer_group_performer";
	\accepts Thread;
\name Voice;
}
\translator
{
	\type "Performer_group_performer";
	\accepts Staff;

\name GrandStaff;}

\translator {
	\type "Performer_group_performer";
	\consists "Lyric_performer";
\name LyricVoice;
}

\translator{
	\type "Performer_group_performer";
	\name ChoirStaff;
	\accepts Staff;
}
\translator { 
	\type "Staff_performer";
	\accepts LyricVoice;
	\name Lyrics;
	\consists "Time_signature_performer";
}
\translator
{
	\type Performer_group_performer;

	\name StaffGroup;
	\accepts Staff;
}
\translator {
	\type "Score_performer";


	\name Score;
	instrument = piano;
	\accepts Staff;
	\accepts GrandStaff;
	\accepts Lyrics; 
	\accepts StaffGroup;
	\accepts ChoirStaff;
	\consists "Swallow_performer";
}

