%
% setup for Request->Element conversion. Guru-only
%
\translator {
	\type "Staff_performer";
	\accepts Voice;
	Staff;
	\consists "Key_performer";
	\consists "Time_signature_performer";
}

\translator
{
	\type "Performer_group_performer";
	Thread ;
	\consists "Note_performer";
}
\translator
{
	\type "Performer_group_performer";
	\accepts Thread;
Voice;
}
\translator
{
	\type "Performer_group_performer";
	\accepts Staff;

GrandStaff;}

\translator {
	\type "Performer_group_performer";
	\consists "Lyric_performer";
LyricVoice;
}

\translator{
	\type "Performer_group_performer";
	ChoirStaff;
	\accepts Staff;
}
\translator { 
	\type "Staff_performer";
	\accepts LyricVoice;
	Lyrics;
	\consists "Time_signature_performer";
}
\translator
{
	\type Performer_group_performer;

	StaffGroup;
	\accepts Staff;
}
\translator {
	\type "Score_performer";


	Score;
	instrument = piano;
	\accepts Staff;
	\accepts GrandStaff;
	\accepts Lyrics; 
	\accepts StaffGroup;
	\accepts ChoirStaff;
	\consists "Swallow_performer";
}

