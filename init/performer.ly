%
% setup for Request->Element conversion. Guru-only
%

Staff =	\translator {
	\type "Staff_performer";
	\accepts Voice;
	\consists "Key_performer";
	\consists "Time_signature_performer";
}

Thread =\translator
{
	\type "Performer_group_performer";
	\consists "Note_performer";
}

Voice = \translator
{
	\type "Performer_group_performer";
	\accepts Thread;
}

GrandStaff = \translator
{
	\type "Performer_group_performer";
	\accepts Staff;
}

LyricVoice = \translator {
	\type "Performer_group_performer";
	\consists "Lyric_performer";
}


Lyrics = \translator { 
	\type "Staff_performer";
	\accepts LyricVoice;
	\consists "Time_signature_performer";
}

StaffGroup = \translator
{
	\type Performer_group_performer;
	\accepts Staff;
}

Score = \translator {
	\type "Score_performer";
	instrument = piano;
	\accepts Staff;
	\accepts GrandStaff;
	\accepts Lyrics; 
	\accepts StaffGroup;
	\consists "Swallow_performer";
}

