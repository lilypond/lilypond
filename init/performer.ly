%
% setup for Request->Element conversion. Guru-only
%

Staff =	\translator {
	\type "Staff_performer";
	\accepts Voice;
	\consists "Key_performer";
	\consists "Meter_performer";
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

Grand_staff = \translator
{
	\type "Performer_group_performer";
	\accepts Staff;
}

Lyric_voice = \translator {
	\type "Performer_group_performer";
	\consists "Lyric_performer";
}


Lyrics = \translator { 
	\type "Staff_performer";
	\accepts Lyric_voice;
	\consists "Meter_performer";
}

Staff_group = \translator
{
	\type Performer_group_performer;
	\accepts Staff;
}

Score = \translator {
	\type "Score_performer";
	instrument = piano;
	\accepts Staff;
	\accepts Grand_staff;
	\accepts Lyrics; 
	\accepts Staff_group;
	\consists "Swallow_performer";
}

