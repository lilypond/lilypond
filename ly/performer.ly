%
% setup for Request->Element conversion. Guru-only
%
\translator {
	\type "Staff_performer";
	\name Staff;
	\accepts Voice;

	\accepts VoiceOne;		% ugh.
	\accepts VoiceTwo;
	\accepts VoiceThree;
	\accepts VoiceFour;

	\consists "Key_performer";
	\consists "Time_signature_performer";
}

%% urg, why (needs praeludium-*.ly) these?
\translator
{
	\type "Performer_group_performer";
	\consists "Note_performer";
	 \name VoiceFour;
}

\translator
{
	\type "Performer_group_performer";
	\consists "Note_performer";
	\name VoiceThree;
}
\translator
{
	\type "Performer_group_performer";
	\consists "Note_performer";
	 \name VoiceOne;
}

\translator
{
	\type "Performer_group_performer";
	\name Voice;
	\consists "Note_performer";
	\consists "Tie_performer";
%	\accepts Grace;
}

\translator {
	\type "Grace_performer_group";
	\name "Grace";
	\consists "Note_performer";
	\consists "Tie_performer";
	 weAreGraceContext = "1";
}

\translator
{
	\type "Performer_group_performer";
	\name VoiceTwo;\consists "Note_performer";

}

\translator
{
	\type "Performer_group_performer";
	\accepts Staff;
	\name GrandStaff;
}

\translator {\type "Performer_group_performer";
	\accepts Staff; \name "PianoStaff";}

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
	instrument = "bright acoustic";
	\accepts Staff;
	\accepts GrandStaff;
	\accepts PianoStaff;
	\accepts Lyrics; 
	\accepts StaffGroup;
	\accepts ChoirStaff;
	\consists "Swallow_performer";
}

