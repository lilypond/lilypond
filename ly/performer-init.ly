\version "1.3.146"
%
% setup for Request->Element conversion. Guru-only
%
StaffContext = \translator {
	\type "Staff_performer"
	\name Staff
	\accepts Voice

	\consists "Key_performer"
	\consists "Tempo_performer"
	\consists "Time_signature_performer"

}
\translator { \StaffContext }
\translator { \StaffContext
  \name RhythmicStaff
}


VoiceContext = \translator {
	\type "Performer_group_performer"
	\name Voice
	\consists "Dynamic_performer"
	\consists "Span_dynamic_performer"
	\consists "Piano_pedal_performer"
	\accepts "Thread"
}
\translator { \VoiceContext }

ThreadContext = \translator {
	\type "Performer_group_performer"
	\name Thread
	\consists "Note_performer"
	\consists "Tie_performer"
}
\translator { \ThreadContext }

% retain for compatibility reasons (FIXME: convert-ly)
\translator {
	\type "Performer_group_performer"
	\name Grace
}

\translator
{
	\type "Performer_group_performer"
	\name VoiceTwo\consists "Note_performer"

}

GrandStaffContext = \translator {
	\type "Performer_group_performer"
	\name GrandStaff
	\accepts RhythmicStaff
	\accepts Staff
}
\translator { \GrandStaffContext }

PianoStaffContext = \translator {
        \type "Performer_group_performer"
	\name "PianoStaff"
	\accepts Staff
}
\translator { \PianoStaffContext }

\translator {
	\type "Performer_group_performer"
	\consists "Lyric_performer"
	\name LyricsVoice
}

\translator{
	\type "Performer_group_performer"
	\name ChoirStaff
	\accepts Staff
}
\translator { 
	\type "Staff_performer"
	\accepts LyricsVoice
	\name Lyrics
	\consists "Time_signature_performer"
	\consists "Tempo_performer"
}

\translator {
	\type "Staff_performer"
	\accepts ChordNameVoice
	\name ChordNames
}

\translator {
	\type Performer_group_performer
	\consists Note_performer
	\name ChordNameVoice	
}

\translator {
	\type Performer_group_performer

	\name StaffGroup
	\accepts Staff
}

ScoreContext = \translator {
	\type "Score_performer"

	\name Score
	instrument = #"bright acoustic"
	\accepts Staff
	\accepts GrandStaff
	\accepts PianoStaff
	\accepts Lyrics 
	\accepts StaffGroup
	\accepts ChoirStaff
	\accepts RhythmicStaff
	\accepts ChordNames

	\consists "Timing_translator"
	\consists "Swallow_performer"
	
	dynamicAbsoluteVolumeFunction = #default-dynamic-absolute-volume
	instrumentEqualizer = #default-instrument-equalizer
}
\translator { \ScoreContext }

