\version "1.9.8"

%
% setup for Request->Element conversion. Guru-only
%
\translator {
	\type "Staff_performer"
	\name Staff
	\accepts Voice

	\consists "Key_performer"
	\consists "Tempo_performer"
	\consists "Time_signature_performer"

}

\translator {
	\type "Performer_group_performer"
	\name Voice
	\consists "Dynamic_performer"
	\consists "Span_dynamic_performer"
	\consists "Tie_performer"
	\consists "Piano_pedal_performer"
	\accepts "Thread"
}

\translator {
	\type "Performer_group_performer"
	\name Thread
	\consists "Note_performer"
}

\translator {
	\type "Performer_group_performer"
	\name FiguredBass 
	\consists "Swallow_performer"
}

\translator {
	\type "Performer_group_performer"
	\name GrandStaff
	\accepts RhythmicStaff
	\accepts Staff
}

\translator {
        \type "Performer_group_performer"
	\name "PianoStaff"
	\accepts Staff
}

\translator {
        \type "Performer_group_performer"
	\name "TabVoice"
	\consists "Swallow_performer"
}

\translator {
        \type "Performer_group_performer"
	\name "TabStaff"
	\accepts "TabVoice"
}

\translator {
	\type "Score_performer"

	\name Score
	\alias Timing
	instrument = #"bright acoustic"
	\accepts Staff
	\accepts GrandStaff
	\accepts PianoStaff
	\accepts TabStaff
	\accepts Lyrics 
	\accepts StaffGroup
	\accepts Devnull
	\accepts ChoirStaff
	\accepts RhythmicStaff
	\accepts ChordNames
	\accepts FiguredBass

	\alias "Timing"
	\consists "Timing_translator"
	\consists "Swallow_performer"
	
	dynamicAbsoluteVolumeFunction = #default-dynamic-absolute-volume
	instrumentEqualizer = #default-instrument-equalizer
}


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
	\type "Performer_group_performer"
	\consists "Note_performer"
	\name ChordNameVoice	
}

\translator {
	\type "Performer_group_performer"

	\name StaffGroup
	\accepts Staff
}

\translator { \StaffContext \name RhythmicStaff }


