\version "2.1.23"

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
    \name Global
    \accepts Score
    }
\translator {
    \StaffContext
    \name DrumStaff
    midiInstrument = #"drums"
    \accepts DrumVoice
}

\translator {
    \type "Performer_group_performer"
    \name Voice
    \consists "Dynamic_performer"
    \consists "Span_dynamic_performer"
    \consists "Tie_performer"
    \consists "Piano_pedal_performer"
}

\translator {
    \VoiceContext
    \remove "Note_performer"
    \consists "Drum_note_performer" 
    \name DrumVoice
}

\translator {
    \type "Performer_group_performer"
    \name Voice
    \consists "Note_performer"
    \consists "Beam_performer"
    \consists "Slur_performer"
    \consists "Melisma_performer"
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
    \accepts DrumStaff
}

\translator {
    \type "Performer_group_performer"
    \name "TabVoice"
    \consists "Swallow_performer"
}

\translator {
    \type "Performer_group_performer"
    \name "Devnull"
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
    melismaBusyProperties = #default-melisma-properties
    instrument = #"bright acoustic"
    \accepts Staff
    \accepts DrumStaff
    \accepts GrandStaff
    \accepts PianoStaff
    \accepts TabStaff
    \accepts Staff
    \accepts StaffGroup
    \accepts Devnull
    \accepts ChoirStaff
    \accepts RhythmicStaff
    \accepts ChordNames
    \accepts FiguredBass
    \accepts Lyrics
    \alias "Timing"
    \consists "Timing_translator"
    \consists "Swallow_performer"
    
    dynamicAbsoluteVolumeFunction = #default-dynamic-absolute-volume
    instrumentEqualizer = #default-instrument-equalizer
    drumPitchTable = #(alist->hash-table midiDrumPitches) 
}


\translator {
    \type "Staff_performer" % Performer_group_performer ?
    \consists "Lyric_performer"
    \name Lyrics
    \consists "Time_signature_performer"
    \consists "Tempo_performer"
}

\translator{
    \type "Performer_group_performer"
    \name ChoirStaff
    \accepts Staff
    \accepts DrumStaff
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
    \accepts DrumStaff
}

\translator { \StaffContext \name RhythmicStaff }


