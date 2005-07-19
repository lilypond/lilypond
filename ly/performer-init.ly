\version "2.6.0"

				%
				% setup for Request->Element conversion. Guru-only
				%
\context {
    \type "Performer_group_performer"
    \name Staff
    \accepts Voice
    
    \consists "Staff_performer"
    \consists "Key_performer"
    \consists "Tempo_performer"
    \consists "Time_signature_performer"
}
\context {
    \name Global
    \accepts Score
\description "Hard coded entry point for LilyPond. Cannot be tuned."
    }
\context {
    \Staff
    \name DrumStaff
    midiInstrument = #"drums"
    \accepts DrumVoice
}

\context {
    \type "Performer_group_performer"
    \name Voice
    % The order of the dynamic performers is significant: absolute dynamic events must override crescendo events in midi.
    \consists "Span_dynamic_performer"
    \consists "Dynamic_performer"
    \consists "Tie_performer"
    \consists "Piano_pedal_performer"
    \consists "Note_performer"
    \consists "Beam_performer"
    \consists "Slur_performer"
    \consists "Melisma_translator"
}

\context {
  \Voice
  \name CueVoice
  \alias Voice
}

\context {
    \Voice
    \remove "Note_performer"
    \consists "Drum_note_performer" 
    \name DrumVoice
}

\context {
    \type "Performer_group_performer"
    \name FiguredBass 
    \consists "Swallow_performer"
}

\context {
    \type "Performer_group_performer"
    \name GrandStaff
    \accepts RhythmicStaff
    \accepts Staff
}

\context {
    \type "Performer_group_performer"
    \name "PianoStaff"
    \accepts Staff
    \accepts DrumStaff
}

\context {
    \type "Performer_group_performer"
    \name "TabVoice"
    \consists "Swallow_performer"
}

\context {
    \type "Performer_group_performer"
    \name "Devnull"
    \consists "Swallow_performer"
}
\context {
    \type "Performer_group_performer"
    \name "TabStaff"
    \accepts "TabVoice"
}

\context {
    \type "Score_performer"

    \name Score
    
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
    
    \consists "Timing_translator"
    \consists "Swallow_performer"
    
    dynamicAbsoluteVolumeFunction = #default-dynamic-absolute-volume
    instrumentEqualizer = #default-instrument-equalizer
    drumPitchTable = #(alist->hash-table midiDrumPitches) 
}


\context {
    \type "Performer_group_performer"
    \consists "Staff_performer" % Performer_group_performer ?
    \consists "Lyric_performer"
    \name Lyrics
    \consists "Time_signature_performer"
    \consists "Tempo_performer"
}

\context{
    \type "Performer_group_performer"
    \name ChoirStaff
    \accepts Staff
    \accepts DrumStaff
}


\context {
    \type "Performer_group_performer"
    \consists "Staff_performer"
    \accepts ChordNameVoice
    \name ChordNames
}

\context {
    \type "Performer_group_performer"
    \consists "Note_performer"
    \name ChordNameVoice	
}

\context {
    \type "Performer_group_performer"

    \name StaffGroup
    \accepts Staff
    \accepts DrumStaff
}

\context { \Staff \name RhythmicStaff }


