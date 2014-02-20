%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 1996--2012 Han-Wen Nienhuys <hanwen@xs4all.nl>
%%%%                          Jan Nieuwenhuizen <janneke@gnu.org>
%%%%
%%%% LilyPond is free software: you can redistribute it and/or modify
%%%% it under the terms of the GNU General Public License as published by
%%%% the Free Software Foundation, either version 3 of the License, or
%%%% (at your option) any later version.
%%%%
%%%% LilyPond is distributed in the hope that it will be useful,
%%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%% GNU General Public License for more details.
%%%%
%%%% You should have received a copy of the GNU General Public License
%%%% along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.

\version "2.17.14"

%%
%% setup for Request->Element conversion.
%%
\context {
  \type "Performer_group"
  \name Staff
  \accepts Voice
  \accepts CueVoice
  \accepts NullVoice
  \defaultchild Voice

  \consists "Staff_performer"
  \consists "Key_performer"
  \consists "Midi_control_function_performer"
}

\context {
  \name Global
  \accepts Score
  \defaultchild Score
  \description "Hard coded entry point for LilyPond.  Cannot be tuned."
}

\context {
  \type "Performer_group"
  \name KievanStaff
  \denies Voice
  \accepts KievanVoice
  \defaultchild KievanVoice
  \alias Staff
  \consists "Staff_performer"
  \consists "Key_performer"
  \consists "Midi_control_function_performer"
}

\context {
  \type "Performer_group"
  \name VaticanaStaff
  \alias Staff
  \denies Voice
  \accepts VaticanaVoice
  \defaultchild VaticanaVoice
  \consists "Staff_performer"
  \consists "Key_performer"
  \consists "Midi_control_function_performer"
}

\context {
  \type "Performer_group"
  \name MensuralStaff
  \denies Voice
  \accepts MensuralVoice
  \defaultchild MensuralVoice
  \alias Staff
  \consists "Staff_performer"
  \consists "Key_performer"
  \consists "Midi_control_function_performer"
}

\context {
  \Staff
  \name DrumStaff
  midiInstrument = #"drums"
  \accepts DrumVoice
  \defaultchild DrumVoice
}

\context {
  \type "Performer_group"
  \name Voice
  \consists "Dynamic_performer"
  \consists "Tie_performer"
  \consists "Piano_pedal_performer"
  \consists "Note_performer"
  \consists "Beam_performer"
  \consists "Slur_performer"
}

\context {
  \Voice
  \name CueVoice
  \alias Voice
}

\context {
  \type "Performer_group"
  \name VaticanaVoice
  \alias Voice
  \consists "Dynamic_performer"
  \consists "Tie_performer"
  \consists "Note_performer"
  \consists "Beam_performer"
  autoBeaming = ##f  % needed for consistent melismata with engravers
  \consists "Slur_performer"
}

\context {
  \type "Performer_group"
  \name KievanVoice
  \alias Voice
  \consists "Dynamic_performer"
  \consists "Tie_performer"
  \consists "Note_performer"
  \consists "Beam_performer"
  autoBeaming = ##f  % needed for consistent melismata with engravers
  \consists "Slur_performer"
}

\context {
  \type "Performer_group"
  \name MensuralVoice
  \alias Voice
  \consists "Dynamic_performer"
  \consists "Tie_performer"
  \consists "Note_performer"
  \consists "Beam_performer"
  autoBeaming = ##f  % needed for consistent melismata with engravers
  \consists "Slur_performer"
}

\context {
  \Voice
  \remove "Note_performer"
  \consists "Drum_note_performer"
  \name DrumVoice
}

\context {
  \type "Performer_group"
  \name FiguredBass
}

\context {
  \type "Performer_group"
  \name FretBoards
}

\context {
  \type "Performer_group"
  \name GrandStaff
  \accepts RhythmicStaff
  \accepts Staff
  \accepts Dynamics
  \defaultchild Staff
}

\context {
  \type "Performer_group"
  \name "PianoStaff"
  \accepts Staff
  \accepts DrumStaff
  \defaultchild Staff
}

\context {
  \Voice
  \name TabVoice
}

\context {
  \type "Performer_group"
  \name "Devnull"
}

\context {
  \type "Performer_group"
  \name NullVoice
  \alias Staff
  \alias Voice
  %% needed for melismata
  %% TODO: at least the tie performer likely does not work without the
  %% Note_performer, but I don't know how to shut note output off in
  %% MIDI.
  \consists "Tie_performer"
  \consists "Beam_performer"
  \consists "Slur_performer"
}

\context {
  \Staff
  \name TabStaff
  midiInstrument = #"acoustic guitar (nylon)"
  \alias Staff
  \accepts TabVoice
  \defaultchild TabVoice
  autoBeaming = ##f  % needed for consistent melismata with engravers
}

\context {
  \type "Score_performer"

  \name Score

  melismaBusyProperties = #default-melisma-properties
  instrumentName = #"bright acoustic"
  midiChannelMapping = #'staff

  %% quarter = 60
  tempoWholesPerMinute = #(ly:make-moment 15/1)

  \accepts Staff
  \accepts DrumStaff
  \accepts GrandStaff
  \accepts PianoStaff
  \accepts TabStaff
  \accepts StaffGroup
  \accepts Devnull
  \accepts ChoirStaff
  \accepts RhythmicStaff
  \accepts ChordNames
  \accepts FiguredBass
  \accepts FretBoards
  \accepts Lyrics
  \accepts VaticanaStaff
  \accepts KievanStaff
  \accepts MensuralStaff

  \consists "Time_signature_performer"
  \consists "Control_track_performer"
  \consists "Tempo_performer"

  \alias "Timing"

  %% An alias for Timing is established by the Timing_translator in
  %% whatever context it is initialized, and the timing variables are
  %% then copied from wherever Timing had been previously established.
  %% The alias at Score level provides a target for initializing
  %% Timing variables in layout definitions before any
  %% Timing_translator has been run.

  timeSignatureSettings = #default-time-signature-settings
  timeSignatureFraction = 4/4
  autoBeaming = ##t  % needed for consistent melismata with engravers

  %% Other beaming variables are not important as autobeams don't affect
  %% the Midi.  Melismata are only affected by beams when autobeaming
  %% is switched off.

  \consists "Timing_translator"

  \defaultchild "Staff"

  dynamicAbsoluteVolumeFunction = #default-dynamic-absolute-volume
  instrumentEqualizer = #default-instrument-equalizer
  drumPitchTable = #(alist->hash-table midiDrumPitches)
  timing = ##t
}


\context {
  \type "Performer_group"
  \consists "Staff_performer" % Performer_group ?
  \consists "Lyric_performer"
  \name Lyrics
}

\context{
  \type "Performer_group"
  \name ChoirStaff
  \accepts Staff
  \accepts DrumStaff
  \defaultchild Staff
}

\context {
  \type "Performer_group"
  \consists "Staff_performer"
  \accepts ChordNameVoice
  \defaultchild ChordNameVoice
  \name ChordNames
}

\context {
  \Voice
  \name ChordNameVoice
}

\context {
  \type "Performer_group"
  \name StaffGroup
  \accepts Staff
  \accepts DrumStaff
  \accepts TabStaff
  \accepts RhythmicStaff
  \accepts GrandStaff
  \accepts PianoStaff
  \accepts Lyrics
  \accepts ChordNames
  \accepts FiguredBass
  \accepts FretBoards
  \defaultchild Staff
}

\context {
  \Staff
  \name RhythmicStaff
}

\context {
  \type "Performer_group"
  \name Dynamics
  \consists "Piano_pedal_performer"
}
