%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 1996--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

\version "2.23.3"

%%
%% setup for Request->Element conversion.
%%
\context {
  \type Performer_group
  \name Staff
  \accepts CueVoice
  \accepts NullVoice
  \accepts Voice
  \defaultchild Voice

  \consists Staff_performer
  \consists Key_performer
  \consists Midi_control_change_performer
}

\context {
  \name Global
  \accepts Score
  \defaultchild Score
  \description "Hard coded entry point for LilyPond.  Cannot be tuned."
}

\context {
  \Staff
  \name KievanStaff
  \alias Staff
  \denies Voice
  \accepts KievanVoice
  \defaultchild KievanVoice
}

\context {
  \Staff
  \name VaticanaStaff
  \alias Staff
  \denies Voice
  \accepts VaticanaVoice
  \defaultchild VaticanaVoice
}

\context {
  \Staff
  \name MensuralStaff
  \alias Staff
  \denies Voice
  \accepts MensuralVoice
  \defaultchild MensuralVoice
}

\context {
  \Staff
  \name PetrucciStaff
  \alias Staff
  \denies Voice
  \accepts PetrucciVoice
  \defaultchild PetrucciVoice
}

\context {
  \Staff
  \name GregorianTranscriptionStaff
  \alias Staff
  \denies Voice
  \accepts GregorianTranscriptionVoice
  \defaultchild GregorianTranscriptionVoice
}

\context {
  \Staff
  \name DrumStaff
  \alias Staff
  midiInstrument = "drums"
  \denies Voice
  \accepts DrumVoice
  \defaultchild DrumVoice
}

\context {
  \type Performer_group
  \name Voice
  \consists Dynamic_performer
  \consists Tie_performer
  \consists Piano_pedal_performer
  \consists Note_performer
  \consists Beam_performer
  \consists Slur_performer
}

\context {
  \Voice
  \name CueVoice
  \alias Voice
}

\context {
  \Voice
  \name VaticanaVoice
  \alias Voice
  autoBeaming = ##f  % needed for consistent melismata with engravers"
}

\context {
  \Voice
  \name KievanVoice
  \alias Voice
  autoBeaming = ##f  % needed for consistent melismata with engravers"
}

\context {
  \Voice
  \name MensuralVoice
  \alias Voice
  autoBeaming = ##f  % needed for consistent melismata with engravers
}

\context {
  \Voice
  \name PetrucciVoice
  \alias Voice
  autoBeaming = ##f  % needed for consistent melismata with engravers
}

\context {
  \Voice
  \name GregorianTranscriptionVoice
  \alias Voice
  autoBeaming = ##f  % needed for consistent melismata with engravers
}

\context {
  \Voice
  \name DrumVoice
  \alias Voice
  \remove Note_performer
  \consists Drum_note_performer
}

\context {
  \type Performer_group
  \name FiguredBass
}

\context {
  \type Performer_group
  \name FretBoards
  \alias Staff
}

\context {
  \type Performer_group
  \name StaffGroup
  \accepts ChoirStaff
  \accepts ChordNames
  \accepts Devnull
  \accepts DrumStaff
  \accepts Dynamics
  \accepts FiguredBass
  \accepts FretBoards
  \accepts GrandStaff
  \accepts GregorianTranscriptionLyrics
  \accepts GregorianTranscriptionStaff
  \accepts KievanStaff
  \accepts Lyrics
  \accepts MensuralStaff
  \accepts NoteNames
  \accepts OneStaff
  \accepts PianoStaff
  \accepts RhythmicStaff
  \accepts Staff
  \accepts StaffGroup
  \accepts TabStaff
  \accepts VaticanaStaff
  \defaultchild Staff
}

\context {
  \StaffGroup
  \name GrandStaff
}

\context {
  \GrandStaff
  \name PianoStaff
  \alias GrandStaff
}

\context{
  \StaffGroup
  \name ChoirStaff
}

\context {
  \Voice
  \name TabVoice
  \alias Voice
}

\context {
  \type Performer_group
  \name Devnull
  \alias Voice
  \alias Staff
}

\context {
  \type Performer_group
  \name NullVoice
  \alias Staff
  \alias Voice
  %% needed for melismata
  \consists Tie_performer
  \consists Beam_performer
  \consists Slur_performer
}

\context {
  \Staff
  \name TabStaff
  midiInstrument = "acoustic guitar (nylon)"
  \alias Staff
  \denies Voice
  \accepts TabVoice
  \defaultchild TabVoice
  autoBeaming = ##f  % needed for consistent melismata with engravers
}

\context {
  \type Score_performer

  \name Score

  associatedVoiceType = #'Voice
  melismaBusyProperties = #default-melisma-properties
  instrumentName = "bright acoustic"
  midiChannelMapping = #'staff

  %% quarter = 60
  tempoWholesPerMinute = #(ly:make-moment 15/1)

  \accepts ChoirStaff
  \accepts ChordNames
  \accepts Devnull
  \accepts DrumStaff
  \accepts Dynamics
  \accepts FiguredBass
  \accepts GrandStaff
  \accepts GregorianTranscriptionLyrics
  \accepts GregorianTranscriptionStaff
  \accepts KievanStaff
  \accepts Lyrics
  \accepts MensuralStaff
  \accepts NoteNames
  \accepts OneStaff
  \accepts PetrucciStaff
  \accepts PianoStaff
  \accepts RhythmicStaff
  \accepts FretBoards
  \accepts Staff
  \accepts StaffGroup
  \accepts TabStaff
  \accepts VaticanaStaff

  \consists Mark_tracking_translator
  \consists Time_signature_performer
  \consists Control_track_performer
  \consists Mark_performer
  \consists Tempo_performer

  \alias Timing

  %% An alias for Timing is established by the Timing_translator in
  %% whatever context it is initialized, and the timing variables are
  %% then copied from wherever Timing had been previously established.
  %% The alias at Score level provides a target for initializing
  %% Timing variables in layout definitions before any
  %% Timing_translator has been run.

  timeSignatureSettings = #default-time-signature-settings
  timeSignatureFraction = 4/4
  autoBeaming = ##t  % needed for consistent melismata with engravers
  codaMarkFormatter = #format-coda-mark
  rehearsalMark = #1
  rehearsalMarkFormatter = #format-mark-letters
  segnoMarkFormatter = #format-segno-mark

  %% It is not unusual for bar number checks to be wrong in MIDI
  %% scores, for example when repeats are unfolded.
  ignoreBarNumberChecks = ##t

  %% Other beaming variables are not important as autobeams don't affect
  %% the Midi.  Melismata are only affected by beams when autobeaming
  %% is switched off.

  \consists Timing_translator

  \defaultchild Staff

  dynamicAbsoluteVolumeFunction = #default-dynamic-absolute-volume
  instrumentEqualizer = #default-instrument-equalizer
  drumPitchTable = #(alist->hash-table midiDrumPitches)

  %% \quoteDuring is supposed to quote everything but we don't admit
  %% cue events by default in order not to get multiple midi
  %% renditions in an orchestral score.

  quotedEventTypes = #'(StreamEvent)
  quotedCueEventTypes = #'()

  timing = ##t
}


\context {
  \type Performer_group
  \consists Staff_performer % Performer_group ?
  \consists Lyric_performer
  \name Lyrics
}

\context {
  \Lyrics
  \name GregorianTranscriptionLyrics
  \alias Lyrics
}

\context {
  \type Performer_group
  \consists Staff_performer
  \name NoteNames
  \alias Staff			% Catch Staff-level overrides like
				% \key, \transposition
}

\context {
  \Voice			% We want all the actual performers
  \name ChordNames
  \alias Staff			% Catch Staff-level overrides like
				% \key, \transposition
  \consists Staff_performer
}

\context {
  \type Performer_group
  \name OneStaff
  \accepts ChordNames
  \accepts DrumStaff
  \accepts Dynamics
  \accepts FiguredBass
  \accepts FretBoards
  \accepts GregorianTranscriptionLyrics
  \accepts GregorianTranscriptionStaff
  \accepts KievanStaff
  \accepts Lyrics
  \accepts MensuralStaff
  \accepts NoteNames
  \accepts PetrucciStaff
  \accepts RhythmicStaff
  \accepts Staff
  \accepts TabStaff
  \accepts VaticanaStaff
  \defaultchild Staff
}

\context {
  \Staff
  \name RhythmicStaff
  \alias Staff
  \defaultchild Voice
}

\context {
  \type Performer_group
  \name Dynamics
  \alias Voice
  \alias Staff
  \consists Piano_pedal_performer
}
