%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 1996--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

\version "2.12.0"

%%
%% setup for Request->Element conversion.
%%
\context {
  \type "Performer_group"
  \name Staff
  \accepts Voice
  \accepts CueVoice
  \defaultchild Voice

  \consists "Staff_performer"
  \consists "Key_performer"
}
\context {
  \name Global
  \accepts Score
  \description "Hard coded entry point for LilyPond.  Cannot be tuned."
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
  \Voice
  \name VaticanaVoice
  \alias Voice
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
  \consists "Swallow_performer"
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
  \consists "Swallow_performer"
}
\context {
  \Staff
  \name TabStaff
  midiInstrument = #"acoustic guitar (nylon)"
  \alias Staff
  \accepts TabVoice
  \defaultchild TabVoice
}

\context {
  \type "Performer_group"
  \name "VaticanaStaff"
  \alias "Staff"
  \denies "Voice"
  \accepts "VaticanaVoice"
  \defaultchild "VaticanaVoice"
}

\context {
  \type "Score_performer"

  \name Score

  melismaBusyProperties = #default-melisma-properties
  instrumentName = #"bright acoustic"

  %% quarter = 60
  tempoWholesPerMinute = #(ly:make-moment 15 1)

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
  \accepts VaticanaStaff

  \consists "Time_signature_performer"
  \consists "Control_track_performer"
  \consists "Tempo_performer"
  \consists "Timing_translator"
  \consists "Swallow_performer"

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
  \name ChordNames
}

\context {
  \type "Performer_group"
  \consists "Note_performer"
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
