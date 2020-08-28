%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 1996--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

\version "2.21.0"

\context {
  \name "Global"

  \accepts "Score"

  \defaultchild "Score"
  \description "Hard coded entry point for LilyPond.  Cannot be tuned."
  \grobdescriptions #all-grob-descriptions
}

\context {
  \type "Engraver_group"
  \name "FretBoards"
  \alias "Staff"
  \description "A context for displaying fret diagrams."

  \consists "Fretboard_engraver"
  \consists "Output_property_engraver"
  \consists "Axis_group_engraver"
  \consists "Separating_line_group_engraver"
  \consists "Font_size_engraver"
  \consists "Instrument_name_engraver"

  %% explicitly set instrument, so it is not inherited from the parent
  instrumentName = #'()
  shortInstrumentName = #'()

  predefinedDiagramTable = #default-fret-table
  handleNegativeFrets = #'recalculate
  restrainOpenStrings = ##f
}

\context {
  \type "Engraver_group"
  \name "Staff"

  \consists "Output_property_engraver"
  \consists "Bar_engraver"
  \consists "Pure_from_neighbor_engraver"
  %% Bar_engraver must be first so default bars aren't overwritten
  %% with empty ones.

  \consists "Font_size_engraver"
  \consists "Separating_line_group_engraver"
  \consists "Dot_column_engraver"
  \consists "Staff_collecting_engraver"

  %% perhaps move to Voice context?
  \consists "Ottava_spanner_engraver"
  \consists "Clef_engraver"
  \consists "Key_engraver"
  \consists "Time_signature_engraver"
  \consists "Ledger_line_engraver"
  \consists "Staff_symbol_engraver"
  \consists "Collision_engraver"
  \consists "Grob_pq_engraver"
  \consists "Rest_collision_engraver"
  \consists "Accidental_engraver"
  \consists "Piano_pedal_engraver"
  \consists "Piano_pedal_align_engraver"
  \consists "Instrument_name_engraver"
  \consists "Axis_group_engraver"
  \consists "Figured_bass_engraver"
  \consists "Figured_bass_position_engraver"
  \consists "Script_row_engraver"
  \consists "Cue_clef_engraver"
  \consists "Fingering_column_engraver"
  \consists "Merge_mmrest_numbers_engraver"

  localAlterations = #'()
  createSpacing = ##t
  ignoreFiguredBassRest = ##f

  %% explicitly set instrument, so we don't get
  %% weird effects when doing instrument names for
  %% piano staves
  instrumentName = #'()
  shortInstrumentName = #'()

  \defaultchild "Voice"
  \accepts "CueVoice"
  \accepts "NullVoice"
  \accepts "Voice"

  \description "Handles clefs, bar lines, keys, accidentals.  It can contain
@code{Voice} contexts."

  ottavationMarkups = #ottavation-numbers
}

\context {
  \Staff
  \type "Engraver_group"
  \name "DrumStaff"
  \alias "Staff"

  \remove "Accidental_engraver"
  \remove "Ottava_spanner_engraver"
  \remove "Key_engraver"
  \remove "Piano_pedal_engraver"

  \description "Handles typesetting for percussion."

  \denies "Voice"
  \accepts "DrumVoice"
  \defaultchild "DrumVoice"

  clefGlyph = "clefs.percussion"
  clefPosition = #0
  \override Script.staff-padding = #0.75
}


\context {
  \type "Engraver_group"
  \name "ChoirStaff"
  \consists "Vertical_align_engraver"
  topLevelAlignment = ##f
  localAlterations = #'()

  \consists "Instrument_name_engraver"
  \consists "System_start_delimiter_engraver"
  systemStartDelimiter = #'SystemStartBracket
  %% explicitly set instrument, so it is not inherited from the parent
  instrumentName = #'()
  shortInstrumentName = #'()
  vocalName = #'()
  shortVocalName = #'()

  \accepts "ChoirStaff"
  \accepts "ChordNames"
  \accepts "DrumStaff"
  \accepts "Dynamics"
  \accepts "FiguredBass"
  \accepts "GrandStaff"
  \accepts "Lyrics"
  \accepts "OneStaff"
  \accepts "PianoStaff"
  \accepts "RhythmicStaff"
  \accepts "Staff"
  \accepts "StaffGroup"
  \defaultchild "Staff"
  \description "Identical to @code{StaffGroup} except that the
contained staves are not connected vertically."
}

\context{
  \type "Engraver_group"

  localAlterations = #'()
  createSpacing = ##t

  squashedPosition = #0
  \name "RhythmicStaff"
  \alias "Staff"

  \override VoltaBracket.staff-padding = #3
  \override StaffSymbol.line-count = #1

  \override Stem.neutral-direction = #UP
  \override Beam.neutral-direction = #UP

  \consists "Output_property_engraver"
  \consists "Font_size_engraver"
  \consists "Separating_line_group_engraver"
  \consists "Dot_column_engraver"
  \consists "Bar_engraver"
  \consists "Staff_symbol_engraver"
  \consists "Pitch_squash_engraver"
  \consists "Time_signature_engraver"
  \consists "Instrument_name_engraver"
  \consists "Axis_group_engraver"
  \consists "Ledger_line_engraver"

  %% explicitly set instrument, so it is not inherited from the parent
  instrumentName = #'()
  shortInstrumentName = #'()

  \accepts "CueVoice"
  \accepts "NullVoice"
  \accepts "Voice"
  \defaultchild "Voice"

  \description "A context like @code{Staff} but for printing rhythms.
Pitches are ignored; the notes are printed on one line."
}


\context {
  \type "Engraver_group"
  \name "Voice"

  \description "Corresponds to a voice on a staff.  This context
handles the conversion of dynamic signs, stems, beams, super- and
subscripts, slurs, ties, and rests.

You have to instantiate this explicitly if you want to have
multiple voices on the same staff."
  %% Grace_engraver sets properties, it must come first.
  \consists "Grace_engraver"
  \consists "Font_size_engraver"

  \consists "Pitched_trill_engraver"
  \consists "Output_property_engraver"
  \consists "Arpeggio_engraver"
  \consists "Multi_measure_rest_engraver"
  \consists "Text_spanner_engraver"
  \consists "Trill_spanner_engraver"
  \consists "Grob_pq_engraver"
  \consists "Forbid_line_break_engraver"
  \consists "Laissez_vibrer_engraver"
  \consists "Repeat_tie_engraver"
  \consists "Note_head_line_engraver"
  \consists "Glissando_engraver"
  \consists "Ligature_bracket_engraver"
  \consists "Breathing_sign_engraver"
  \consists "Note_heads_engraver"
  \consists "Dots_engraver"
  \consists "Rest_engraver"

  %% switch on to make stem directions interpolate for the
  %% center line.
  %  \consists "Melody_engraver"

  \consists "Stem_engraver"
  \consists "Beam_engraver"
  \consists "Grace_beam_engraver"
  \consists "Auto_beam_engraver"
  \consists "Grace_auto_beam_engraver"

  %% must come before Script_column_engraver.
  \consists "New_fingering_engraver"

  \consists "Chord_tremolo_engraver"
  \consists "Double_percent_repeat_engraver"
  \consists "Percent_repeat_engraver"
  \consists "Slash_repeat_engraver"
  \consists "Part_combine_engraver"

  \consists "Text_engraver"
  \consists "Dynamic_engraver"
  \consists "Dynamic_align_engraver"
  \consists "Fingering_engraver"
  \consists "Bend_engraver"

  \consists "Script_engraver"
  \consists "Script_column_engraver"
  \consists "Rhythmic_column_engraver"
  \consists "Note_spacing_engraver"
  \consists "Spanner_break_forbid_engraver"
  \consists "Phrasing_slur_engraver"
  \consists "Cluster_spanner_engraver"
  \consists "Slur_engraver"
  \consists "Tie_engraver"
  \consists "Tuplet_engraver"
  \consists "Instrument_switch_engraver"
}

\context{
  \Voice

  \name "CueVoice"
  \alias "Voice"
  fontSize = #-4
  \override NoteHead.ignore-ambitus = ##t
  \override Stem.length-fraction = #(magstep -4)
  \override Beam.length-fraction = #(magstep -4)
  \override Beam.beam-thickness = #0.35
  \override StemTremolo.beam-thickness = #0.35
}

\context {
  \Voice
  \name "DrumVoice"
  \alias "Voice"

  \description "A voice on a percussion staff."
  \remove "Arpeggio_engraver"
  \consists "Grob_pq_engraver"

  \remove "Note_head_line_engraver"
  \remove "Glissando_engraver"
  \remove "Ligature_bracket_engraver"
  \remove "Note_heads_engraver"
  \consists "Drum_notes_engraver"
  \remove "New_fingering_engraver"

  \remove "Fingering_engraver"

  \remove "Cluster_spanner_engraver"
}

\context{
  \type "Engraver_group"
  \name "GrandStaff"
  localAlterations = #'()

  \description "A group of staves, with a brace on the left
side, grouping the staves together.  The bar lines of the
contained staves are connected vertically."

  \consists "Instrument_name_engraver"
  \consists "Span_bar_engraver"
  %% The default for DynamicText.extra-spacing-width causes dynamics to
  %% be placed across span bars, so switch it off:
  \override DynamicText.extra-spacing-width = ##f
  \consists "Span_bar_stub_engraver"
  \consists "Span_arpeggio_engraver"
  \consists "System_start_delimiter_engraver"
  \consists "Vertical_align_engraver"
  systemStartDelimiter = #'SystemStartBrace
  topLevelAlignment = ##f
  %% explicitly set instrument, so it is not inherited from the parent
  instrumentName = #'()
  shortInstrumentName = #'()

  \defaultchild "Staff"
  \accepts "ChordNames"
  \accepts "DrumStaff"
  \accepts "Dynamics"
  \accepts "FiguredBass"
  \accepts "Lyrics"
  \accepts "RhythmicStaff"
  \accepts "Staff"
  \accepts "TabStaff"
}

\context{
  \GrandStaff
  \name "PianoStaff"
  \alias "GrandStaff"

  \description "Just like @code{GrandStaff}, but the staves are only removed
together, never separately."

  \consists "Vertical_align_engraver"
  \consists "Keep_alive_together_engraver"
  topLevelAlignment = ##f

  instrumentName = #'()
  shortInstrumentName = #'()
}

\context {
  \type "Engraver_group"
  \name "StaffGroup"

  \consists "Vertical_align_engraver"
  topLevelAlignment = ##f

  \consists "Instrument_name_engraver"
  \consists "Span_bar_engraver"
  %% The default for DynamicText.extra-spacing-width causes dynamics to
  %% be placed across span bars, so switch it off:
  \override DynamicText.extra-spacing-width = ##f
  \consists "Span_bar_stub_engraver"
  \consists "Span_arpeggio_engraver"
  \consists "Output_property_engraver"
  systemStartDelimiter = #'SystemStartBracket
  %% explicitly set instrument, so it is not inherited from the parent
  instrumentName = #'()
  shortInstrumentName = #'()

  \consists "System_start_delimiter_engraver"

  \defaultchild "Staff"
  \accepts "ChoirStaff"
  \accepts "ChordNames"
  \accepts "DrumStaff"
  \accepts "FiguredBass"
  \accepts "FretBoards"
  \accepts "GrandStaff"
  \accepts "Lyrics"
  \accepts "OneStaff"
  \accepts "PianoStaff"
  \accepts "RhythmicStaff"
  \accepts "Staff"
  \accepts "StaffGroup"
  \accepts "TabStaff"

  \description "Groups staves while adding a bracket on the left
side, grouping the staves together.  The bar lines of the contained
staves are connected vertically.  @code{StaffGroup} only consists of
a collection of staves, with a bracket in front and spanning bar lines."
}

\context {
  \type "Engraver_group"
  \name "OneStaff"
  \accepts "ChordNames"
  \accepts "DrumStaff"
  \accepts "Dynamics"
  \accepts "FiguredBass"
  \accepts "FretBoards"
  \accepts "GregorianTranscriptionStaff"
  \accepts "KievanStaff"
  \accepts "Lyrics"
  \accepts "MensuralStaff"
  \accepts "NoteNames"
  \accepts "PetrucciStaff"
  \accepts "RhythmicStaff"
  \accepts "Staff"
  \accepts "TabStaff"
  \accepts "VaticanaStaff"
  \defaultchild "Staff"
  \consists "Axis_group_engraver"

  \description "Provides a common axis for the contained staves,
making all of them appear in the same vertical space.  This can be
useful for typesetting staves of different types in immediate succession
or for temporarily changing the character of one staff or overlaying
it with a different one.  Often used with @code{\\stopStaff} and
@code{\\startStaff} for best results."
}

\context {
  \type "Engraver_group"
  \name "Dynamics"
  \alias "Voice"
  \consists "Output_property_engraver"
  \consists "Bar_engraver"
  \consists "Piano_pedal_engraver"
  \consists "Script_engraver"
  \consists "Dynamic_engraver"
  \consists "Dynamic_align_engraver"
  \consists "Text_engraver"
  \consists "Text_spanner_engraver"
  \consists "Font_size_engraver"
  \consists "Axis_group_engraver"

  pedalSustainStrings = #'("Ped." "*Ped." "*")
  pedalUnaCordaStrings = #'("una corda" "" "tre corde")
  \override VerticalAxisGroup.staff-affinity = #CENTER
  \override VerticalAxisGroup.nonstaff-relatedstaff-spacing =
    #'((basic-distance . 5)
       (padding . 0.5))
  \override TextScript.font-shape = #'italic
  \override DynamicLineSpanner.Y-offset = #0
  \override DynamicLineSpanner.outside-staff-priority = ##f
  \override DynamicText.outside-staff-priority = ##f
  \override Hairpin.outside-staff-priority = ##f

  \description "Holds a single line of dynamics, which will be
centered between the staves surrounding this context."
}


\context{
  \type "Engraver_group"

  \description "Corresponds to a voice with lyrics.  Handles the
printing of a single line of lyrics."

  \name "Lyrics"
  \consists "Lyric_engraver"
  \consists "Extender_engraver"
  \consists "Hyphen_engraver"
  \consists "Stanza_number_engraver"
  \consists "Instrument_name_engraver"
  \consists "Font_size_engraver"
  \consists "Axis_group_engraver"
  \consists "Pure_from_neighbor_engraver"
  searchForVoice = ##f
  %% explicitly set instrument, so it is not inherited from the parent
  instrumentName = #'()
  shortInstrumentName = #'()

  \override VerticalAxisGroup.remove-first = ##t
  \override VerticalAxisGroup.remove-empty = ##t
  \override VerticalAxisGroup.staff-affinity = #UP
  \override VerticalAxisGroup.nonstaff-relatedstaff-spacing =
    #'((basic-distance . 5.5)
       (padding . 0.5)
       (stretchability . 1))
  \override VerticalAxisGroup.nonstaff-nonstaff-spacing =
     #'((basic-distance . 0)
        (minimum-distance . 2.8)
        (padding . 0.2)
        (stretchability . 0))
  \override VerticalAxisGroup.nonstaff-unrelatedstaff-spacing.padding = #1.5
  \override InstrumentName.self-alignment-Y = ##f

  %% sync with define-grobs.scm ;
  \override InstrumentName.font-size = #1.0

  %% make sure that barlines aren't collapsed, when
  %% Bar_engraver is there.
  \override BarLine.bar-extent = #'(-0.05 . 0.05)

}

\context {
  \type "Engraver_group"
  \name "NoteNames"
  \alias Staff			% Catch Staff-level overrides like
				% \key, \transposition
  \description "A context for printing the names of notes."
  \consists "Axis_group_engraver"

  \override VerticalAxisGroup.staff-affinity = #UP
  \override VerticalAxisGroup.nonstaff-nonstaff-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 2.8)
       (padding . 0.2)
       (stretchability . 0))
  \override VerticalAxisGroup.nonstaff-relatedstaff-spacing =
    #'((basic-distance . 5.5)
       (padding . 0.5)
       (stretchability . 1))
  \override VerticalAxisGroup.nonstaff-unrelatedstaff-spacing.padding = 1.5

  \consists "Tie_engraver"
  \consists "Note_name_engraver"
  \consists "Separating_line_group_engraver"
}

\context {
  \type "Engraver_group"
  \name "ChordNames"
  \alias Staff			% Catch Staff-level overrides like
				% \key, \transposition
  \description "Typesets chord names."

  \consists "Output_property_engraver"
  \consists "Separating_line_group_engraver"
  \consists "Chord_name_engraver"
  \consists "Axis_group_engraver"
  %\consists "Note_spacing_engraver"

  \override VerticalAxisGroup.remove-first = ##t
  \override VerticalAxisGroup.remove-empty = ##t
  \override VerticalAxisGroup.staff-affinity = #DOWN
  \override VerticalAxisGroup.nonstaff-relatedstaff-spacing.padding = #0.5
  \override VerticalAxisGroup.nonstaff-nonstaff-spacing.padding = #0.5
  \override ParenthesesItem.font-size = #1.5
}

\context {
  \type "Score_engraver"
  \name "Score"

  \description "This is the top level notation context.  No
other context can contain a @code{Score} context.  This context
handles the administration of time signatures.  It also makes sure
that items such as clefs, time signatures, and key-signatures are
aligned across staves.

You cannot explicitly instantiate a @code{Score} context (since it
is not contained in any other context).  It is instantiated
automatically when an output definition (a @code{\\score} or
@code{\\layout} block) is processed."

  \consists "Paper_column_engraver"
  \consists "Repeat_acknowledge_engraver"
  \consists "Staff_collecting_engraver"

  \alias "Timing"

  %% An alias for Timing is established by the Timing_translator in
  %% whatever context it is initialized, and the timing variables are
  %% then copied from wherever Timing had been previously established.
  %% The alias at Score level provides a target for initializing
  %% Timing variables in layout definitions before any
  %% Timing_translator has been run.

  %% timing translator must come BEFORE bar number engraver
  \consists "Timing_translator"
  \consists "Default_bar_line_engraver"
  \consists "Output_property_engraver"
  \consists "Tweak_engraver"
  \consists "System_start_delimiter_engraver"
  \consists "Mark_engraver"
  \consists "Volta_engraver"
  \consists "Metronome_mark_engraver"
  \consists "Break_align_engraver"
  \consists "Spacing_engraver"
  \consists "Grace_spacing_engraver"
  \consists "Vertical_align_engraver"
  \consists "Stanza_number_align_engraver"
  \consists "Bar_number_engraver"
  \consists "Parenthesis_engraver"
  \consists "Concurrent_hairpin_engraver"
  \consists "Beam_collision_engraver"
  \consists "Footnote_engraver"

  \defaultchild "Staff"

  \accepts "ChoirStaff"
  \accepts "ChordNames"
  \accepts "Devnull"
  \accepts "DrumStaff"
  \accepts "Dynamics"
  \accepts "FiguredBass"
  \accepts "FretBoards"
  \accepts "GrandStaff"
  \accepts "GregorianTranscriptionStaff"
  \accepts "KievanStaff"
  \accepts "Lyrics"
  \accepts "MensuralStaff"
  \accepts "NoteNames"
  \accepts "OneStaff"
  \accepts "PetrucciStaff"
  \accepts "PianoStaff"
  \accepts "RhythmicStaff"
  \accepts "Staff"
  \accepts "StaffGroup"
  \accepts "TabStaff"
  \accepts "VaticanaStaff"

  noteToFretFunction = #determine-frets
  predefinedDiagramTable = ##f
  soloText = "Solo"
  soloIIText = "Solo II"
  aDueText = "a2"
  printPartCombineTexts = ##t
  partCombineTextsOnNote = ##t
  systemStartDelimiter =#'SystemStartBar

  drumStyleTable = #drums-style

  associatedVoiceType = #'Voice
  melismaBusyProperties = #default-melisma-properties
  tieWaitForNote = ##f
  clefGlyph = "clefs.G"
  clefPosition = #-2
  middleCClefPosition = #-6
  middleCPosition = #-6
  firstClef = ##t

  crescendoSpanner = #'hairpin
  decrescendoSpanner = #'hairpin

  defaultBarType = "|"
  doubleRepeatType = ":..:"
  startRepeatType = ".|:"
  endRepeatType = ":|."
  alternativeRestores = #'(measurePosition measureLength lastChord)
  barNumberVisibility = #first-bar-number-invisible-and-no-parenthesized-bar-numbers
  barNumberFormatter = #robust-bar-number-function
  clefTranspositionFormatter = #clef-transposition-markup
  cueClefTranspositionFormatter = #clef-transposition-markup
  automaticBars = ##t

  explicitClefVisibility = #all-visible
  explicitCueClefVisibility = #end-of-line-invisible
  explicitKeySignatureVisibility = #all-visible
  initialTimeSignatureVisibility = #end-of-line-invisible

  repeatCountVisibility = #all-repeat-counts-visible

  restNumberThreshold = 1

  %% Other Timing variables are derived and set by the Timing_translator
  %% at initialization time by calling the functions in
  %% scm/time-signature-settings.scm

  timeSignatureSettings = #default-time-signature-settings
  timeSignatureFraction = 4/4

  beamHalfMeasure = ##t

  autoBeaming = ##t
  autoBeamCheck = #default-auto-beam-check

  completionFactor = #unity-if-multimeasure

  scriptDefinitions = #default-script-alist

  pedalSustainStrings = #'("Ped." "*Ped." "*")
  pedalSustainStyle = #'text
  pedalUnaCordaStrings = #'("una corda" "" "tre corde")
  pedalUnaCordaStyle = #'text

  %% These are in ordinary italic font, including the *,
  %% but they are unlikely to be used,
  %% as the default pedal-style for SostenutoPedal is 'mixed':
  %% i.e.  Sost. Ped_____________________
  pedalSostenutoStrings = #'("Sost. Ped." "*Sost. Ped." "*")
  pedalSostenutoStyle = #'mixed

  harmonicAccidentals = ##t
  fingeringOrientations = #'(up down)
  stringNumberOrientations = #'(up down)
  strokeFingerOrientations = #'(right)

  extendersOverRests = ##t
  lyricMelismaAlignment = #LEFT
  markFormatter = #format-mark-letters
  rehearsalMark = #1
  subdivideBeams = ##f
  extraNatural = ##t
  autoAccidentals = #`(Staff ,(make-accidental-rule 'same-octave 0))
  autoCautionaries = #'()

  printKeyCancellation = ##t
  keyAlterationOrder = #`(
    (6 . ,FLAT) (2  . ,FLAT) (5 . ,FLAT ) (1  . ,FLAT) (4  . ,FLAT) (0  . ,FLAT) (3  . ,FLAT)
    (3 . ,SHARP) (0 . ,SHARP) (4 . ,SHARP) (1 . ,SHARP) (5 . ,SHARP) (2 . ,SHARP) (6 . ,SHARP)
    (6 . ,DOUBLE-FLAT) (2 . ,DOUBLE-FLAT) (5 . ,DOUBLE-FLAT ) (1 . ,DOUBLE-FLAT) (4 . ,DOUBLE-FLAT) (0 . ,DOUBLE-FLAT) (3 . ,DOUBLE-FLAT)
    (3  . ,DOUBLE-SHARP) (0 . ,DOUBLE-SHARP) (4 . ,DOUBLE-SHARP) (1 . ,DOUBLE-SHARP) (5 . ,DOUBLE-SHARP) (2 . ,DOUBLE-SHARP) (6 . ,DOUBLE-SHARP)
  )

  barCheckSynchronize = ##f

  %% note names:
  noteNameFunction = #note-name-markup
  noteNameSeparator = "/"
  printOctaveNames = ##f
  printAccidentalNames = ##t

  %% chord names:
  chordNameFunction = #ignatzek-chord-names
  minorChordModifier = #(make-simple-markup "m")
  additionalPitchPrefix = "" % was "add"
  majorSevenSymbol = #whiteTriangleMarkup
  chordNameLowercaseMinor = ##f
  chordNameSeparator = #(make-hspace-markup 0.5)
  slashChordSeparator = #(make-simple-markup "/")
  chordNameExceptions = #ignatzekExceptions
  chordNoteNamer = #'()
  chordRootNamer = #note-name->markup
  chordPrefixSpacer = #0
  noChordSymbol = #(make-simple-markup "N.C.")

  %% tablature:
  stringOneTopmost = ##t
  highStringOne = ##t

  %% One may change the string tunings as follows :
  %% The length of the list must be equal to the number of strings
  stringTunings = #guitar-tuning
  tablatureFormat = #fret-number-tablature-format
  tabStaffLineLayoutFunction = #tablature-position-on-lines

%%
  figuredBassFormatter = #format-bass-figure
  metronomeMarkFormatter = #format-metronome-markup

  %% See also make-voice-props-set
  graceSettings = #score-grace-settings

  keepAliveInterfaces = #'(
    bass-figure-interface
    chord-name-interface
    cluster-beacon-interface
    dynamic-interface
    fret-diagram-interface
    lyric-syllable-interface
    note-head-interface
    tab-note-head-interface
    lyric-interface
    percent-repeat-item-interface
    percent-repeat-interface

    ;; need this, as stanza numbers are items, and appear only once.
    stanza-number-interface
  )
  %% \quoteDuring is supposed to quote everything, cueDuring only the essentials
  quotedEventTypes = #'(StreamEvent)
  quotedCueEventTypes = #'(
    note-event
    rest-event
    tie-event
    beam-event
    tuplet-span-event
    tremolo-event)
  instrumentTransposition = #(ly:make-pitch 0 0 0)

  topLevelAlignment = ##t

  timing = ##t
}




\context {
  \type "Engraver_group"
  \name "FiguredBass"
  \description "A context for printing a figured bass line."

  \consists "Figured_bass_engraver"
  \consists "Separating_line_group_engraver"
  \consists "Axis_group_engraver"

  \override VerticalAxisGroup.remove-empty = ##t
  \override VerticalAxisGroup.remove-first = ##t
  \override VerticalAxisGroup.staff-affinity = #UP
  \override VerticalAxisGroup.nonstaff-relatedstaff-spacing.padding = #0.5
  \override VerticalAxisGroup.nonstaff-nonstaff-spacing.padding = #0.5
}

\context {
  \name "Devnull"
  \type "Engraver_group"

  %% don't want to route anything out of here:
  \alias "Staff"
  \alias "Voice"
  \description "Silently discards all musical information given to this
context."
}

\context {
  \name "NullVoice"
  \type "Engraver_group"
  \description "For aligning lyrics without printing notes"

  %% don't route anything out of here
  \alias "Staff"
  \alias "Voice"

  %% provide non-printing NoteHeads with proper extents for lyric alignment
  \consists "Note_heads_engraver"
  \omit NoteHead
  \override NoteHead.X-extent = #(lambda (g)
    (ly:stencil-extent (ly:note-head::print g) X))

  %% generate no accidentals
  nullAccidentals = ##t

  %% keep noteheads inside the staff
  \consists "Pitch_squash_engraver"
  squashedPosition = 0

  %% generate no ledger lines, needed for staves with custom
  %% line positions and ledger lines that appear inside the staff
  \override NoteHead.no-ledgers = ##t

  %% the engravers that control the 'busy' flags for note-onsets and melismata
  \consists "Grob_pq_engraver"
  \consists "Tie_engraver"
  \omit Tie
  \consists "Beam_engraver"
  \omit Beam
  \consists "Slur_engraver"
  \omit Slur
}

\context {
  \Voice
  \name "TabVoice"
  \alias "Voice"
  \consists "Tab_note_heads_engraver"
  \consists "Tab_tie_follow_engraver"

  \remove "Note_heads_engraver"
  \remove "Fingering_engraver"
  \remove "New_fingering_engraver"
  \remove "Pitched_trill_engraver"

  \description "Context for drawing notes in a Tab staff."

  %% No accidental in tablature !
  \remove "Accidental_engraver"
}

\context {
  \Staff
  \alias "Staff"
  \name "TabStaff"
  \denies "Voice"
  \consists "Tab_staff_symbol_engraver"

  \description "Context for generating tablature. It accepts only @code{TabVoice}
contexts and handles the line spacing, the tablature clef etc. properly."

  \accepts "TabVoice"
  \defaultchild "TabVoice"

  %% 6 strings, bigger spacing
  \override StaffSymbol.staff-space = #1.5

  %% Don't draw stems over the tablature figures !
  \override Stem.avoid-note-head = ##t

  %% No accidental in tablature !
  \remove "Accidental_engraver"
  \remove "Key_engraver"

  \remove "Ottava_spanner_engraver"
  %% the clef handler
  \override Clef.stencil = #clef::print-modern-tab-if-set
  %% no time signature
  \override TimeSignature.stencil = ##f
  %% no arpeggios
  \override Arpeggio.stencil = ##f
  %% we ignore collision warnings that may occur due to
  %% stem overlapping, because we have no stems ;-)
  \override NoteColumn.ignore-collision = ##t
  %% Special "TAB" clef
  clefGlyph = "clefs.tab"
  clefPosition = #0
  %% Change string if note results in negative fret number
  handleNegativeFrets = #'recalculate
  %% Allow open strings even if minimumFret is set
  restrainOpenStrings = ##f

  %% TabStaff increase the staff-space, which in turn
  %% increases beam thickness and spacing; beams are
  %% too big. We have to adjust the beam settings:
  \override Beam.beam-thickness = #0.32
  \override Beam.length-fraction = #0.62
  %% the same goes for tremolo beams
  \override StemTremolo.beam-thickness = #0.32
  %% NOTE: in lily/stem-tremolo.cc, we have length-fraction = 1,
  %% and the tablature staff space is scaled (1.5 by default),
  %% so we use the inversion of the scale factor:
  \override StemTremolo.length-fraction = #(lambda (grob)
                                               (/ 1 (ly:staff-symbol-staff-space grob)))
  \override StemTremolo.beam-width = #stem-tremolo::calc-tab-width

  %% make the Stems as short as possible to minimize their influence
  %% on the slur::calc-control-points routine
  \override Stem.no-stem-extend = ##t
  \override Flag.style = #'no-flag
  \override Stem.details = #'((lengths 0 0 0 0 0 0)
                                (beamed-lengths 0 0 0)
                                (beamed-minimum-free-lengths 0 0 0)
                                (beamed-extreme-minimum-free-lengths 0 0)
                                (stem-shorten 0 0))
  %% after all, the stubs of the stems may still be visible, so ...
  \override Stem.stencil = ##f
  \override Flag.stencil = ##f
  %% automatic beams should be suppressed for similar reasons ...
  autoBeaming = ##f
  %% remove beams, dots and rests ...
  \override Beam.stencil = ##f
  \override StemTremolo.stencil = ##f
  \override Dots.stencil = ##f
  \override Rest.stencil = ##f
  \override MultiMeasureRest.stencil = ##f
  \override MultiMeasureRestNumber.stencil = ##f
  \override MultiMeasureRestScript.stencil = ##f
  \override MultiMeasureRestText.stencil = ##f
  %% ... all kinds of ties/slurs
  \override Tie.stencil = ##f
  \override RepeatTie.stencil = ##f
  \override LaissezVibrerTie.stencil = ##f
  \override Slur.stencil = #slur::draw-tab-slur
  \override PhrasingSlur.stencil = ##f
  %% 'tied to' fret numbers become invisible or parenthesized, respectively)
  \override Tie.after-line-breaking = #tie::handle-tab-note-head
  \override RepeatTie.after-line-breaking = #repeat-tie::handle-tab-note-head
  %% ... and all kinds of markups, spanners etc.
  \override TupletBracket.stencil = ##f
  \override TupletNumber.stencil = ##f
  \override DynamicText.stencil = ##f
  \override DynamicTextSpanner.stencil = ##f
  \override TextSpanner.stencil = ##f
  \override Hairpin.stencil = ##f
  \override Script.stencil = ##f
  \override TextScript.stencil = ##f
  \override Glissando.stencil = #glissando::draw-tab-glissando
  %% the direction for glissando lines will be automatically corrected
  \override Glissando.extra-dy = #glissando::calc-tab-extra-dy
  \override Glissando.bound-details.right = #`((attach-dir . ,LEFT)
                                                   (padding . 0.3))
  \override Glissando.bound-details.left = #`((attach-dir . ,RIGHT)
                                                   (padding . 0.3))
  %% dead notes
  \override TabNoteHead.glyph-name = #tab-note-head::calc-glyph-name
  \override TabNoteHead.stencil = #tab-note-head::whiteout-if-style-set
}

\context {
  \Voice
  \name "VaticanaVoice"
  \alias "Voice"
  \description "Same as @code{Voice} context, except that it is
accommodated for typesetting Gregorian Chant in the notational style
of Editio Vaticana."

  \remove "Slur_engraver"
  \remove "Stem_engraver"
  \remove "Ligature_bracket_engraver"
  \consists "Vaticana_ligature_engraver"
  \remove "Text_spanner_engraver"
  \consists "Episema_engraver"

  %% Set default head for notes outside of \[ \].
  \override NoteHead.style = #'vaticana.punctum

  %% Put some space before and after divisiones.
  %% FIXME: This does not seem to show any effect.
  \override Script.padding = #0.5

  %% There are no beams in Gregorian Chant notation.
  autoBeaming = ##f
}

\context {
  \Staff
  \name "VaticanaStaff"
  \alias "Staff"
  \denies "Voice"
  \accepts "VaticanaVoice"
  \defaultchild "VaticanaVoice"

  \description "Same as @code{Staff} context, except that it is
accommodated for typesetting Gregorian Chant in the notational style
of Editio Vaticana."

  \remove "Time_signature_engraver"
  \consists "Custos_engraver"

  %% We can not remove Bar_engraver; otherwise clefs and custodes will
  %% not show up any more among other line breaking issues.
  %% Instead, we make the grob transparent.
  \override BarLine.transparent = ##t

  \override StaffSymbol.line-count = #4
  \override StaffSymbol.thickness = #0.6

  %% FIXME: unit on StaffSymbol's width should be \linewidth.
  %% \override StaffSymbol.width = #60.0

  %% Choose vaticana do clef on 3rd line as default.
  clefGlyph = "clefs.vaticana.do"
  middleCPosition = #1
  middleCClefPosition = #1
  clefPosition = #1
  clefTransposition = #0

  %% Select vaticana style font.
  \override KeySignature.glyph-name-alist = #alteration-vaticana-glyph-name-alist
  \override Accidental.glyph-name-alist = #alteration-vaticana-glyph-name-alist
  \override Custos.style = #'vaticana
  \override Custos.neutral-position = #3
  \override Custos.neutral-direction = #DOWN
  \override Dots.style = #'vaticana
}

\context {
  \Voice
  \name "GregorianTranscriptionVoice"
  \alias "Voice"
  \consists "Episema_engraver"

  %% Removing ligature bracket engraver without replacing it by some
  %% other ligature engraver would cause a "Junking event: `LigatureEvent'"
  %% warning for every "\[" and "\]".  Therefore, we make the grob
  %% transparent instead.
  \override LigatureBracket.transparent = ##t

  %% Put some space before and after divisiones.
  %% FIXME: This does not seem to show any effect.
  \override Script.padding = #0.5

  %% There are no beams in Gregorian Chant notation.
  autoBeaming = ##f
}

\context {
  \Staff
  \name "GregorianTranscriptionStaff"
  \alias "Staff"
  \denies "Voice"
  \accepts "GregorianTranscriptionVoice"
  \defaultchild "GregorianTranscriptionVoice"

  %% We can not remove Bar_engraver; otherwise clefs and custodes will
  %% not show up any more among other line breaking issues.
  %% Instead, we make the grob transparent.
  \override BarLine.transparent = ##t
}

\context {
  \Voice
  \name "MensuralVoice"
  \alias "Voice"
  \description "Same as @code{Voice} context, except that it is
accommodated for typesetting a piece in mensural style."

  \remove "Slur_engraver"
  \remove "Ligature_bracket_engraver"
  \consists "Mensural_ligature_engraver"

  %% Set default head for notes outside of \[ \].
  \override NoteHead.style = #'mensural
  \override Rest.style = #'mensural
  \override Flag.style = #'mensural

  %% There are no beams in mensural notation.
  autoBeaming = ##f
}

\context {
  \Staff
  \name "MensuralStaff"
  \alias "Staff"
  \denies "Voice"
  \defaultchild "MensuralVoice"
  \accepts "MensuralVoice"
  \description "Same as @code{Staff} context, except that it is
accommodated for typesetting a piece in mensural style."

  \consists "Custos_engraver"

  %% We can not remove Bar_engraver; otherwise clefs and custodes will
  %% not show up any more among other line breaking issues.
  %% Instead, we make the grob transparent.
  \override BarLine.transparent = ##t

  \override StaffSymbol.thickness = #0.6

  %% FIXME: unit on StaffSymbol's width should be \linewidth.
  %% \override StaffSymbol.width = #60.0

  %% Choose mensural g clef on 2nd line as default.
  clefGlyph = "clefs.mensural.g"
  middleCClefPosition = #-6
  middleCPosition = #-6
  clefPosition = #-2
  clefTransposition = #0

  %% Select mensural style font.
  \override TimeSignature.style = #'mensural
  \override KeySignature.glyph-name-alist = #alteration-mensural-glyph-name-alist
  \override Accidental.glyph-name-alist = #alteration-mensural-glyph-name-alist
  \override AccidentalSuggestion.glyph-name-alist = #alteration-mensural-glyph-name-alist
  \override Custos.style = #'mensural
  \override Custos.neutral-position = #3
  \override Custos.neutral-direction = #DOWN

  %% Accidentals are valid only once (same as
  %% \accidentalStyle forget)
  extraNatural = ##f
  autoAccidentals = #`(Staff ,(make-accidental-rule 'same-octave -1))
  autoCautionaries = #'()
  printKeyCancellation = ##f
}

\context {
  \Voice
  \name "PetrucciVoice"
  \alias "Voice"
  \description "Same as @code{Voice} context, except that it is
accommodated for typesetting a piece in Petrucci style."

  \remove "Ligature_bracket_engraver"
  \consists "Mensural_ligature_engraver"

  %% Set glyph styles.
  \override NoteHead.style = #'petrucci
  \override Rest.style = #'mensural

  %% Thickens and shortens stems.
  \override Stem.thickness = #1.7
  \override Stem.length = #5

  %% There are no beams in Petrucci notation.
  autoBeaming = ##f
}

\context {
  \Staff
  \name "PetrucciStaff"
  \alias "Staff"
  \denies "Voice"
  \defaultchild "PetrucciVoice"
  \accepts "PetrucciVoice"
  \description "Same as @code{Staff} context, except that it is
accommodated for typesetting a piece in Petrucci style."

  \consists "Custos_engraver"

  \override StaffSymbol.thickness = #1.3

  %% Choose Petrucci g clef on 2nd line as default.
  clefGlyph = "clefs.petrucci.g"
  middleCClefPosition = #-6
  middleCPosition = #-6
  clefPosition = #-2
  clefTransposition = #0

  \override Custos.style = #'mensural
  \override Custos.neutral-position = #3
  \override Custos.neutral-direction = #DOWN

  %% Accidentals are valid only once (if the following note is different)
  extraNatural = ##f
  autoAccidentals = #`(Staff ,(make-accidental-rule 'same-octave 0)
                             ,neo-modern-accidental-rule)
  autoCautionaries = #'()
  printKeyCancellation = ##f
}

\context {
  \Voice
  \name "KievanVoice"
  \alias "Voice"
  \description "Same as @code{Voice} context, except that it is
accommodated for typesetting a piece in Kievan style."

  \remove "Ligature_bracket_engraver"
  \consists "Kievan_ligature_engraver"

  %% Set glyph styles.
  \override NoteHead.style = #'kievan
  \override Stem.X-offset = #stem::kievan-offset-callback
  \override Stem.stencil = ##f
  \override Flag.stencil = ##f
  \override Rest.style = #'mensural
  \override Accidental.glyph-name-alist = #alteration-kievan-glyph-name-alist
  \override Dots.style = #'kievan
  \override Slur.stencil = ##f
  \override Stem.length = #0.0
  \override Beam.positions = #beam::get-kievan-positions
  \override Beam.quantized-positions = #beam::get-kievan-quantized-positions
  \override NoteHead.duration-log = #note-head::calc-kievan-duration-log

  %% There are beams in Kievan notation, but they are invoked manually
  autoBeaming = ##f
}

\context {
  \Staff
  \name "KievanStaff"
  \alias "Staff"
  \denies "Voice"
  \defaultchild "KievanVoice"
  \accepts "KievanVoice"
  \description "Same as @code{Staff} context, except that it is
accommodated for typesetting a piece in Kievan style."

  \remove "Time_signature_engraver"

  %% Choose Kievan tsefaut clef
  clefGlyph = "clefs.kievan.do"
  middleCClefPosition = #0
  middleCPosition = #0
  clefPosition = #0
  clefTransposition = #0

  %% Accidentals are valid only once (if the following note is different)
  extraNatural = ##f
  autoAccidentals = #`(Staff ,(make-accidental-rule 'same-octave 0)
                             ,neo-modern-accidental-rule)
  autoCautionaries = #'()
  printKeyCancellation = ##f

}

%% Keep the old definitions in here for compatibility (they erase previous
%% settings to the corresponding context!).
%% For new scores, one should simply insert the \RemoveEmptyStaves settings
%% into the desired context. That's just as easy, requires only one line more
%% (the \*Staff), but preserves previous context mods.
%% TODO: DEPRECATED_2.13.17, remove at some point in the future
RemoveEmptyStaffContext = \context {
  \Staff
  \RemoveEmptyStaves
}

AncientRemoveEmptyStaffContext = \context {
  \VaticanaStaff
  \RemoveEmptyStaves
}

RemoveEmptyDrumStaffContext = \context {
  \DrumStaff
  \RemoveEmptyStaves
}

RemoveEmptyRhythmicStaffContext = \context {
  \RhythmicStaff
  \RemoveEmptyStaves
}

RemoveEmptyTabStaffContext = \context {
  \TabStaff
  \RemoveEmptyStaves
}
