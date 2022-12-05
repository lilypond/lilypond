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

\version "2.23.12"

%% Warning: if updating context hierarchies, don't forget ly/performer-init.ly!

\context {
  \name Global

  \accepts Score
  \accepts ChordGridScore
  \accepts StandaloneRhythmScore

  \defaultchild Score
  \description "Hard coded entry point for LilyPond.  Usually not meant
to be modified directly."
  \grobdescriptions #all-grob-descriptions
}

\context {
  \type Engraver_group
  \name FretBoards
  \alias Staff
  \description "A context for displaying fret diagrams."

  \consists Fretboard_engraver
  \consists Output_property_engraver
  \consists Axis_group_engraver
  \consists Separating_line_group_engraver
  \consists Font_size_engraver
  \consists Instrument_name_engraver

  %% explicitly set instrument, so it is not inherited from the parent
  instrumentName = #'()
  shortInstrumentName = #'()

  predefinedDiagramTable = #default-fret-table
  handleNegativeFrets = #'recalculate
  restrainOpenStrings = ##f
}

\context {
  \type Engraver_group
  \name Staff

  \consists Output_property_engraver
  \consists Skip_typesetting_engraver
  \consists Bar_engraver
  \consists Caesura_engraver
  \consists Pure_from_neighbor_engraver
  %% Bar_engraver must be first so default bars aren't overwritten
  %% with empty ones.

  \consists Font_size_engraver
  \consists Separating_line_group_engraver
  \consists Dot_column_engraver
  \consists Staff_collecting_engraver

  %% perhaps move to Voice context?
  \consists Ottava_spanner_engraver
  \consists Clef_engraver
  \consists Key_engraver
  \consists Time_signature_engraver
  \consists Ledger_line_engraver
  \consists Staff_symbol_engraver
  \consists Collision_engraver
  \consists Grob_pq_engraver
  \consists Rest_collision_engraver
  \consists Accidental_engraver
  \consists Piano_pedal_engraver
  \consists Piano_pedal_align_engraver
  \consists Instrument_name_engraver
  \consists Axis_group_engraver
  \consists Figured_bass_engraver
  \consists Figured_bass_position_engraver
  \consists Script_row_engraver
  \consists Non_musical_script_column_engraver
  \consists Cue_clef_engraver
  \consists Fingering_column_engraver
  \consists Merge_mmrest_numbers_engraver
  \consists Alteration_glyph_engraver
  \consists Staff_highlight_engraver

  localAlterations = #'()
  createSpacing = ##t
  ignoreFiguredBassRest = ##f

  %% explicitly set instrument, so we don't get
  %% weird effects when doing instrument names for
  %% piano staves
  instrumentName = #'()
  shortInstrumentName = #'()

  \defaultchild Voice
  \accepts CueVoice
  \accepts NullVoice
  \accepts Voice

  \description "Handles clefs, bar lines, keys, accidentals.  It can contain
@code{Voice} contexts."

  ottavationMarkups = #ottavation-numbers
}

\context {
  \Staff
  \name InternalGregorianStaff
  \denies Voice % derived contexts will accept specific voices

  \description "An internal @code{Staff} type with settings shared by
multiple ancient notation schemes."

  \remove Caesura_engraver
  \consists Divisio_engraver

  caesuraType = #'((breath . varcomma))
  caesuraTypeTransform = #caesura-to-bar-line-or-divisio

  %% Eliminate measure bar lines and allow line breaks anywhere.
  forbidBreakBetweenBarLines = ##f
  measureBarType = #'()

  %% The chosen caesuraTypeTransform prefers BarLine over Divisio.
  %% Also create bar lines for \section, \fine, and \repeat volta.
  doubleRepeatBarType = "||"
  endRepeatBarType = "||"
  fineBarType = "||"
  sectionBarType = "||"
  startRepeatBarType = "||"
  underlyingRepeatBarType = "||"
  %% It does not seem likely that anyone will use in-staff segno, but ...
  doubleRepeatSegnoBarType = "S-||"
  endRepeatSegnoBarType = "S-||"
  fineSegnoBarType = "S-||"
  fineStartRepeatSegnoBarType = "S-||"
  segnoBarType = "S-||"
  startRepeatSegnoBarType = "S-||"

  %% Maintain these BreathingSign settings in case someone follows old
  %% documentation or examples that use \breathe rather than \caesura.
  \override BreathingSign.extra-spacing-width = #'(-1.0 . 0.0)
  \override BreathingSign.extra-spacing-height =
    #item::extra-spacing-height-including-staff

  %% Accidentals are valid only once (same as
  %% \accidentalStyle forget)
  extraNatural = ##f
  autoAccidentals = #`(Staff ,(make-accidental-rule 'same-octave -1))
  autoCautionaries = #'()
  printKeyCancellation = ##f
}

\context {
  \Staff
  \type Engraver_group
  \name DrumStaff
  \alias Staff

  \remove Accidental_engraver
  \remove Ottava_spanner_engraver
  \remove Key_engraver
  \remove Piano_pedal_engraver

  \description "Handles typesetting for percussion."

  \denies Voice
  \accepts DrumVoice
  \defaultchild DrumVoice

  clefGlyph = "clefs.percussion"
  clefPosition = #0
  \override Script.staff-padding = #0.75
}

\context{
  \type Engraver_group

  localAlterations = #'()
  createSpacing = ##t

  squashedPosition = #0
  \name RhythmicStaff
  \alias Staff

  \override VoltaBracket.staff-padding = #3
  \override StaffSymbol.line-count = #1

  \override Stem.neutral-direction = #UP
  \override Beam.neutral-direction = #UP

  \consists Output_property_engraver
  \consists Font_size_engraver
  \consists Separating_line_group_engraver
  \consists Dot_column_engraver
  \consists Bar_engraver
  \consists Caesura_engraver
  \consists Staff_symbol_engraver
  \consists Pitch_squash_engraver
  \consists Time_signature_engraver
  \consists Instrument_name_engraver
  \consists Axis_group_engraver
  \consists Ledger_line_engraver
  \consists Staff_highlight_engraver

  %% explicitly set instrument, so it is not inherited from the parent
  instrumentName = #'()
  shortInstrumentName = #'()

  \accepts CueVoice
  \accepts NullVoice
  \accepts Voice
  \defaultchild Voice

  \description "A context like @code{Staff} but for printing rhythms.
Pitches are ignored; the notes are printed on one line."
}


\context {
  \type Engraver_group
  \name Voice

  \description "Corresponds to a voice on a staff.  This context
handles the conversion of dynamic signs, stems, beams, super- and
subscripts, slurs, ties, and rests.

You have to instantiate this explicitly if you want to have
multiple voices on the same staff."
  %% Grace_engraver sets properties, it must come first.
  \consists Grace_engraver
  \consists Font_size_engraver

  \consists Pitched_trill_engraver
  \consists Output_property_engraver
  \consists Arpeggio_engraver
  \consists Multi_measure_rest_engraver
  \consists Text_spanner_engraver
  \consists Trill_spanner_engraver
  \consists Grob_pq_engraver
  \consists Forbid_line_break_engraver
  \consists Laissez_vibrer_engraver
  \consists Repeat_tie_engraver
  \consists Note_head_line_engraver
  \consists Glissando_engraver
  \consists Ligature_bracket_engraver
  \consists Breathing_sign_engraver
  \consists Note_heads_engraver
  \consists Dots_engraver
  \consists Rest_engraver

  %% switch on to make stem directions interpolate for the
  %% center line.
  %  \consists Melody_engraver

  \consists Stem_engraver
  \consists Beam_engraver
  \consists Grace_beam_engraver
  \consists Auto_beam_engraver
  \consists Grace_auto_beam_engraver

  %% must come before Script_column_engraver.
  \consists New_fingering_engraver

  \consists Chord_tremolo_engraver
  \consists Double_percent_repeat_engraver
  \consists Percent_repeat_engraver
  \consists Slash_repeat_engraver
  \consists Part_combine_engraver

  \consists Text_engraver
  \consists Dynamic_engraver
  \consists Dynamic_align_engraver
  \consists Fingering_engraver
  \consists Bend_engraver
  \consists Finger_glide_engraver

  \consists Script_engraver
  \consists Script_column_engraver
  \consists Rhythmic_column_engraver
  \consists Note_spacing_engraver
  \consists Spanner_break_forbid_engraver
  \consists Phrasing_slur_engraver
  \consists Cluster_spanner_engraver
  \consists Slur_engraver
  \consists Tie_engraver
  \consists Tuplet_engraver
  \consists Instrument_switch_engraver
}

\context{
  \Voice

  \name CueVoice
  \alias Voice
  fontSize = #-4
  \override NoteHead.ignore-ambitus = ##t
  \override Stem.length-fraction = #(magstep -4)
  \override Beam.length-fraction = #(magstep -4)
  \override Beam.beam-thickness = #0.35
  \override StemTremolo.beam-thickness = #0.35
}

\context {
  \Voice
  \name DrumVoice
  \alias Voice

  \description "A voice on a percussion staff."
  \remove Arpeggio_engraver

  \remove Note_head_line_engraver
  \remove Glissando_engraver
  \remove Ligature_bracket_engraver
  \remove Note_heads_engraver
  \consists Drum_notes_engraver
  \remove New_fingering_engraver

  \remove Fingering_engraver

  \remove Cluster_spanner_engraver
}

\context {
  \type Engraver_group
  \name StaffGroup

  \consists Vertical_align_engraver
  topLevelAlignment = ##f

  \consists Instrument_name_engraver
  \consists Span_bar_engraver
  %% The default for DynamicText.extra-spacing-width causes dynamics to
  %% be placed across span bars, so switch it off:
  \override DynamicText.extra-spacing-width = ##f
  \consists Span_bar_stub_engraver
  \consists Span_arpeggio_engraver
  \consists Output_property_engraver
  systemStartDelimiter = #'SystemStartBracket
  %% explicitly set instrument, so it is not inherited from the parent
  instrumentName = #'()
  shortInstrumentName = #'()

  localAlterations = ##f
  \consists System_start_delimiter_engraver

  \defaultchild Staff
  % Accept all kinds of contexts also accepted by Score.
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
  \accepts PetrucciStaff
  \accepts PianoStaff
  \accepts RhythmicStaff
  \accepts Staff
  \accepts StaffGroup
  \accepts TabStaff
  \accepts VaticanaLyrics
  \accepts VaticanaStaff

  localAlterations = #'()

  \description "Groups staves while adding a bracket on the left
side, grouping the staves together.  The bar lines of the contained
staves are connected vertically.  @code{StaffGroup} only consists of
a collection of staves, with a bracket in front and spanning bar lines."
}

\context{
  \StaffGroup
  \name GrandStaff

  systemStartDelimiter = #'SystemStartBrace

  localAlterations = #'()
}

\context{
  \GrandStaff
  \name PianoStaff
  \alias GrandStaff

  \description "Just like @code{GrandStaff}, but the staves are only removed
together, never separately."

  \consists Keep_alive_together_engraver
}

\context {
  \StaffGroup
  \name ChoirStaff
  \remove Span_bar_engraver

  %% StaffGroup sets this property for the sake of span
  %% bars, but ChoirStaff does not have these.
  \revert DynamicText.extra-spacing-width

  \description "Identical to @code{StaffGroup} except that the
contained staves are not connected vertically."
}

\context {
  \type Engraver_group
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
  \accepts VaticanaLyrics
  \accepts VaticanaStaff
  \defaultchild Staff
  \consists Axis_group_engraver

  \description "Provides a common axis for the contained staves,
making all of them appear in the same vertical space.  This can be
useful for typesetting staves of different types in immediate succession
or for temporarily changing the character of one staff or overlaying
it with a different one.  Often used with @code{\\stopStaff} and
@code{\\startStaff} for best results."
}

\context {
  \type Engraver_group
  \name Dynamics
  \alias Voice
  \alias Staff
  \consists Output_property_engraver
  %% The bar lines are invisible, but allow the to-barline
  %% property to work.
  \consists Bar_engraver
  \consists Piano_pedal_engraver
  \consists Script_engraver
  \consists Dynamic_engraver
  \consists Dynamic_align_engraver
  \consists Text_engraver
  \consists Text_spanner_engraver
  \consists Font_size_engraver
  \consists Axis_group_engraver

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
  \type Engraver_group

  \description "Corresponds to a voice with lyrics.  Handles the
printing of a single line of lyrics."

  \name Lyrics
  \consists Lyric_engraver
  \consists Extender_engraver
  \consists Hyphen_engraver
  \consists Stanza_number_engraver
  \consists Instrument_name_engraver
  \consists Font_size_engraver
  \consists Axis_group_engraver
  \consists Pure_from_neighbor_engraver
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

  %% The use case for LyricRepeatCount is Gregorian chant.  In other
  %% genres, a "Sing N times" instruction or a volta bracket over a
  %% singleton alternative would be clearer, so we don't expect this
  %% to be used, but we want a sane default regardless.
  lyricRepeatCountFormatter = #(make-lyric-repeat-count-formatter
                                '(lowercase roman-ij dot))
}

\context {
  \type Engraver_group
  \name NoteNames
  \alias Staff			% Catch Staff-level overrides like
				% \key, \transposition
  \description "A context for printing the names of notes."
  \consists Axis_group_engraver

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

  \consists Tie_engraver
  \consists Note_name_engraver
  \consists Separating_line_group_engraver
  \consists Alteration_glyph_engraver
}

\context {
  \type Engraver_group
  \name ChordNames
  \alias Staff			% Catch Staff-level overrides like
				% \key, \transposition
  \description "Typesets chord names."

  \consists Output_property_engraver
  \consists Separating_line_group_engraver
  \consists Current_chord_text_engraver
  \consists Chord_name_engraver
  \consists Axis_group_engraver
  %\consists Note_spacing_engraver
  \consists Alteration_glyph_engraver

  \override VerticalAxisGroup.remove-first = ##t
  \override VerticalAxisGroup.remove-empty = ##t
  \override VerticalAxisGroup.staff-affinity = #DOWN
  \override VerticalAxisGroup.nonstaff-relatedstaff-spacing.padding = #0.5
  \override VerticalAxisGroup.nonstaff-nonstaff-spacing.padding = #0.5
  \override Parentheses.font-size = #1.5
}

\context {
  \type Score_engraver
  \name Score

  \description "This is the top level notation context.  No
other context can contain a @code{Score} context.  This context
handles the administration of time signatures.  It also makes sure
that items such as clefs, time signatures, and key-signatures are
aligned across staves.

You cannot explicitly instantiate a @code{Score} context (since it
is not contained in any other context).  It is instantiated
automatically when an output definition (a @code{\\score} or
@code{\\layout} block) is processed.

@anchor{Timing}
An alias for @code{Timing} is established by the
@code{Timing_translator} in whatever context it is initialized, and
the timing variables are then copied from wherever @code{Timing}
had been previously established.  The alias at @code{Score} level
provides a target for initializing @code{Timing} variables in
layout definitions before any @code{Timing_translator} has been
run."

  \consists Mark_tracking_translator
  \consists Paper_column_engraver
  \consists Repeat_acknowledge_engraver
  \consists Staff_collecting_engraver

  \alias Timing

  %% timing translator must come BEFORE bar number engraver
  \consists Timing_translator
  \consists Output_property_engraver
  \consists Tweak_engraver
  \consists Spanner_tracking_engraver
  \consists System_start_delimiter_engraver
  \consists Mark_engraver
  \consists Text_mark_engraver
  \consists Jump_engraver
  \consists Volta_engraver
  \consists Metronome_mark_engraver
  \consists Break_align_engraver
  \consists Spacing_engraver
  \consists Grace_spacing_engraver
  \consists Vertical_align_engraver
  \consists Stanza_number_align_engraver
  \consists Bar_number_engraver
  \consists Parenthesis_engraver
  \consists Concurrent_hairpin_engraver
  \consists Beam_collision_engraver
  \consists Footnote_engraver
  \consists Centered_bar_number_align_engraver
  \consists Show_control_points_engraver

  \defaultchild Staff

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
  \accepts PetrucciStaff
  \accepts PianoStaff
  \accepts RhythmicStaff
  \accepts Staff
  \accepts StaffGroup
  \accepts TabStaff
  \accepts VaticanaLyrics
  \accepts VaticanaStaff

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

  forbidBreakBetweenBarLines = ##t % where Bar_engraver is present

  measureBarType = "|"
  sectionBarType = "||"
  fineBarType = "|."

  doubleRepeatBarType = ":..:"
  endRepeatBarType = ":|."
  startRepeatBarType = ".|:"
  %% At points of repetition or departure where there wouldn't
  %% otherwise be a bar line, print a thin double bar line (Behind
  %% Bars, p.240).
  underlyingRepeatBarType = "||"

  doubleRepeatSegnoBarType = ":|.S.|:"
  endRepeatSegnoBarType = ":|.S"
  fineSegnoBarType = "|.S"
  fineStartRepeatSegnoBarType = "|.S.|:"
  segnoBarType = "S"
  startRepeatSegnoBarType = "S.|:"

  %% TODO: Add 'timing to alternativeRestores?
  alternativeRestores = #'(measurePosition measureLength measureStartNow
                           lastChord)
  barNumberVisibility = #first-bar-number-invisible-and-no-parenthesized-bar-numbers
  barNumberFormatter = #robust-bar-number-function
  centerBarNumbers = ##f
  clefTranspositionFormatter = #clef-transposition-markup
  cueClefTranspositionFormatter = #clef-transposition-markup

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

  breathMarkDefinitions = #default-breath-alist
  breathMarkType = #'comma

  caesuraType = #'((breath . caesura))

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
  codaMarkFormatter = #format-coda-mark
  rehearsalMark = #1
  rehearsalMarkFormatter = #format-mark-letters
  segnoMarkFormatter = #format-segno-mark-considering-bar-lines
  segnoStyle = #'mark
  subdivideBeams = ##f
  extraNatural = ##t
  autoAccidentals = #`(Staff ,(make-accidental-rule 'same-octave 0))
  autoCautionaries = #'()

  dalSegnoTextFormatter = #format-dal-segno-text
  fineText = "Fine"

  printKeyCancellation = ##t
  keyAlterationOrder = #`(
    (6 . ,FLAT) (2  . ,FLAT) (5 . ,FLAT ) (1  . ,FLAT) (4  . ,FLAT) (0  . ,FLAT) (3  . ,FLAT)
    (3 . ,SHARP) (0 . ,SHARP) (4 . ,SHARP) (1 . ,SHARP) (5 . ,SHARP) (2 . ,SHARP) (6 . ,SHARP)
    (6 . ,DOUBLE-FLAT) (2 . ,DOUBLE-FLAT) (5 . ,DOUBLE-FLAT ) (1 . ,DOUBLE-FLAT) (4 . ,DOUBLE-FLAT) (0 . ,DOUBLE-FLAT) (3 . ,DOUBLE-FLAT)
    (3  . ,DOUBLE-SHARP) (0 . ,DOUBLE-SHARP) (4 . ,DOUBLE-SHARP) (1 . ,DOUBLE-SHARP) (5 . ,DOUBLE-SHARP) (2 . ,DOUBLE-SHARP) (6 . ,DOUBLE-SHARP)
  )
  alterationGlyphs = ##f

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

  figuredBassFormatter = #format-bass-figure

  figuredBassAlterationDirection = #LEFT
  figuredBassLargeNumberAlignment = #CENTER
  figuredBassPlusDirection = #LEFT
  figuredBassPlusStrokedAlist = #'(
    (2 . "figbass.twoplus")
    (4 . "figbass.fourplus")
    (5 . "figbass.fiveplus")
    (6 . "figbass.sixstroked")
    (7 . "figbass.sevenstroked")
    (9 . "figbass.ninestroked"))

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

  suspendMelodyDecisions = ##f
}

%{
Some TODOs on chord grids:

* Basically every score has to use

  \paper {
    indent = 0
    system-system-spacing.padding = 2.5 % or some similar value
  }

  We should arrange for these properties to become settable in
  context defs and give them more sensible defaults for ChordGridScore.

* At different staff sizes, not all thicknesses are properly
  scaled yet.  The current implementation of \magnifyStaff is
  also weird and buggy.  See
  https://gitlab.com/lilypond/lilypond/-/merge_requests/1294#note_907159523

* Support chordChanges property, probably printing a horizontal line
  in the square.

* A bass note could be printed differently (cf. Baudoin).

* According to Baudoin, rhythmic breaks can be notated with
  parentheses.  \parenthesize works on chord names, but there is no
  provision to parenthesize several chord names at the same time.
  See also https://lsr.di.unimi.it/LSR/Item?id=902

* Detect anticipated chords and bring them on beat so that the same
  music variable can be used for outputting a chord grid and for a
  more realistic MIDI performance.

* The repeat bar lines "[|:", ":|]" and ":|][|:" are apparently
  standard in jazz.  It would be nice to use them by default.  Sadly,
  they don't look good due to the increased height.  This likely needs
  a good way to reduce the hair-thickness for those bar lines only
  while preserving it for normal bar lines.

* If there were a property to let \repeat volta print a ".|:" bar even
  at the beginning of the piece, it could be switched on by default in
  ChordGrid, since this is standard practice for jazz.

* It's a little odd that the vertical lines separating squares are
  drawn by SystemStartBar at the beginning of the line but BarLine
  elsewhere.  The problem here is that no bar line is created by
  default at the beginning of the piece.

--JeanAS
%}

\context {
  \Score
  \name ChordGridScore
  \alias Score
  \description "Top-level context replacing @code{Score} in chord grid notation.
Compared to @code{Score}, it uses proportional notation, and has a few other
settings like removing bar numbers."
  \accepts ChordGrid
  \remove Bar_number_engraver
  \remove System_start_delimiter_engraver
  proportionalNotationDuration = #(ly:make-moment 1/4)
}

\context {
  \name ChordGrid
  \type Engraver_group
  \alias Staff
  \description "Creates chord grid notation.  This context is always part of
a @code{ChordGridScore} context."
  \consists Output_property_engraver
  \consists Current_chord_text_engraver
  \consists Grid_chord_name_engraver
  \consists Chord_square_engraver
  \consists Axis_group_engraver
  \consists Alteration_glyph_engraver
  \consists Bar_engraver
  \consists Staff_symbol_engraver
  \consists Percent_repeat_engraver
  \consists Double_percent_repeat_engraver
  \consists System_start_delimiter_engraver

  \override StaffSymbol.line-positions = #'(-13.5 13.5)
  \override SystemStartBar.thickness = 2
  \override StaffSymbol.thickness = 2
  \override BarLine.hair-thickness = 2
  \override BarLine.font-size = 3
  \override BarLine.kern = 5
}

\context {
  \type Engraver_group
  \name FiguredBass
  \description "A context for printing a figured bass line."

  \consists Figured_bass_engraver
  \consists Separating_line_group_engraver
  \consists Axis_group_engraver

  \override VerticalAxisGroup.remove-empty = ##t
  \override VerticalAxisGroup.remove-first = ##t
  \override VerticalAxisGroup.staff-affinity = #UP
  \override VerticalAxisGroup.nonstaff-relatedstaff-spacing.padding = #0.5
  \override VerticalAxisGroup.nonstaff-nonstaff-spacing.padding = #0.5
}

\context {
  \name Devnull
  \type Engraver_group

  %% don't want to route anything out of here:
  \alias Staff
  \alias Voice
  \description "Silently discards all musical information given to this
context."
}

\context {
  \name NullVoice
  \type Engraver_group
  \description "For aligning lyrics without printing notes"

  %% don't route anything out of here
  \alias Staff
  \alias Voice

  %% provide non-printing NoteHeads with proper extents for lyric alignment
  \consists Note_heads_engraver
  \omit NoteHead
  \override NoteHead.X-extent = #(lambda (g)
    (ly:stencil-extent (ly:note-head::print g) X))

  %% generate no accidentals
  nullAccidentals = ##t

  %% keep noteheads inside the staff
  \consists Pitch_squash_engraver
  squashedPosition = 0

  %% generate no ledger lines, needed for staves with custom
  %% line positions and ledger lines that appear inside the staff
  \override NoteHead.no-ledgers = ##t

  %% the engravers that control the 'busy' flags for note-onsets and melismata
  \consists Grob_pq_engraver
  \consists Tie_engraver
  \omit Tie
  \consists Beam_engraver
  \omit Beam
  \consists Slur_engraver
  \omit Slur
}

\context {
  \Voice
  \name TabVoice
  \alias Voice
  \consists Tab_note_heads_engraver
  \consists Tab_tie_follow_engraver
  \consists Bend_spanner_engraver

  \remove Note_heads_engraver
  \remove Fingering_engraver
  \remove New_fingering_engraver
  \remove Pitched_trill_engraver

  \description "Context for drawing notes in a Tab staff."

  %% No accidental in tablature !
  \remove Accidental_engraver
}

\context {
  \Staff
  \alias Staff
  \name TabStaff
  \denies Voice
  \consists Tab_staff_symbol_engraver

  \description "Context for generating tablature. It accepts only @code{TabVoice}
contexts and handles the line spacing, the tablature clef etc. properly."

  \accepts TabVoice
  \defaultchild TabVoice

  %% 6 strings, bigger spacing
  \override StaffSymbol.staff-space = #1.5

  %% Don't draw stems over the tablature figures !
  \override Stem.avoid-note-head = ##t

  %% No accidental in tablature !
  \remove Accidental_engraver
  \remove Key_engraver

  \remove Ottava_spanner_engraver
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
  \override Slur.control-points = #slur::move-closer-to-tab-note-heads
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
  \name VaticanaVoice
  \alias Voice
  \description "Same as @code{Voice} context, except that it is
accommodated for typesetting Gregorian Chant in the notational style
of Editio Vaticana."

  \remove Slur_engraver
  \remove Stem_engraver
  \remove Ligature_bracket_engraver
  \consists Vaticana_ligature_engraver
  \remove Text_spanner_engraver
  \consists Episema_engraver

  %% Set default head for notes outside of \[ \].
  \override NoteHead.style = #'vaticana.punctum

  %% Compensate for thinner staff lines set in `VaticanaStaff`.
  \override Episema.thickness = #1.6
  %% Compensate for roundedness of the line.
  \override Episema.bound-details.left.padding = #0.05
  \override Episema.bound-details.right.padding = #0.05

  %% There are no beams in Gregorian Chant notation.
  autoBeaming = ##f
}

\context {
  \InternalGregorianStaff
  \name VaticanaStaff
  \alias Staff
  \accepts VaticanaVoice
  \defaultchild VaticanaVoice

  \description "Same as @code{Staff} context, except that it is
accommodated for typesetting Gregorian Chant in the notational style
of Editio Vaticana."

  \remove Time_signature_engraver
  \consists Custos_engraver

  \EnableGregorianDivisiones

  %% Mark bar lines a little thinner to match the divisiones.
  %% BarLine.hair-thickness = StaffSymbol.thickness * Divisio.thickness
  \override BarLine.hair-thickness = #0.6
  \override BarLine.thick-thickness = #1.8

  %% Match BreathingSign to Divisio in case someone follows old
  %% documentation or examples that use \breathe.
  \override BreathingSign.font-size = #-2
  \override BreathingSign.thickness = #1

  %% Reduce the size of caesura and virgula marks.
  \override Divisio.font-size = #-2
  %% Match the thickness of divisiones to the staff lines.
  \override Divisio.thickness = #1

  \override StaffSymbol.line-count = #4
  \override StaffSymbol.thickness = #0.6

  \override LedgerLineSpanner.length-fraction = #0.9
  \override StaffSymbol.ledger-line-thickness = #'(1 . 0)

  %% FIXME: unit on StaffSymbol's width should be \linewidth.
  %% \override StaffSymbol.width = #60.0

  %% Choose vaticana do clef on 3rd line as default.
  clefGlyph = "clefs.vaticana.do"
  middleCPosition = #1
  middleCClefPosition = #1
  clefPosition = #1
  clefTransposition = #0

  %% Select vaticana style font.
  alterationGlyphs = #alteration-vaticana-glyph-name-alist
  \override Custos.style = #'vaticana
  \override Custos.neutral-position = #3
  \override Custos.neutral-direction = #DOWN
  \override Dots.style = #'vaticana

  \override Clef.space-alist.first-note = #'(minimum-fixed-space . 1.4)
  \override LeftEdge.space-alist.clef = #'(extra-space . 0)
  \override BarLine.space-alist.custos = #'(minimum-space . 0.7)
  \override Custos.space-alist.right-edge = #'(extra-space . 0)
}

\context {
  \Lyrics
  \name VaticanaLyrics
  \alias Lyrics

  \override LyricText.font-size = #-4
  %% TODO: Implement Editio Vaticana style for aligning syllables to notes
  %%       (essentially, align with a syllable's vowel).  The GregorioTeX
  %%       documentation contains all the necessary details.

  \override LyricHyphen.stencil = #lyric-hyphen::vaticana-style
  \override LyricHyphen.font-series = #'medium
  \override LyricHyphen.font-size = #-4

  \description "Same as @code{Lyrics} context, except that it
provides a hyphenation style (a single, flush-left hyphen between
two syllables) as used in the notational style of Editio Vaticana."
}

\context {
  \Voice
  \name GregorianTranscriptionVoice
  \alias Voice
  \remove Stem_engraver
  \consists Episema_engraver

  %% Removing ligature bracket engraver without replacing it by some
  %% other ligature engraver would cause a "Junking event: `LigatureEvent'"
  %% warning for every "\[" and "\]".  Therefore, we make the grob
  %% transparent instead.
  \override LigatureBracket.transparent = ##t

  %% There are no beams in Gregorian Chant notation.
  autoBeaming = ##f
}

\context {
  \InternalGregorianStaff
  \name GregorianTranscriptionStaff
  \alias Staff
  \accepts GregorianTranscriptionVoice
  \defaultchild GregorianTranscriptionVoice

  \description "A staff for notating Gregorian chant in modern style."

  \remove Time_signature_engraver
}

\context {
  \Lyrics
  \name GregorianTranscriptionLyrics
  \alias Lyrics

  \consists Lyric_repeat_count_engraver

  %% Let the count use all the space under the finalis signs.
  \override LyricRepeatCount.parent-alignment-X = #RIGHT

  \description "A lyrics context for notating Gregorian chant in
modern style."
}

\context {
  \Voice
  \name MensuralVoice
  \alias Voice
  \description "Same as @code{Voice} context, except that it is
accommodated for typesetting a piece in mensural style."

  \remove Slur_engraver
  \remove Ligature_bracket_engraver
  \consists Mensural_ligature_engraver

  %% Set default head for notes outside of \[ \].
  \override NoteHead.style = #'mensural
  \override Rest.style = #'mensural
  \override Flag.style = #'mensural

  %% There are no beams in mensural notation.
  autoBeaming = ##f
}

\context {
  \InternalGregorianStaff
  \name MensuralStaff
  \alias Staff
  \defaultchild MensuralVoice
  \accepts MensuralVoice
  \description "Same as @code{Staff} context, except that it is
accommodated for typesetting a piece in mensural style."

  \consists Custos_engraver

  \EnableGregorianDivisiones

  %% Mark bar lines a little thinner to match the divisiones.
  %% BarLine.hair-thickness = StaffSymbol.thickness * Divisio.thickness
  \override BarLine.hair-thickness = #0.6
  \override BarLine.thick-thickness = #1.8

  %% Match BreathingSign to Divisio in case someone follows old
  %% documentation or examples that use \breathe.
  \override BreathingSign.font-size = #-2
  \override BreathingSign.thickness = #1

  %% Reduce the size of caesura and virgula marks.
  \override Divisio.font-size = #-2
  %% Match the thickness of divisiones to the staff lines.
  \override Divisio.thickness = #1

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
  alterationGlyphs = #alteration-mensural-glyph-name-alist
  \override Custos.style = #'mensural
  \override Custos.neutral-position = #3
  \override Custos.neutral-direction = #DOWN
}

\context {
  \Voice
  \name PetrucciVoice
  \alias Voice
  \description "Same as @code{Voice} context, except that it is
accommodated for typesetting a piece in Petrucci style."

  \remove Ligature_bracket_engraver
  \consists Mensural_ligature_engraver

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
  \name PetrucciStaff
  \alias Staff
  \denies Voice
  \defaultchild PetrucciVoice
  \accepts PetrucciVoice
  \description "Same as @code{Staff} context, except that it is
accommodated for typesetting a piece in Petrucci style."

  \consists Custos_engraver
  \consists Signum_repetitionis_engraver

  %% Eliminate measure bar lines and allow line breaks anywhere.
  forbidBreakBetweenBarLines = ##f
  measureBarType = #'()

  %% Both single and double bar lines appear at section breaks in
  %% Harmonice Musices Odhecaton.  It is not clear that there is any
  %% semantic difference.  Likewise, there does not seem to be any
  %% difference between section and final double bar lines.  The line
  %% thickness does vary noticeably throughout (even between lines of
  %% the same double bar) and we use that as an excuse to preserve our
  %% modern distinctions in these defaults.
  sectionBarType = "||"
  fineBarType = "|."

  %% Harmonice Musices Odhecaton has notation for repeated sections.
  %%
  %% See "Bergerette savoyene" (f. 12v-13r) for an example of a repeat
  %% to the beginning of the piece.  As in modern notation, there is
  %% no bar line at the beginning.
  %%
  %% See the tenor part of "Amours amours amours" (f. 25v) for (maybe)
  %% a repeat back to a previous double bar line which is at the end
  %% of a line.  That is why we leave start-repeat double bar lines
  %% with the visibility of a modern double bar line rather than make
  %% them visible at the beginning of the line.
  %%
  %% See "Le corps" (f. 72v-73r) for an example of the repeat sign
  %% appearing along with the final double bar line.  That is why we
  %% have implemented the repeat sign as its own grob.
  doubleRepeatBarType = #'()
  endRepeatBarType = #'()
  startRepeatBarType = "||"
  underlyingRepeatBarType = #'()

  %% Match BarLine.hair-thickness to stems:
  %% BarLine.hair-thickness = Stem.thickness * StaffSymbol.thickness
  %% Set kern and thick-thickness relatively, based on scans.
  \override BarLine.hair-thickness = #2.21
  \override BarLine.kern = #2.9
  \override BarLine.thick-thickness = #2.9

  %% In Harmonice Musices Odhecaton, double bar lines extend about
  %% half a staff space from the top and bottom bar lines, and single
  %% bar lines seem to extend about a space.  We lack the
  %% infrastructure to do both.
  \override BarLine.bar-extent = #'(-2.5 . 2.5)
  %% Because the ends will be visible, round the corners.
  \override BarLine.rounded = ##t

  %% same settings for SignumRepetitionis (repeat sign) as for BarLine
  \override SignumRepetitionis.hair-thickness = #2.21
  \override SignumRepetitionis.kern = #2.9
  \override SignumRepetitionis.thick-thickness = #2.9
  \override SignumRepetitionis.bar-extent = #'(-2.5 . 2.5)
  \override SignumRepetitionis.rounded = ##t

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
  \name KievanVoice
  \alias Voice
  \description "Same as @code{Voice} context, except that it is
accommodated for typesetting a piece in Kievan style."

  \remove Ligature_bracket_engraver
  \consists Kievan_ligature_engraver

  %% Set glyph styles.
  \override NoteHead.style = #'kievan
  \override Stem.X-offset = #stem::kievan-offset-callback
  \override Stem.stencil = ##f
  \override Flag.stencil = ##f
  \override Rest.style = #'mensural
  alterationGlyphs = #alteration-kievan-glyph-name-alist
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
  \name KievanStaff
  \alias Staff
  \denies Voice
  \defaultchild KievanVoice
  \accepts KievanVoice
  \description "Same as @code{Staff} context, except that it is
accommodated for typesetting a piece in Kievan style."

  \remove Time_signature_engraver

  %% Sample: Обиход нотного пения употребительных церковных роспевов.
  %% http://seminaria.ru/toporiki

  %% Eliminate measure bar lines and allow line breaks anywhere.
  forbidBreakBetweenBarLines = ##f
  measureBarType = #'()

  %% A single bar line delimits phrases.  A width of about 3x the
  %% staff line makes it hard to decide whether to use a thinner thick
  %% bar or a thicker thin bar.  The decision is "." to avoid
  %% thickening other bar types that might be used editorially.
  %%
  %% Preserving articulations on \caesura is a conscious choice, but
  %% is not carefully considered.  They should possibly be stripped.
  caesuraType = #'((bar-line . "."))
  \override BarLine.thick-thickness = 3

  %% The "k" bar is consistently used as a final bar line, but is
  %% sometimes used in mid line, seemingly between sections.
  sectionBarType = "k"
  fineBarType = "k"

  %% I see no obvious notation for repeated sections, so I have used
  %% the section bar for them. [DE]
  doubleRepeatBarType = "k"
  endRepeatBarType = "k"
  startRepeatBarType = "k"
  underlyingRepeatBarType = "k"

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

\context {
  \Score
  \name StandaloneRhythmScore
  \description "A @code{Score}-level context for use by
@code{\\markup \\rhythm}."
  \alias Score
  %% We want to avoid a notion of a measure in \markup \rhythm.  For
  %% example, it can be used to notate 8 8 = \tuplet 3/2 { 4 8 },
  %% (swing indication), and this doesn't mean anything on the time
  %% signature of the score itself.  By design, beaming is manual
  %% within \rhythm.
  \cadenzaOn
  \accepts StandaloneRhythmStaff
  \defaultchild StandaloneRhythmStaff
  %% Ensure consistent horizontal spacing across \rhythm snippets,
  %% which is particularly needed for the typical "Swing" indications.
  \override SpacingSpanner.common-shortest-duration =
    #(ly:make-moment 1/10)
}

\context {
  \RhythmicStaff
  \name StandaloneRhythmStaff
  \description "A @code{Staff}-level context for use by
@code{\\markup \\rhythm}."
  \alias Staff
  \accepts StandaloneRhythmVoice
  \defaultchild StandaloneRhythmVoice
  \remove Time_signature_engraver
  \override StaffSymbol.line-count = 0
  %% The squashed position is 1, i.e. c'', so that the bottom of the
  %% note heads is exactly on the baseline.
  squashedPosition = 1
}

\context {
  \Voice
  \name StandaloneRhythmVoice
  \description "A @code{Voice}-level context for use by
@code{\\markup \\rhythm}."
  \alias Voice
  %% Force direction of stems, since they would be down by default
  %% for c''.
  \stemUp
}
