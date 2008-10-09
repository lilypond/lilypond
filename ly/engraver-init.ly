\version "2.10.0"

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

  \consists "Output_property_engraver"	

  \consists "Axis_group_engraver" 
  \consists "Fretboard_engraver"
  \consists "Separating_line_group_engraver"
  \consists "Font_size_engraver"
  \consists "Instrument_name_engraver"

  predefinedDiagramTable = #fretboard-table
}

\context {
  \type "Engraver_group"
  \name "Staff"
  
  \consists "Output_property_engraver"	
  \consists "Bar_engraver"
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
  \consists "Rest_collision_engraver"
  \consists "Accidental_engraver"
  \consists "Piano_pedal_engraver"
  \consists "Piano_pedal_align_engraver"
  \consists "Instrument_name_engraver"
  \consists "String_number_engraver"
  \consists "Axis_group_engraver"
  \consists "Figured_bass_engraver"
  \consists "Figured_bass_position_engraver"
  \consists "Script_row_engraver"

  localKeySignature = #'()
  createSpacing = ##t
  ignoreFiguredBassRest = ##t 
  \override VerticalAxisGroup #'minimum-Y-extent = #'(-4 . 4)
  
  %% explicitly set instrument, so we don't get 
  %% weird effects when doing instrument names for
  %% piano staves

  instrumentName = #'()
  shortInstrumentName = #'()
  
  \defaultchild "Voice"
  \accepts "Voice"
  \accepts "CueVoice"
  
  \description "Handles clefs, bar lines, keys, accidentals.  It can contain
@code{Voice} contexts."

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
  \remove "String_number_engraver"
  
  \description "Handles typesetting for percussion."

  \denies "Voice"
  \accepts "DrumVoice"
  \defaultchild "DrumVoice"

  clefGlyph = #"clefs.percussion"
  clefPosition = #0
  \override Script #'staff-padding = #0.75 
}


\context {
  \type "Engraver_group"
  \name "InnerChoirStaff"
  \consists "System_start_delimiter_engraver"
  systemStartDelimiter = #'SystemStartBracket
  vocalName = #'()
  shortVocalName = #'()

  \accepts "Staff"
  \accepts "DrumStaff"
  \accepts "RhythmicStaff"
  \accepts "GrandStaff"
  \accepts "PianoStaff"
  \accepts "Lyrics"
  \accepts "ChordNames"
  \defaultchild "Staff"
}

\context {
  \InnerChoirStaff
  \name ChoirStaff
  
  \defaultchild "Staff"
  \accepts "InnerChoirStaff"
  \accepts "InnerStaffGroup"
  \description "Identical to @code{StaffGroup} except that the
contained staves are not connected vertically."

}

\context{
  \type "Engraver_group"
  
  \override VerticalAxisGroup #'minimum-Y-extent = ##f
  localKeySignature = #'()
  createSpacing = ##t

  squashedPosition = #0
  \name RhythmicStaff
  \alias "Staff"
  
  \override BarLine #'bar-size = #4
  \override VoltaBracket #'staff-padding = #3
  \override StaffSymbol #'line-count = #1	

  \override Stem  #'neutral-direction = #UP
  \override Beam  #'neutral-direction = #UP
  
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
  
  \accepts "Voice"
  \accepts "CueVoice"
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

  localKeySignature = #'()
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
  \consists "Tweak_engraver"
  
  %% switch on to make stem directions interpolate for the
  %% center line.
  %  \consists "Melody_engraver"

  \consists "Stem_engraver"
  \consists "Beam_engraver"
  \consists "Grace_beam_engraver"
  \consists "Auto_beam_engraver"

  %% must come before Script_column_engraver.
  \consists "New_fingering_engraver"
  
  \consists "Chord_tremolo_engraver"
  \consists "Percent_repeat_engraver"
  \consists "Slash_repeat_engraver"
  \consists "Part_combine_engraver"

  \consists "Text_engraver"
  \consists "New_dynamic_engraver"
  \consists "Dynamic_align_engraver"
%  \consists "Dynamic_engraver"
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
  \consists "Grace_engraver"
  \consists "Instrument_switch_engraver"
  \consists "Skip_event_swallow_translator"
}

\context{
  \Voice
  
  \name CueVoice
  \alias Voice
  fontSize = #-4
  \override Stem #'length-fraction = #(magstep -4)
  \override Beam #'length-fraction = #(magstep -4)
  \override Beam #'thickness = #0.35
}

\context {
  \Voice
  \name DrumVoice
  \alias Voice

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

  \consists "Skip_event_swallow_translator"
}

\context{
  \type "Engraver_group"
  \name GrandStaff
  localKeySignature = #'()
  
  \description "A group of staves, with a brace on the left
side, grouping the staves together.  The bar lines of the
contained staves are connected vertically."

  \consists "Span_bar_engraver"
  \consists "Span_arpeggio_engraver"
  \consists "System_start_delimiter_engraver"
  systemStartDelimiter = #'SystemStartBrace

  \defaultchild "Staff"
  \accepts "Staff"
  \accepts "FiguredBass"
}

\context{
  \GrandStaff
  \name "PianoStaff"
  \alias "GrandStaff"

  \description "Just like @code{GrandStaff} but with support for
instrument names at the start of each system."

  \consists "Instrument_name_engraver"
  
  instrumentName = #'()
  shortInstrumentName = #'()
}

\context {
  \type "Engraver_group"
  \name InnerStaffGroup

  \consists "Span_bar_engraver"
  \consists "Span_arpeggio_engraver"
  \consists "Output_property_engraver"	
  systemStartDelimiter = #'SystemStartBracket

  \consists "System_start_delimiter_engraver"

  \defaultchild "Staff"
  \accepts "Staff"
  \accepts "RhythmicStaff"
  \accepts "DrumStaff"
  \accepts "GrandStaff"
  \accepts "PianoStaff"
  \accepts "TabStaff"	
  \accepts "Lyrics"
  \accepts "ChordNames"
}

\context {
  \InnerStaffGroup
  \name StaffGroup
  
  \description "Groups staves while adding a bracket on the left
side, grouping the staves together.  The bar lines of the contained
staves are connected vertically.  @code{StaffGroup} only consists of
a collection of staves, with a bracket in front and spanning bar lines."
  
  \accepts "InnerChoirStaff"
  \accepts "ChoirStaff"
  \accepts "InnerStaffGroup"
  \accepts "FiguredBass"
}


\context{
  \type "Engraver_group"
  \override VerticalAxisGroup #'minimum-Y-extent = #'(-0.75 . 2.0)

  \description "Corresponds to a voice with lyrics.  Handles the
printing of a single line of lyrics."
  
  \name "Lyrics"
  instrumentName = #'()
  shortInstrumentName = #'()
  
  \consists "Lyric_engraver"
  \consists "Extender_engraver"
  \consists "Hyphen_engraver"
  \consists "Stanza_number_engraver"
  \consists "Instrument_name_engraver"
  \consists "Skip_event_swallow_translator"
  \consists "Font_size_engraver"
  \consists "Hara_kiri_engraver"

  \override VerticalAxisGroup #'remove-first = ##t
  \override VerticalAxisGroup #'remove-empty = ##t
  \override VerticalAxisGroup #'keep-fixed-while-stretching = ##t
  \override SeparationItem #'padding = #0.2
  \override InstrumentName #'self-alignment-Y = ##f

  %% sync with define-grobs.scm ;
  \override InstrumentName #'font-size = #1.0

  %% make sure that barlines aren't collapsed, when
  %% Bar_engraver is there.
  \override BarLine #'bar-size = #0.1 
  
}

\context {
  \type "Engraver_group"
  \name NoteNames
  \consists "Axis_group_engraver"

  \override VerticalAxisGroup #'minimum-Y-extent = ##f

  
  \consists "Rest_swallow_translator" 
  \consists "Skip_event_swallow_translator"
  \consists "Tie_engraver"
  \consists "Note_name_engraver"
  \consists "Separating_line_group_engraver"
}

\context {
  \type "Engraver_group"
  \name ChordNames
  \description "Typesets chord names."
  
  \consists "Rest_swallow_translator" 
  \consists "Output_property_engraver"	
  \consists "Separating_line_group_engraver"
  \consists "Chord_name_engraver"
  \consists "Skip_event_swallow_translator"
  \consists "Hara_kiri_engraver"
%  \consists "Note_spacing_engraver"
  \override VerticalAxisGroup #'minimum-Y-extent = #'(0 . 2)
  \override VerticalAxisGroup #'remove-first = ##t
  \override VerticalAxisGroup #'remove-empty = ##t
}


RemoveEmptyStaffContext= \context {
  \Staff
  \remove "Axis_group_engraver"
  \consists "Hara_kiri_engraver"
  \override Beam #'auto-knee-gap = #'()
  \override VerticalAxisGroup #'remove-empty = ##t
}

AncientRemoveEmptyStaffContext = \context {
%% why not add by default?
  
  \RemoveEmptyStaffContext
  \accepts "VaticanaVoice"
  \accepts "GregorianTranscriptionVoice"
  \accepts "MensuralVoice"
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
automatically when an output definition (a @code{\score} or
@code{\layout} block) is processed."

  \consists "Paper_column_engraver"
  \consists "Vertically_spaced_contexts_engraver"
  \consists "Repeat_acknowledge_engraver"
  \consists "Staff_collecting_engraver"

  %% move the alias along with the engraver.

  \consists "Timing_translator"
  \consists "Default_bar_line_engraver"
  \consists "Output_property_engraver"
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
  
  \defaultchild "Staff"

  \accepts "FretBoards"
  \accepts "Staff"
  \accepts "RhythmicStaff"
  \accepts "TabStaff"
  \accepts "VaticanaStaff"
  \accepts "GregorianTranscriptionStaff"
  \accepts "MensuralStaff"
  \accepts "StaffGroup"
  \accepts "DrumStaff"
  \accepts "Lyrics"
  \accepts "ChordNames"
  \accepts "GrandStaff"
  \accepts "ChoirStaff"
  \accepts "PianoStaff"
  \accepts "Devnull"
  \accepts "NoteNames"
  \accepts "FiguredBass"


  noteToFretFunction = #determine-frets
  soloText = #"Solo"
  soloIIText = #"Solo II"
  aDueText = #"a2"
  printPartCombineTexts = ##t
  systemStartDelimiter =#'SystemStartBar

  drumStyleTable = #drums-style
  
  melismaBusyProperties = #default-melisma-properties
  tieWaitForNote = ##f
  clefGlyph = #"clefs.G"
  clefPosition = #-2
  middleCClefPosition = #-6
  middleCPosition = #-6
  firstClef = ##t

  crescendoSpanner = #'hairpin
  decrescendoSpanner = #'hairpin
  
  defaultBarType = #"|"
  doubleRepeatType = #":|:"
  barNumberVisibility = #first-bar-number-invisible
  automaticBars = ##t
  
  explicitClefVisibility = #all-visible
  explicitKeySignatureVisibility = #all-visible
  implicitTimeSignatureVisibility = #end-of-line-invisible
  
  autoBeamSettings = #default-auto-beam-settings
  autoBeaming = ##t
  autoBeamCheck = #default-auto-beam-check
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
  
  lyricMelismaAlignment = #LEFT
  markFormatter = #format-mark-letters
  rehearsalMark = #1
  subdivideBeams = ##f
  allowBeamBreak = ##f
  extraNatural = ##t
  autoAccidentals = #`(Staff ,(make-accidental-rule 'same-octave 0))
  autoCautionaries = #'()  

  printKeyCancellation = ##t
  keyAlterationOrder = #`(
    (6 . ,FLAT) (2  . ,FLAT) (5 . ,FLAT ) (1  . ,FLAT) (4  . ,FLAT) (0  . ,FLAT) (3  . ,FLAT)
    (3 . ,SHARP) (0 . ,SHARP) (4 . ,SHARP) (1 . ,SHARP) (5 . ,SHARP) (2 . ,SHARP) (6 . ,SHARP)
    (6 . ,DOUBLE-FLAT) (2 . ,DOUBLE-FLAT) (5 . ,DOUBLE-FLAT ) (1 . ,DOUBLE-FLAT) (4 . ,DOUBLE-FLAT) (0 . ,DOUBLE-FLAT) (3 . ,DOUBLE-FLAT)
    (3  . ,DOUBLE-SHARP) (0 . ,DOUBLE-SHARP) (4 . ,DOUBLE-SHARP) (2 . ,DOUBLE-SHARP) (5 . ,DOUBLE-SHARP) (2 . ,DOUBLE-SHARP) (6 . ,DOUBLE-SHARP)
  )

  barCheckSynchronize = ##f
  
%% chord names:
  chordNameFunction = #ignatzek-chord-names
  majorSevenSymbol = #whiteTriangleMarkup
  chordNameSeparator = #(make-simple-markup  "/")
  chordNameExceptions = #ignatzekExceptions
  chordNoteNamer = #'()
  chordRootNamer = #note-name->markup
  chordPrefixSpacer = #0
  chordNameExceptionsFull = #fullJazzExceptions
  chordNameExceptionsPartial = #partialJazzExceptions
  

  bassStaffProperties = #'((assign clefGlyph "clefs.F")
  (assign clefPosition 2)
  (assign middleCPosition 6)
  (assign middleCClefPosition 6))
%% tablature:
  stringOneTopmost = ##t
  highStringOne = ##t

%% One may change the strings tuning as following :
%% The lenght of the list must be equal to the number of string
  stringTunings = #guitar-tuning
  tablatureFormat = #fret-number-tablature-format

%%
  figuredBassFormatter = #format-bass-figure
  metronomeMarkFormatter = #format-metronome-markup


  %% See also make-voice-props-set
  graceSettings = #`(
    (Voice Stem direction ,UP)
    (Voice Stem font-size -3)
    (Voice NoteHead font-size -3)
    (Voice Dots font-size -3)
    (Voice Stem length-fraction 0.8)
    (Voice Stem no-stem-extend #t)
    (Voice Beam thickness 0.384)
    (Voice Beam length-fraction 0.8)
    (Voice Accidental font-size -4)
    (Voice AccidentalCautionary font-size -4)
    (Voice Slur direction ,DOWN)
    (Voice Script font-size -3)
  )

  keepAliveInterfaces = #'(
    rhythmic-grob-interface
    lyric-interface
    percent-repeat-item-interface
    percent-repeat-interface

    ;; need this, as stanza numbers are items, and appear only once. 
    stanza-number-interface
  )
  quotedEventTypes = #'(
    note-event
    rest-event
    tie-event
    beam-event
    tuplet-span-event)
  instrumentTransposition = #(ly:make-pitch 0 0 0)

  verticallySpacedContexts = #'(Staff)
  
  timing = ##t
}




\context {
  \type "Engraver_group"
  \name "FiguredBass"

  \consists "Figured_bass_engraver"
  \consists "Note_swallow_translator"
  \consists "Skip_event_swallow_translator"
  \consists "Separating_line_group_engraver"
  \consists "Hara_kiri_engraver"

  \override VerticalAxisGroup #'remove-empty = ##t
  \override VerticalAxisGroup #'remove-first = ##t
  \override VerticalAxisGroup #'minimum-Y-extent = #'(0 . 2)
}

\context {
  \name "Devnull"
  \type "Engraver_group"

%% don't want to route anything out of here: 
  \alias "Staff"
  \alias "Voice"
  \consists "Swallow_engraver"
  \description "Silently discards all musical information given to this
context."
}

\context {
  \Voice
  \name "TabVoice"
  \alias "Voice"
  \consists "Tab_note_heads_engraver"
  \consists "Tab_harmonic_engraver"
  
  \remove "Note_heads_engraver"
  \remove "Fingering_engraver"
  \remove "New_fingering_engraver"

  \description "Context for drawing notes in a Tab staff."

  %% TabStaff increase the staff-space, which in turn
  %% increases beam thickness and spacing; beams are
  %% too big. We have to adjust the beam settings:
  \override Beam #'thickness = #0.32
  \override Beam #'length-fraction = #0.62

  %% No accidental in tablature !
  \remove Accidental_engraver

  \override Glissando #'extra-dy = #0.75
  \override Glissando #'bound-details #'right = #`((attach-dir . ,LEFT)
						   (padding . 0.3))
  \override Glissando #'bound-details #'left = #`((attach-dir . ,RIGHT)
						   (padding . 0.3))
  \override Glissando #'extra-dy = #0.75
  \override Glissando #'gap = #0.2
}

\context {
  \Staff
  \alias "Staff"
  \name "TabStaff"
  \denies "Voice"
  \consists "Tab_staff_symbol_engraver"
  
  \description "Context for generating tablature. [DOCME]"

  \accepts "TabVoice"
  \defaultchild "TabVoice"
  
  %% 6 strings
  \override StaffSymbol #'staff-space = #1.5

  %% Don't draw stems over the tablature figures !
  \override Stem #'avoid-note-head = ##t
  
  %% No accidental in tablature !
  \remove "Accidental_engraver"
  \remove "Key_engraver"
  \remove "String_number_engraver"
  %% Special "TAB" clef
  clefGlyph = #"clefs.tab"
  clefPosition = #0
}

%% TODO: Gregorian Chant contexts should be moved to gregorian.ly,
%% but this does not work (is this a bug or intended behaviour?):
%%
%% If I try to do so, I get "error: unknown escaped string:
%% `\VaticanaStaff'" in params-init.ly.  If I also move
%% "\context { \Vaticana*Context }" from params-init.ly to the end
%% of gregorian.ly, then I get "error: parse error, unexpected
%% TRANSLATOR: \context { \VaticanaStaff }" in
%% gregorian.ly. --jr

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

  %% Set default head for notes outside of \[ \].
  \override NoteHead #'style = #'vaticana.punctum

  %% Put some space before and after divisiones.
  %% FIXME: This does not seem to show any effect.
  \override Script #'padding = #0.5

  %% There are no beams in Gregorian Chant notation.
  autoBeaming = ##f

  %% Prepare TextSpanner for \episem{Initium|Finis} use.
  %%
  %% FIXME: The line @code{\override TextSpanner #'padding = #-0.1} is
  %% required to force the articulation signs being placed vertically
  %% tightly to the correpsonding note heads.
  %%
  \override TextSpanner #'style = #'line
  \override TextSpanner #'padding = #-0.1
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
  \override BarLine #'transparent = ##t

  \override StaffSymbol #'line-count = #4
  \override StaffSymbol #'thickness = #0.6

  %% FIXME: unit on StaffSymbol's width should be \linewidth.
  %% \override StaffSymbol #'width = #60.0

  %% Choose vaticana do clef on 3rd line as default.
  clefGlyph = #"clefs.vaticana.do"
  middleCPosition = #1
  middleCClefPosition = #1
  clefPosition = #1
  clefOctavation = #0

  %% Select vaticana style font.
  \override KeySignature #'style = #'vaticana
  \override Accidental #'glyph-name-alist = #alteration-vaticana-glyph-name-alist
  \override Custos #'style = #'vaticana
  \override Custos #'neutral-position = #3
  \override Custos #'neutral-direction = #DOWN
  \override Dots #'style = #'vaticana
}

\context {
  \Voice
  \name "GregorianTranscriptionVoice"
  \alias "Voice"
  
  %% Removing ligature bracket engraver without replacing it by some
  %% other ligature engraver would cause a "Junking event: `LigatureEvent'"
  %% warning for every "\[" and "\]".  Therefore, we make the grob
  %% transparent instead.
  \override LigatureBracket #'transparent = ##t

  %% Put some space before and after divisiones.
  %% FIXME: This does not seem to show any effect.
  \override Script #'padding = #0.5

  %% There are no beams in Gregorian Chant notation.
  autoBeaming = ##f

  %% Prepare TextSpanner for \episem{Initium|Finis} use.
  %%
  %% N.B.: dash-fraction MUST be unset; otherwise, TextSpanner will
  %% always produce dashed lines, regardless of the style property.
  %%
  %% FIXME: The line @code{\override TextSpanner #'padding = #-0.1} is
  %% required to force the articulation signs being placed vertically
  %% tightly to the correpsonding note heads.
  %%
  \override TextSpanner #'dash-fraction = #'()
  \override TextSpanner #'style = #'line
  \override TextSpanner #'padding = #-0.1
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
  \override BarLine #'transparent = ##t
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
  \override NoteHead #'style = #'petrucci

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
  \override BarLine #'transparent = ##t

  \override StaffSymbol #'thickness = #0.6

  %% FIXME: unit on StaffSymbol's width should be \linewidth.
  %% \override StaffSymbol #'width = #60.0

  %% Choose petrucci g clef on 2nd line as default.
  clefGlyph = #"clefs.petrucci.g"
  middleCClefPosition = #-6
  middleCPosition = #-6
  clefPosition = #-2
  clefOctavation = #0

  %% Select mensural style font.
  \override TimeSignature #'style = #'mensural
  \override KeySignature #'style = #'mensural
  \override Accidental #'glyph-name-alist = #alteration-mensural-glyph-name-alist
  \override Custos #'style = #'mensural
  \override Custos #'neutral-position = #3
  \override Custos #'neutral-direction = #DOWN

  %% Accidentals are valid only once (same as
  %% #(set-accidental-style 'forget))
  extraNatural = ##f
  autoAccidentals = #`(Staff ,(make-accidental-rule 'same-octave -1))
  autoCautionaries = #'()  
  printKeyCancellation = ##f
}


RemoveEmptyRhythmicStaffContext= \context {
  \RhythmicStaff
  \remove "Axis_group_engraver"
  \override VerticalAxisGroup #'remove-empty = ##t
  \consists "Hara_kiri_engraver"
}
