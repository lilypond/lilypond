\version "2.1.36"

\context {
    \name Global

    \accepts Score
    \description "Hard coded entry point for LilyPond. Cannot be tuned."
    \grobdescriptions #all-grob-descriptions    
}

%
% setup for Request->Element conversion. Guru-only
%

\context {
	\type "Engraver_group_engraver"
	\name Staff

	
	\consists "Output_property_engraver"	
	
	\consists "Bar_engraver"
% Bar_engraver must be first so default bars aren't overwritten
% with empty ones.
	
	\consists "Font_size_engraver"
	
	\consists "Volta_engraver"
	\consists "Separating_line_group_engraver"	
	\consists "Dot_column_engraver"

	% perhaps move to Voice context?
	\consists "Ottava_spanner_engraver"
	\consists "Clef_engraver"
	\consists "Key_engraver"
	\consists "Time_signature_engraver"
	\consists "Staff_symbol_engraver"
	\consists "Collision_engraver"
	\consists "Rest_collision_engraver"
	\consists "Accidental_engraver"
	\consists "Piano_pedal_engraver"
	\consists "Instrument_name_engraver"
	\consists "String_number_engraver"
	\consistsend "Axis_group_engraver"

	minimumVerticalExtent = #'(-6 . 6)
	extraVerticalExtent = ##f
	verticalExtent = ##f 
	localKeySignature = #'()
	createSpacing = ##t
	% explicitly set instrument, so we don't get 
	% weird effects when doing instrument names for
	% piano staves

	instrument = #'()
	instr = #'()
	  
	\accepts "Voice"
	\description "Handles clefs, bar lines, keys, accidentals.  It can contain
@code{Voice} contexts."

}

\context {
    \StaffContext
    \type "Engraver_group_engraver"
    \name DrumStaff
    \alias Staff

    \remove "Accidental_engraver"
    \remove "Ottava_spanner_engraver"
    \remove "Key_engraver" 
    \remove "Piano_pedal_engraver"
    \remove "String_number_engraver"
    
    \description "Handles typesetting for percussion."

    \denies Voice
    \accepts DrumVoice

    clefGlyph = #"clefs-percussion"
    clefPosition = #0
    \override Script #'staff-padding = #0.75 
}


\context {
    \type "Engraver_group_engraver"
    \name InnerChoirStaff
    \consists "System_start_delimiter_engraver"
    systemStartDelimiter = #'SystemStartBracket
    localKeySignature = #'()

    \accepts "Staff"
    \accepts "DrumStaff"
    \accepts "RhythmicStaff"
    \accepts "GrandStaff"
    \accepts "PianoStaff"
    \accepts "Lyrics"
    \accepts "ChordNames"
}

\context {
	\InnerChoirStaffContext
	\name ChoirStaff
	
	\accepts "InnerChoirStaff"
	\accepts "InnerStaffGroup"
	\description "Identical to @code{StaffGroup} except that the
    contained staves are not connected vertically."
	
}


\context{
    \type "Engraver_group_engraver"
    
    \consists "Output_property_engraver"	

    minimumVerticalExtent = ##f
    extraVerticalExtent = ##f
    verticalExtent = ##f 
    localKeySignature = #'()

    \consists "Pitch_squash_engraver"
    squashedPosition = #0
    \consists "Separating_line_group_engraver"	
    \name RhythmicStaff
    \alias "Staff"
    
    \override BarLine #'bar-size = #4
    \override VoltaBracket #'minimum-space = #15
    \override VoltaBracket #'padding = #5
    \override StaffSymbol #'line-count = #1	

    \override Stem  #'neutral-direction = #1
    \override Beam  #'neutral-direction = #1 	
				%	\consists "Repeat_engraver"
    \consists "Dot_column_engraver"
    \consists "Volta_engraver"
    \consists "Bar_engraver"
    \consists "Time_signature_engraver"
    \consists "Staff_symbol_engraver"
    \consists "Instrument_name_engraver"
    \consistsend "Axis_group_engraver"
    \accepts "Voice"
    \description  "
    A context like @code{Staff} but for printing rhythms.  Pitches are
    ignored; the notes are printed on one line.  
"
}


\context {
    \type "Engraver_group_engraver"
    \name Voice

    \description "
    Corresponds to a voice on a staff.  This context handles the
    conversion of dynamic signs, stems, beams, super- and subscripts,
    slurs, ties, and rests.

    You have to instantiate this explicitly if you want to have
    multiple voices on the same staff."

    localKeySignature = #'()
    \consists "Font_size_engraver"
    
    \consists "Output_property_engraver"	
    \consists "Arpeggio_engraver"
    \consists "Multi_measure_rest_engraver"
    \consists "Text_spanner_engraver"
    \consists "Grob_pq_engraver"
    \consists "Forbid_line_break_engraver"

    \consists "Note_head_line_engraver"
    \consists "Glissando_engraver"
    \consists "Ligature_bracket_engraver"
    \consists "Breathing_sign_engraver"
				% \consists "Rest_engraver"
    \consists "Note_heads_engraver"
    \consists "Rest_engraver"

    \consists "Stem_engraver"
    \consists "Beam_engraver"
    \consists "Grace_beam_engraver"
    \consists "Auto_beam_engraver"
    \consists "New_fingering_engraver"
    \consists "Chord_tremolo_engraver"
    \consists "Percent_repeat_engraver"
    \consists "Slash_repeat_engraver"
    \consists "Melisma_translator"
    \consists "Part_combine_engraver"

%{
    Must come before text_engraver, but after note_column engraver.

%}
    \consists "Text_engraver"
    \consists "Dynamic_engraver"
    \consists "Fingering_engraver"

    \consists "Script_engraver"
    \consists "Script_column_engraver"
    \consists "Rhythmic_column_engraver"
    \consists "Phrasing_slur_engraver"
    \consists "Cluster_spanner_engraver"
    \consists "Slur_engraver"
    \consists "Tie_engraver"
    \consists "Tuplet_engraver"

    \consists "Skip_event_swallow_translator"
}

\context {
    \VoiceContext
    \name DrumVoice
    \alias Voice

    \description "A voice on a percussion staff."
    \remove "Arpeggio_engraver"
    \consists "Multi_measure_rest_engraver"
    \consists "Text_spanner_engraver"
    \consists "Grob_pq_engraver"

    \remove "Note_head_line_engraver"
    \remove "Glissando_engraver"
    \remove "Ligature_bracket_engraver"
    \remove "Note_heads_engraver"
    \consists "Rest_engraver"
    \consists "Drum_notes_engraver"
    \remove "New_fingering_engraver"

    \remove "Fingering_engraver"

    \remove "Cluster_spanner_engraver"
    \consists "Tuplet_engraver"

    \consists "Skip_event_swallow_translator"
}

\context{
    \type "Engraver_group_engraver"
    \name GrandStaff
    localKeySignature = #'()
    
    \description " A group of staffs, with a brace on the left
    side, grouping the staves together.  The bar lines of the
    contained staves are connected vertically.  "

    \consists "Span_bar_engraver"
    \consists "Span_arpeggio_engraver"
    \consists "System_start_delimiter_engraver"
    systemStartDelimiter = #'SystemStartBrace

    \accepts "Staff"
}

\context{
    \GrandStaffContext
    \name "PianoStaff"
    \alias "GrandStaff"

    \description
    "Just like @code{GrandStaff} but with a forced distance between
    the staves, so cross staff beaming and slurring can be used."
    
    verticalAlignmentChildCallback = #Align_interface::fixed_distance_alignment_callback
    \override VerticalAlignment #'forced-distance = #12
    \override VerticalAlignment #'self-alignment-Y = #0

    \consists "Vertical_align_engraver"
    \consists "Instrument_name_engraver"
    
    instrument = #'()
    instr = #'()
}

\context {
    \type "Engraver_group_engraver"
    \name InnerStaffGroup
    localKeySignature = #'()

    \consists "Span_bar_engraver"
    \consists "Span_arpeggio_engraver"
    \consists "Output_property_engraver"	
    systemStartDelimiter = #'SystemStartBracket

    \consists "System_start_delimiter_engraver"
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
    \InnerStaffGroupContext
    \name StaffGroup
    
    \description

    "Groups staffs while adding a bracket on the left side, grouping
the staves together.  The bar lines of the contained staves are
connected vertically.  "
    
    \accepts "InnerChoirStaff"
    \accepts "ChoirStaff"
    \accepts "InnerStaffGroup"
    \accepts "FiguredBass"
}


\context{
    \type "Engraver_group_engraver"
    \consistsend "Hara_kiri_engraver"
    minimumVerticalExtent = #'(-1.2 . 2.4)
    extraVerticalExtent = ##f
    verticalExtent = ##f

    \description " Corresponds to a voice with lyrics.  Handles the
printing of a single line of lyrics.  "
    
    \name Lyrics 
    \consists "Separating_line_group_engraver"
    \consists "Lyric_engraver"
    \consists "Extender_engraver"
    \consists "Hyphen_engraver"
    \consists "Stanza_number_engraver"
    \consists "Vocal_name_engraver"
    \consists "Skip_event_swallow_translator"
    \consists "Font_size_engraver"
    \override SeparationItem #'padding = #0.2
}

\context {
    \type "Engraver_group_engraver"
    \name NoteNames
    \consistsend "Axis_group_engraver"

    minimumVerticalExtent = ##f
    extraVerticalExtent = ##f
    verticalExtent = ##f 

    
    \consists "Rest_swallow_translator" 
    \consists "Skip_event_swallow_translator"
    \consists "Tie_engraver"
    \consists "Note_name_engraver"
    \consists "Separating_line_group_engraver"
}

\context {
    \type "Engraver_group_engraver"
    \name ChordNames
    \description "Typesets chord names."
    
    \consists "Rest_swallow_translator" 
    \consists "Output_property_engraver"	
    \consists "Separating_line_group_engraver"
    \consists "Chord_name_engraver"
    \consists "Skip_event_swallow_translator"
    \consistsend "Hara_kiri_engraver"
    
    minimumVerticalExtent = #'(0 . 2.5)
    extraVerticalExtent = ##f
    \override SeparatingGroupSpanner #'padding = #0.8
    verticalExtent = ##f 
}


RemoveEmptyStaffContext= \context {
    \StaffContext
    \remove "Axis_group_engraver"
    \consistsend "Hara_kiri_engraver"
    \override Beam #'auto-knee-gap = #'()
}

AncientRemoveEmptyStaffContext = \context {
    %% why not add by default?
    
    \RemoveEmptyStaffContext
    \accepts "VaticanaVoice"
    \accepts "GregorianTranscriptionVoice"
}

\context {
    \type Score_engraver
    \name Score
    localKeySignature = #'()

    \description "This is the top level notation context.  No
    other context can contain a @code{Score} context.  This context
    handles the administration of time signatures.  It also makes sure
    that items such as clefs, time signatures, and key-signatures are
    aligned across staves.

    You cannot explicitly instantiate a Score context (since it is
    not contained in any other context).  It is instantiated
    automatically when an output definition (a @code{\score} or
    @code{\paper} block) is processed."
    
    \consists "Repeat_acknowledge_engraver"
    \consists "Staff_collecting_engraver"

				% move the alias along with the engraver.

    \consists "Timing_engraver"
    
    \consists "Output_property_engraver"
    \consists "System_start_delimiter_engraver"
    \consists "Mark_engraver"	
    \consists "Metronome_mark_engraver"	
    \consists "Break_align_engraver"
    \consists "Spacing_engraver"
    \consists "Vertical_align_engraver"
    \consists "Stanza_number_align_engraver"
    \consists "Bar_number_engraver"
    \consists "Span_arpeggio_engraver"

    \accepts "Staff"
    \accepts "TabStaff"
    \accepts "VaticanaStaff"
    \accepts "GregorianTranscriptionStaff"
    \accepts "StaffGroup"
    \accepts "RhythmicStaff"
    \accepts "DrumStaff"
    \accepts "Lyrics"
    \accepts "ChordNames"
    \accepts "GrandStaff"
    \accepts "ChoirStaff"
    \accepts "PianoStaff"
    \accepts "Devnull"
    \accepts "NoteNames"
    \accepts "FiguredBass"

    soloText = #"Solo"
    soloIIText = #"Solo II"
    aDueText = #"a2"
    soloADue = ##t
    systemStartDelimiter =#'SystemStartBar

    drumStyleTable = #drums-style
    
    melismaBusyProperties = #default-melisma-properties
    
    clefGlyph = #"clefs-G"
    clefPosition = #-2
    middleCPosition = #-6
    
    defaultBarType = #"|"
    barNumberVisibility = #default-bar-number-visibility
    automaticBars = ##t
    
    explicitClefVisibility = #all-visible
    explicitKeySignatureVisibility = #all-visible
    autoBeamSettings = #default-auto-beam-settings
    autoBeaming = ##t
    scriptDefinitions = #default-script-alist

    verticalAlignmentChildCallback = #Align_interface::alignment_callback

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
    tupletNumberFormatFunction = #denominator-tuplet-formatter
    markFormatter = #format-mark-letters
    rehearsalMark = #1 
    subdivideBeams = ##f
    allowBeamBreak = ##f
    extraNatural = ##t
    autoAccidentals = #'(Staff (same-octave . 0))
    autoCautionaries = #'()  

    printKeyCancellation = ##t
    keyAccidentalOrder = #`(
	(6 . ,FLAT) (2  . ,FLAT) (5 . ,FLAT ) (1  . ,FLAT) (4  . ,FLAT) (0  . ,FLAT) (3  . ,FLAT)
	(3  . ,SHARP) (0 . ,SHARP) (4 . ,SHARP) (1 . ,SHARP) (5 . ,SHARP) (2 . ,SHARP) (6 . ,SHARP)
	(6 . ,DOUBLE-FLAT) (2  . ,DOUBLE-FLAT) (5 . ,DOUBLE-FLAT ) (1  . ,DOUBLE-FLAT) (4  . ,DOUBLE-FLAT) (0  . ,DOUBLE-FLAT) (3 . ,DOUBLE-FLAT)
	(3  . ,DOUBLE-SHARP) (0 . ,DOUBLE-SHARP) (4 . ,DOUBLE-SHARP) (2 . ,DOUBLE-SHARP) (5 . ,DOUBLE-SHARP) (2 . ,DOUBLE-SHARP) (6 . ,DOUBLE-SHARP)
    )

    %{

    this order is more complex, see wansek p254 and further.

    for instance, order of clef and :|: depends on function of the clef

    (clef of start-repeat) :|: (change-clef)

    is the proper order.
    
    %}
    
    barCheckSynchronize = ##f
    
    %% chord names:
    chordNameFunction = #ignatzek-chord-names
    majorSevenSymbol = #whiteTriangleMarkup
    chordNameSeparator = #(make-simple-markup  "/")
    chordNameExceptions = #ignatzekExceptions
    chordNoteNamer = #'()
    chordRootNamer = #note-name->markup
    chordNameExceptionsFull = #fullJazzExceptions
    chordNameExceptionsPartial = #partialJazzExceptions
    
    %% tablature:
    stringOneTopmost = ##t
    highStringOne = ##t

    %% One may change the strings tuning as following :
    %% The lenght of the list must be equal to the number of string
    stringTunings = #guitar-tunings
    tablatureFormat = #fret-number-tablature-format

    %%
    bassFigureFormatFunction = #format-bass-figure
    metronomeMarkFormatter = #format-metronome-markup
    graceSettings = #`#(
	(Voice Stem direction 1)
	;; TODO: should take from existing definition.
	;; c&p from define-grobs.scm
	
	(Voice Stem lengths ,(map (lambda (x) (* 0.8 x)) '(3.5 3.5 3.5 4.5 5.0)))
	(Voice Stem stem-shorten (0.4 0))
	(Voice Stem  font-size -3)
	(Voice NoteHead  font-size -3)
	(Voice Dots  font-size -3)
	(Voice Stem beamed-lengths  
	 ,(map (lambda (x) (* 0.8 x)) '(3.26)))
	(Voice Stem beamed-minimum-free-lengths  
	 ,(map (lambda (x) (* 0.8 x)) '(2.5 2.0 1.5)))
	(Voice Stem beamed-extreme-minimum-free-lengths  
	 ,(map (lambda (x) (* 0.8 x)) '(1.83 1.5)))

	(Voice Stem no-stem-extend #t)
	(Voice Beam thickness 0.384)
	(Voice Beam space-function ,(lambda (beam mult)
				     (* 0.8 (Beam::space_function
					     beam mult))))
	(Voice Accidental font-size -4)
	(Voice Slur direction -1)
    )
    
}

OrchestralScoreContext = \context {
	\ScoreContext
}

EasyNotation = \context {
	\ScoreContext
	\override NoteHead #'print-function = #Note_head::brew_ez_stencil
	\override NoteHead #'Y-extent-callback = #'()
	\override NoteHead #'X-extent-callback = #'()
}



\context {
	\type "Engraver_group_engraver"
	\name FiguredBass 
	\consists "Figured_bass_engraver"
	\consists "Rest_swallow_translator"
	\consists "Note_swallow_translator"
	\consists "Skip_event_swallow_translator"
	\consists "Separating_line_group_engraver"
	
	\consistsend "Hara_kiri_engraver"
}

\context {
    \name "Devnull"
    \type "Engraver_group_engraver"

    %% don't want to route anything out of here: 
    \alias "Staff"
    \alias "Voice"
    \consists "Swallow_engraver"
    \description "Silently discards all musical information given to this context. "
    }

\context {
      \VoiceContext
      \name "TabVoice"
      \consists "Tab_note_heads_engraver"
      \remove "Note_heads_engraver"
      \remove "Fingering_engraver"
      \remove "New_fingering_engraver"

      \description "Context for drawing notes in a Tab staff. "
      \override Slur #'font-family    = #'roman
      \override Slur #'print-function = #hammer-print-function
      \override Slur #'direction = #-1

      % Draws all stems/beams out of the staff (and not in the middle of the staff !)
      % This feature is now disabled because most of the tab does not use it.
      %\override Beam #'damping = #100000
      %\override Stem #'up-to-staff = ##t

      % No accidental in tablature !
      \remove Accidental_engraver
}

\context {
      \StaffContext
      \alias "Staff"
      \name "TabStaff"
      \denies "Voice"
      \remove "Staff_symbol_engraver"
      \consists "Tab_staff_symbol_engraver"
      
      \description "Context for generating tablature. [DOCME]"

      \accepts "TabVoice"
      
      % 6 strings
      \override StaffSymbol #'staff-space = #1.5

     % Don't draw stems over the tablature figures !
      \override Stem #'avoid-note-head = ##t
      
      % No accidental in tablature !
      \remove "Accidental_engraver"
      \remove "Key_engraver"
      \remove "String_number_engraver"
      % Special "TAB" clef
      clefGlyph = #"clefs-tab"
      clefPosition = #0
}

% TODO: Gregorian Chant contexts should be moved to gregorian-init.ly,
% but this does not work (is this a bug or intended behaviour?):
%
% If I try to do so, I get "error: unknown escaped string:
% `\VaticanaStaffContext'" in params-init.ly.  If I also move
% "\context { \Vaticana*Context }" from params-init.ly to the end
% of gregorian-init.ly, then I get "error: parse error, unexpected
% TRANSLATOR: \context { \VaticanaStaffContext }" in
% gregorian-init.ly. --jr

\context {
  \VoiceContext
  \name "VaticanaVoice"
  \alias "Voice"
  \description "Same as @code{Voice} context, except that it is accommodated for tyepsetting Gregorian Chant in the notational style of Editio Vaticana."

  \remove "Slur_engraver"
  \remove "Stem_engraver"
  \remove "Ligature_bracket_engraver"
  \consists "Vaticana_ligature_engraver"

  % Set default head for notes outside of \[ \].
  \override NoteHead #'style = #'vaticana_punctum

  % Put some space before and after divisiones.
  % FIXME: This does not seem to show any effect.
  \override Script #'padding = #0.5

  % There are no beams in Gregorian Chant notation.
  autobeaming = ##f

  % Prepare TextSpanner for \episem{Initium|Finis} use.
  \override TextSpanner #'style = #'line
  \override TextSpanner #'edge-height = #'(0 . 0)
  \override TextSpanner #'padding = #0.5
  \override TextSpanner #'enclose-bounds = #1
  \override TextSpanner #'edge-text = #'("" . "")
}

\context {
  \StaffContext
  \name "VaticanaStaff"
  \alias "Staff"
  \denies "Voice"
  \accepts "VaticanaVoice"
  \description "Same as @code{Staff} context, except that it is accommodated for tyepsetting Gregorian Chant in the notational style of Editio Vaticana."

  \remove "Time_signature_engraver"
  \consists "Custos_engraver"

  % We can not remove Bar_engraver; otherwise clefs and custodes will
  % not show up any more among other line breaking issues.
  % Instead, we make the grob transparent.
  \override BarLine #'transparent = ##t

  \override StaffSymbol #'line-count = #4
  \override StaffSymbol #'thickness = #0.6

  % FIXME: unit on StaffSymbol's width should be \linewidth.
  % \override StaffSymbol #'width = #60.0

  % Choose vaticana do clef on 3rd line as default.
  clefGlyph = #"clefs-vaticana_do"
  middleCPosition = #1
  clefPosition = #1
  clefOctavation = #0

  % Select vaticana style font.
  \override KeySignature #'style = #'vaticana
  \override Accidental #'style = #'vaticana
  \override Custos #'style = #'vaticana
  \override Custos #'neutral-position = #3
  \override Custos #'neutral-direction = #-1

  % Score.timing = ##f
  % Score.barAlways = ##t
}

\context {
  \VoiceContext
  \name "GregorianTranscriptionVoice"
  \alias "Voice"

  % Removing ligature bracket engraver without replacing it by some
  % other ligature engraver would cause a "Junking event: `LigatureEvent'"
  % warning for every "\[" and "\]".  Therefore, we make the grob
  % transparent instead.
  \override LigatureBracket #'transparent = ##t

  % Put some space before and after divisiones.
  % FIXME: This does not seem to show any effect.
  \override Script #'padding = #0.5

  % There are no beams in Gregorian Chant notation.
  autobeaming = ##f

  % Prepare TextSpanner for \episem{Initium|Finis} use.
  \override TextSpanner #'style = #'line
  \override TextSpanner #'edge-height = #'(0 . 0)
  \override TextSpanner #'padding = #0.5
  \override TextSpanner #'enclose-bounds = #1
  \override TextSpanner #'edge-text = #'("" . "")
}
 \context {
  \StaffContext
  \name "GregorianTranscriptionStaff"
  \alias "Staff"
  \denies "Voice"
  \accepts "GregorianTranscriptionVoice"

  % We can not remove Bar_engraver; otherwise clefs and custodes will
  % not show up any more among other line breaking issues.
  % Instead, we make the grob transparent.
  \override BarLine #'transparent = ##t
}
