\version "1.9.8"


%
% setup for Request->Element conversion. Guru-only
%

\translator {
	\type "Engraver_group_engraver"
	\name Staff

	\description "Handles clefs, bar lines, keys, accidentals.  It can contain
@code{Voice} contexts."

	
	\consists "Output_property_engraver"	
	
	\consists "Bar_engraver"
% Bar_engraver must be first so default bars aren't overwritten
% with empty ones.
	\consists "Font_size_engraver"

%	\consists "Repeat_engraver"
	\consists "Volta_engraver"
	\consists "Separating_line_group_engraver"	
	SeparatingGroupSpanner \override #'spacing-procedure
	  =  #Separating_group_spanner::set_spacing_rods_and_seqs
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
	\consists "Grob_pq_engraver"
	\consists "Forbid_line_break_engraver"
	\consists "String_number_engraver"
	\consistsend "Axis_group_engraver"

	minimumVerticalExtent = #'(-6 . 6)
	extraVerticalExtent = ##f
	verticalExtent = ##f 
	localKeySignature = #'()

	% explicitly set instrument, so we don't get 
	% weird effects when doing instrument names for
	% piano staves

	instrument = #'()
	instr = #'()
	  
	\accepts "Voice"
}


\translator {
	\type Engraver_group_engraver
	\consists "Axis_group_engraver"
	minimumVerticalExtent = ##f
	extraVerticalExtent = ##f
	verticalExtent = ##f 
	localKeySignature = #'()

	\accepts Staff
	\name StaffContainer
}

\translator {
	\type "Engraver_group_engraver"
	\name InnerChoirStaff
	\consists "System_start_delimiter_engraver"
	systemStartDelimiter = #'SystemStartBracket
	localKeySignature = #'()

	\accepts "Staff"
	\accepts "RhythmicStaff"
	\accepts "GrandStaff"
	\accepts "PianoStaff"
	\accepts "Lyrics"
	\accepts "ChordNames"
}

\translator {
	\InnerChoirStaffContext
	\name ChoirStaff
	
	\description "Identical to @code{StaffGroup} except that the
    contained staves are not connected vertically."
	
	\accepts "InnerChoirStaff"
	\accepts "InnerStaffGroup"
}


\translator{
	\type "Engraver_group_engraver"
	
	\consists "Output_property_engraver"	

\description  "
    A context like @code{Staff} but for printing rhythms.  Pitches are
    ignored; the notes are printed on one line.  
"
	minimumVerticalExtent = ##f
	extraVerticalExtent = ##f
	verticalExtent = ##f 
	localKeySignature = #'()

	\consists "Pitch_squash_engraver"
	\consists "Separating_line_group_engraver"	
	\name RhythmicStaff
	\alias "Staff"
	
	BarLine \override #'bar-size = #4
	VoltaBracket \override #'minimum-space =  #15
	VoltaBracket \override #'padding =  #5
	StaffSymbol \override #'line-count = #1	

	Stem \override #'neutral-direction = #1
	Beam \override #'neutral-direction = #1 	
%	\consists "Repeat_engraver"
	\consists "Dot_column_engraver"
	\consists "Volta_engraver"
	\consists "Bar_engraver"
	\consists "Time_signature_engraver"
	\consists "Staff_symbol_engraver"
	\consists "Instrument_name_engraver"
	\consistsend "Axis_group_engraver"
	\accepts "Voice"
}


\translator {
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
	
	% must come before all
	\consists "Voice_devnull_engraver"
	\consists "Output_property_engraver"	
	\consists "Arpeggio_engraver"
	\consists "Multi_measure_rest_engraver"
	\consists "Text_spanner_engraver"
	\consists "Grob_pq_engraver"

	\consists "Note_head_line_engraver"
	\consists "Glissando_engraver"
	\consists "Ligature_bracket_engraver"
	\consists "Breathing_sign_engraver"
 	% \consists "Rest_engraver"
	\consists "Stem_engraver"
	\consists "Beam_engraver"
	\consists "Grace_beam_engraver"
	\consists "Auto_beam_engraver"
	\consists "New_fingering_engraver"
	\consists "Chord_tremolo_engraver"
	\consists "Percent_repeat_engraver"
	\consists "Slash_repeat_engraver"
	\consists "Melisma_engraver"

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
	\consists "A2_engraver"

	\consists "Skip_event_swallow_translator"
	\accepts Thread % bug if you leave out this!
}

\translator{
	\type Engraver_group_engraver
	\name Thread
	localKeySignature = #'()
\description "
    Handles note heads, and is contained in the Voice context.  You
    have to instantiate this explicitly if you want to adjust the
    style of individual note heads.
"
	\consists "Font_size_engraver"	
	\consists "Thread_devnull_engraver"
	\consists "Note_heads_engraver"
	\consists "Rest_engraver"

	% why here ? 
	\consists "Output_property_engraver"	
}


\translator{
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

\translator{
	\GrandStaffContext
	\name "PianoStaff"
	\alias "GrandStaff"
\description "
    Just like @code{GrandStaff} but with @code{minVerticalAlign} set
    equal to @code{maxVerticalAlign} so that interstaff beaming and
    slurring can be used."
	verticalAlignmentChildCallback = #Align_interface::fixed_distance_alignment_callback
	VerticalAlignment \override #'forced-distance = #12
	VerticalAlignment \override #'self-alignment-Y = #0

	\consists "Vertical_align_engraver"
	\consists "Instrument_name_engraver"
	
	instrument = #'()
	instr = #'()
	
%	\consistsend "Axis_group_engraver"
}

\translator {
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
	\accepts "GrandStaff"
	\accepts "PianoStaff"
	\accepts "TabStaff"	
	\accepts "Lyrics"
	\accepts "ChordNames"
}

\translator {
	\InnerStaffGroupContext
	\name StaffGroup
	
	\description

	" Groups staffs while adding a bracket on the left side,
	grouping the staves together.  The bar lines of the contained
	staves are connected vertically.
"
	
	\accepts "InnerChoirStaff"
	\accepts "ChoirStaff"
	\accepts "InnerStaffGroup"
	\accepts "FiguredBass"
}


% UGH! JUNKME
\translator{
	\type "Engraver_group_engraver"
	\consistsend "Hara_kiri_engraver"
	minimumVerticalExtent = #'(-1.2 . 2.4)
	extraVerticalExtent = ##f
	verticalExtent = ##f

	\description "
    Corresponds to a voice with lyrics.  Handles the printing of a
    single line of lyrics.
"
	
	\name LyricsVoice 
	\consists "Separating_line_group_engraver"
	\consists "Lyric_engraver"
	\consists "Extender_engraver"
	\consists "Hyphen_engraver"
	\consists "Stanza_number_engraver"
	\consists "Vocal_name_engraver"
	\consists "Skip_event_swallow_translator"
	SeparationItem \set #'padding = #0.5
}
\translator {
	\type "Engraver_group_engraver"
	\name NoteNames
	\consistsend "Axis_group_engraver"

	minimumVerticalExtent = ##f
	extraVerticalExtent = ##f
	verticalExtent = ##f 

	
	\consists "Rest_swallow_translator" 
	\consists "Skip_event_swallow_translator"
	\consists "Tie_engraver"
	\consists "Note_swallow_translator"
	\consists "Note_name_engraver"
	\consists "Separating_line_group_engraver"
}

\translator {
	\type "Engraver_group_engraver"
	\name Lyrics
	\description  "Typesets lyrics."
	
	%% To get folded repeats right.
	\consists Vertical_align_engraver 

	minimumVerticalExtent = ##f
	extraVerticalExtent = ##f
	verticalExtent = ##f 
	
	\accepts "LyricsVoice"
}


\translator {
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
	SeparatingGroupSpanner \override #'padding = #0.8
	verticalExtent = ##f 
}


RemoveEmptyStaffContext= \translator {
	\StaffContext
	\remove "Axis_group_engraver"
	\consistsend "Hara_kiri_engraver"
	\accepts "Voice"

	% hara kiri & auto knee don't work together.
	Beam \override #'auto-knee-gap = #'()
}

AncientRemoveEmptyStaffContext = \translator {
    %% why not add by default?
    
	\RemoveEmptyStaffContext
	\accepts "VaticanaVoice"
	\accepts "GregorianTranscriptionVoice"
}

\translator {
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

	%% TODO? add this alias from Timing_engraver::initialize() ? 
	\consists "Timing_engraver"
	\alias "Timing"
	
	\consists "Output_property_engraver"
	\consists "System_start_delimiter_engraver"
	\consists "Mark_engraver"	
	\consists "Metronome_mark_engraver"	
	\consists "Break_align_engraver"
	\consists "Spacing_engraver"
	\consists "Vertical_align_engraver"
	\consists "Lyric_phrasing_engraver"
	\consists "Bar_number_engraver"
	\consists "Span_arpeggio_engraver"

	\accepts "Staff"
	\accepts "TabStaff"
	\accepts "VaticanaStaff"
	\accepts "GregorianTranscriptionStaff"
	\accepts "StaffContainer"
	\accepts "StaffGroup"
	\accepts "RhythmicStaff"
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
	splitInterval = #'(0 . 1)
	changeMoment = #`(,(ly:make-moment 0 0) . ,(ly:make-moment 1 512))
	systemStartDelimiter =#'SystemStartBar

	melismaBusyProperties = #default-melisma-properties
	
	clefGlyph = #"clefs-G"
	clefPosition = #-2
	centralCPosition = #-6
	
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

	fingeringOrientations = #'(up down)
	tupletNumberFormatFunction = #denominator-tuplet-formatter
	markFormatter = #format-mark-letters
	rehearsalMark = #1 
	subdivideBeams = ##f
	allowBeamBreak = ##f
	extraNatural = ##t
	autoAccidentals = #'(Staff (same-octave . 0))
	autoCautionaries = #'()  

       keyAccidentalOrder = #`(
         (6 . ,FLAT) (2  . ,FLAT) (5 . ,FLAT ) (1  . ,FLAT) (4  . ,FLAT) (0  . ,FLAT) (3  . ,FLAT)
	 (3  . ,SHARP) (0 . ,SHARP) (4 . ,SHARP) (1 . ,SHARP) (5 . ,SHARP) (2 . ,SHARP) (6 . ,SHARP)
         (6 . ,DOUBLE-FLAT) (2  . ,DOUBLE-FLAT) (5 . ,DOUBLE-FLAT ) (1  . ,DOUBLE-FLAT) (4  . ,DOUBLE-FLAT) (0  . ,DOUBLE-FLAT) (3 . ,DOUBLE-FLAT)
         (3  . ,DOUBLE-SHARP) (0 . ,DOUBLE-SHARP) (4 . ,DOUBLE-SHARP) (2 . ,DOUBLE-SHARP) (5 . ,DOUBLE-SHARP) (2 . ,DOUBLE-SHARP) (6 . ,DOUBLE-SHARP)
        )
	breakAlignOrder = #'(
	  instrument-name
	  left-edge
	  ambitus
	  breathing-sign
	  clef
	  rehearsal-mark
	  staff-bar
	  key-signature
	  time-signature
	  custos
	)
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
      	stringTunings   = #guitar-tunings
	tablatureFormat = #fret-number-tablature-format

	%%
	bassFigureFormatFunction = #make-bass-figure-markup
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
		 (Voice Beam position-callbacks (,Beam::least_squares
						    ,Beam::check_concave
						    ,Beam::slope_damping))
		 (Staff Accidental font-size -4)
		 (Voice Slur direction -1)
	)
	
	\grobdescriptions #all-grob-descriptions
}

OrchestralScoreContext = \translator {
	\ScoreContext
}

EasyNotation = \translator {
	\ScoreContext
	NoteHead \override #'molecule-callback = #Note_head::brew_ez_molecule
	NoteHead \override #'Y-extent-callback = #'()
	NoteHead \override #'X-extent-callback = #'()
}



\translator {
	\type "Engraver_group_engraver"
	\name FiguredBass 
	\consists "Figured_bass_engraver"
	\consists "Rest_swallow_translator"
	\consists "Note_swallow_translator"
	\consists "Separating_line_group_engraver"
	
	\consistsend "Hara_kiri_engraver"
}

\translator {
    \name "Devnull"
    \type "Engraver_group_engraver"
    \consists "Swallow_engraver"
    \description "Silently discards all musical information given to this context. "
    }

\translator {
      \VoiceContext
      \name "TabVoice"
      \denies "Thread"
      \consists "Tab_note_heads_engraver"
      \remove "Fingering_engraver"
      \remove "New_fingering_engraver"

      \description "Context for drawing notes in a Tab staff. "
      Slur \override #'font-family       = #'roman
      Slur \override #'molecule-callback = #hammer-molecule-callback
      Slur \override #'direction    = #-1

      % Draws all stems/beams out of the staff (and not in the middle of the staff !)
      % This feature is now disabled because most of the tab does not use it.
      %Beam \override #'damping = #100000
      %Stem \override #'up-to-staff = ##t

      % No accidental in tablature !
      \remove Accidental_engraver
}

\translator {
      \StaffContext
      \alias "Staff"
      \name "TabStaff"
      \denies "Voice"

      \description "Context for generating tablature. [DOCME]"

%{
      TODO: this context should use a special staff_symbol engraver that
      takes the line count out of the stringTunings property.

%}
      
      
      \accepts "TabVoice"
      
      % 6 strings
      StaffSymbol \override #'line-count  = #6
      StaffSymbol \override #'staff-space = #1.5

     % Don't draw stems over the tablature figures !
      Stem \override #'avoid-note-head = ##t
      
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
% "\translator { \Vaticana*Context }" from params-init.ly to the end
% of gregorian-init.ly, then I get "error: parse error, unexpected
% TRANSLATOR: \translator { \VaticanaStaffContext }" in
% gregorian-init.ly. --jr

\translator {
  \VoiceContext
  \name "VaticanaVoice"
  \alias "Voice"
  \description "Same as @code{Voice} context, except that it is accommodated for tyepsetting Gregorian Chant in the notational style of Editio Vaticana."

  % We can not remove Slur_engraver, since \addlyrics depends on it.
  % Instead, we make the grob transparent.
  % Unfortunately, this gives us a lot of warnings ("Degenerate bow:
  % infinite steepness reqd"), since in ligatures, all note heads are in
  % the same paper column such that the (transparent) slurs eventually may
  % start and end in the same column.
  Slur \override #'transparent = ##t

  % We can not remove Stem_engraver, since slurs depend on stems.  If
  % we try anyway, lily will crash in slur.scm:16:6: "Wrong type argument
  % in position 1 (expecting grob): ()".
  % As a workaround, we make the grob transparent.
  Stem \set #'transparent = ##t

  % Since we do not remove stems, but only make it transparent, we have
  % to set the length to 0.0.  Otherwise, articulation marks (such as
  % ictus, circulus or accentus) may be vertically placed quite away from
  % the note head.
  Stem \set #'length = #'0.0

  \remove "Ligature_bracket_engraver"
  \consists "Vaticana_ligature_engraver"

  % Set default head for notes outside of \[ \].
  NoteHead \set #'style = #'vaticana_punctum

  % Put some space before and after divisiones.
  % FIXME: This does not seem to show any effect.
  Script \set #'padding = #0.5

  % There are no beams in Gregorian Chant notation.
  autobeaming = ##f

  % Prepare TextSpanner for \episem{Initium|Finis} use.
  TextSpanner \set #'style = #'line
  TextSpanner \set #'edge-height = #'(0 . 0)
  TextSpanner \set #'padding = #0.5
  TextSpanner \set #'enclose-bounds = #1
  TextSpanner \set #'edge-text = #'("" . "")
}

\translator {
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
  BarLine \set #'transparent = ##t

  StaffSymbol \set #'line-count = #4
  StaffSymbol \set #'thickness = #0.6

  % FIXME: unit on StaffSymbol's width should be \linewidth.
  % StaffSymbol \set #'width = #60.0

  % Choose vaticana do clef on 3rd line as default.
  clefGlyph = #"clefs-vaticana_do"
  centralCPosition = #1
  clefPosition = #1
  clefOctavation = #0

  % Select vaticana style font.
  KeySignature \set #'style = #'vaticana
  Accidental \set #'style = #'vaticana
  Custos \set #'style = #'vaticana
  Custos \set #'neutral-position = #3
  Custos \set #'neutral-direction = #-1
  Custos \set #'adjust-if-on-staffline = ##t

  % Score.timing = ##f
  % Score.barAlways = ##t
}

\translator {
  \VoiceContext
  \name "GregorianTranscriptionVoice"
  \alias "Voice"

  % Removing ligature bracket engraver without replacing it by some
  % other ligature engraver would cause a "Junking event: `LigatureEvent'"
  % warning for every "\[" and "\]".  Therefore, we make the grob
  % transparent instead.
  LigatureBracket \set #'transparent = ##t

  % We can not remove Slur_engraver, since \addlyrics depends on it.
  % Instead, we make the grob transparent.
  % Unfortunately, this gives us a lot of warnings ("Degenerate bow:
  % infinite steepness reqd"), since in ligatures, all note heads are in
  % the same paper column such that the (transparent) slurs eventually may
  % start and end in the same column.
  Slur \override #'transparent = ##t

  % We can not remove Stem_engraver, since slurs depend on stems.  If
  % we try anyway, lily will crash in slur.scm:16:6: "Wrong type argument
  % in position 1 (expecting grob): ()".
  % As a workaround, we make the grob transparent.
  Stem \set #'transparent = ##t

  % Since we do not remove stems, but only make it transparent, we have
  % to set the length to 0.0.  Otherwise, articulation marks (such as
  % ictus, circulus or accentus) may be vertically placed quite away from
  % the note head.
  Stem \set #'length = #'0.0

  % Put some space before and after divisiones.
  % FIXME: This does not seem to show any effect.
  Script \set #'padding = #0.5

  % There are no beams in Gregorian Chant notation.
  autobeaming = ##f

  % Prepare TextSpanner for \episem{Initium|Finis} use.
  TextSpanner \set #'style = #'line
  TextSpanner \set #'edge-height = #'(0 . 0)
  TextSpanner \set #'padding = #0.5
  TextSpanner \set #'enclose-bounds = #1
  TextSpanner \set #'edge-text = #'("" . "")
}
 \translator {
  \StaffContext
  \name "GregorianTranscriptionStaff"
  \alias "Staff"
  \denies "Voice"
  \accepts "GregorianTranscriptionVoice"

  % We can not remove Bar_engraver; otherwise clefs and custodes will
  % not show up any more among other line breaking issues.
  % Instead, we make the grob transparent.
  BarLine \set #'transparent = ##t
}
