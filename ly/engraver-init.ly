\version "1.7.18"



%
% setup for Request->Element conversion. Guru-only
%

StaffContext=\translator {
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


StaffContainerContext = \translator {
	\type Engraver_group_engraver
	\consists "Axis_group_engraver"
	minimumVerticalExtent = ##f
	extraVerticalExtent = ##f
	verticalExtent = ##f 
	localKeySignature = #'()

	\accepts Staff
	\name StaffContainer
}

InnerChoirStaffContext = \translator {
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

ChoirStaffContext = \translator {
	\InnerChoirStaffContext
	\name ChoirStaff
	
	\description "Identical to @code{StaffGroup} except that the
    contained staves are not connected vertically."
	
	\accepts "InnerChoirStaff"
	\accepts "InnerStaffGroup"
}


RhythmicStaffContext=\translator{
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
	VoltaBracket \override #'minimum-space =  #15  % urg, in \pt
	VoltaBracket \override #'padding =  #5  % urg, in \pt
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


VoiceContext = \translator {
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
	\consists "New_tie_engraver"
	\consists "Tuplet_engraver"
	\consists "A2_engraver"

	\consists "Skip_event_swallow_translator"
	\accepts Thread % bug if you leave out this!
}

ThreadContext = \translator{
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



GrandStaffContext=\translator{
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

PianoStaffContext = \translator{
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

InnerStaffGroupContext= \translator {
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

StaffGroupContext = \translator {
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
LyricsVoiceContext= \translator{
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
	\consists "Skip_event_swallow_translator"
	phrasingPunctuation = #".,:!?\""
	SeparationItem \set #'padding = #0.5
}
NoteNamesContext = \translator {
	\type "Engraver_group_engraver"
	\name NoteNames
	\consistsend "Axis_group_engraver"

	minimumVerticalExtent = ##f
	extraVerticalExtent = ##f
	verticalExtent = ##f 

	
	\consists "Note_name_engraver"
	\consists "Separating_line_group_engraver"
}

LyricsContext = \translator {
	\type "Engraver_group_engraver"
	\name Lyrics
	\description  "Typesets lyrics."
	%% To get folded repeats right.
	\consists Vertical_align_engraver 

	\consistsend "Hara_kiri_engraver"
	minimumVerticalExtent = ##f
	extraVerticalExtent = ##f
	verticalExtent = ##f 
	
	\accepts "LyricsVoice"
}


ChordNamesContext = \translator {
	\type "Engraver_group_engraver"
	\name ChordNames
\description "    Typesets chord names."
	
	\consists "Rest_swallow_translator" 
	\consists "Output_property_engraver"	
	\consists "Separating_line_group_engraver"
	\consists "Chord_name_engraver"
	\consists "Skip_event_swallow_translator"
	\consistsend "Hara_kiri_engraver"
	minimumVerticalExtent = #'(0 . 2.5)
	extraVerticalExtent = ##f
	verticalExtent = ##f 
}


RemoveEmptyStaffContext = \translator {
	\StaffContext
	\remove "Axis_group_engraver"
	\consistsend "Hara_kiri_engraver"
	\consists "Instrument_name_engraver"
	\accepts "Voice"

	% hara kiri & auto knee don't work together.
	Beam \override #'auto-knee-gap = #'()
}

HaraKiriStaffContext = \translator {
	\RemoveEmptyStaffContext
	\accepts "VaticanaVoice"
}

ScoreContext = \translator {
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
	\accepts "StaffContainer"
	\accepts "StaffGroup"
	\accepts "RhythmicStaff"
	\accepts "Lyrics"
	\accepts "ChordNames"
	\accepts "GrandStaff"
	\accepts "ChoirStaff"
	\accepts "PianoStaff"
	\accepts "NoteNames"
	\accepts "FiguredBass"

	soloText = #"Solo"
	soloIIText = #"Solo II"
	aDueText = #"a2"
	soloADue = ##t
	splitInterval = #'(0 . 1)
	changeMoment = #`(,(ly:make-moment 0 0) . ,(ly:make-moment 1 512))
	systemStartDelimiter =#'SystemStartBar


	clefGlyph = #"clefs-G"
	clefPosition = #-2
	centralCPosition = #-6
	
        automaticPhrasing = ##t
	automaticMelismata = ##t
	
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

	
	tupletNumberFormatFunction = #denominator-tuplet-formatter
	
	subdivideBeams = ##f
	allowBeamBreak = ##f
	extraNatural = ##t
	autoAccidentals = #'(Staff (same-octave . 0))
	autoCautionaries = #'()  

       keyAccidentalOrder = #'(
         (6 . -1) (2  . -1) (5 . -1 ) (1  . -1) (4  . -1) (0  . -1) (3  . -1)
	 (3  . 1) (0 . 1) (4 . 1) (1 . 1) (5 . 1) (2 . 1) (6 . 1)
         (6 . -2) (2  . -2) (5 . -2 ) (1  . -2) (4  . -2) (0  . -2) (3 . -2)
         (3  . 2) (0 . 2) (4 . 2) (2 . 2) (5 . 2) (2 . 2) (6 . 2)
        )
	breakAlignOrder = #'(
	  instrument-name
	  left-edge
	  ambitus
	  breathing-sign
	  clef
	  staff-bar
	  key-signature
	  time-signature
	  custos
	)
	barCheckSynchronize = ##t

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
	metronomeMarkFormatter = #make-metronome-markup

	\grobdescriptions #all-grob-descriptions
}

OrchestralScoreContext= \translator {
	\ScoreContext
}

EasyNotation =  \translator {
	\ScoreContext
	NoteHead \override #'molecule-callback = #Note_head::brew_ez_molecule
}



FiguredBassContext = \translator {
	\type "Engraver_group_engraver"
	\name FiguredBass 
	\consists "Figured_bass_engraver"
	\consists "Rest_swallow_translator"
	\consists "Note_swallow_translator"
	\consists "Separating_line_group_engraver"
	
	\consistsend "Hara_kiri_engraver"
}

TabVoiceContext =   \translator {
      \VoiceContext
      \name "TabVoice"
      \denies "Thread"
      \consists "Tab_note_heads_engraver"
      \remove "Fingering_engraver"
      \remove "New_fingering_engraver"
      
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

TabStaffContext = \translator {
      \StaffContext
      \alias "Staff"
      \name "TabStaff"
      \denies "Voice"

      \description "Context for generating tablature. [DOCME]"
      
      \accepts "TabVoice"
      
      % 6 strings
      StaffSymbol \override #'line-count  = #6
      StaffSymbol \override #'staff-space = #1.5

     % Don't draw stems over the tablature figures !
      Stem \override #'avoid-note-head = ##t
      
      % No accidental in tablature !
      \remove Accidental_engraver
      \remove Key_engraver

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

VaticanaVoiceContext = \translator {
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
  TextSpanner \set #'edge-text = #'("" . "")
}

VaticanaStaffContext = \translator {
  \StaffContext
  \name "VaticanaStaff"
  \alias "Staff"
  \denies "Voice"
  \accepts "VaticanaVoice"
%  \description "Same as @code{Staff} context, except that it is accommodated for tyepsetting Gregorian Chant in the notational style of Editio Vaticana."

  \remove "Time_signature_engraver"
  \consists "Custos_engraver"

  % We can not remove Bar_engraver, since clef and custos depend on it.
  % Instead, we make the grob transparent.
  BarLine \set #'transparent = ##t

  StaffSymbol \set #'line-count = #4

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
