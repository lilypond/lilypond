\version "1.5.68"

%
% setup for Request->Element conversion. Guru-only
%

StaffContext=\translator {
	\type "Engraver_group_engraver"
	\name Staff 
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

	instrument = ##f
	instr = ##f
	  
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
	%% alignmentReference = #0 FIXME
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
	\accepts "InnerChoirStaff"
	\accepts "InnerStaffGroup"
}


RhythmicStaffContext=\translator{
	\type "Engraver_group_engraver"
	
	\consists "Output_property_engraver"	


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


	localKeySignature = #'()
	\consists "Font_size_engraver"
	
	% must come before all
	\consists "Voice_devnull_engraver"
	\consists "Output_property_engraver"	
	\consists "Arpeggio_engraver"
	\consists "Multi_measure_rest_engraver"
	\consists "Text_spanner_engraver"
	\consists "Grob_pq_engraver"

	\consists "Ligature_bracket_engraver"
	\consists "Breathing_sign_engraver"
 	% \consists "Rest_engraver"
	\consists "Stem_engraver"
	\consists "Beam_engraver"
	\consists "Grace_beam_engraver"
	\consists "Auto_beam_engraver"

	\consists "Chord_tremolo_engraver"
	\consists "Percent_repeat_engraver"
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
	\consists "Slur_engraver"
	\consists "Tie_engraver"
	\consists "Porrectus_engraver"
	\consists "Tuplet_engraver"
	\consists "A2_engraver"

	\consists "Skip_req_swallow_translator"
	\accepts Thread % bug if you leave out this!
}

ThreadContext = \translator{
	\type Engraver_group_engraver
	\name Thread
	localKeySignature = #'()

	\consists "Font_size_engraver"	
	\consists "Thread_devnull_engraver"
	\consists "Note_heads_engraver"
	\consists "Rest_engraver"
	\consists "Note_head_line_engraver"
	\consists "Output_property_engraver"	

}



GrandStaffContext=\translator{
	\type "Engraver_group_engraver"
	\name GrandStaff
	localKeySignature = #'()
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
	\accepts "InnerChoirStaff"
	\accepts "ChoirStaff"
	\accepts "InnerStaffGroup"
	\accepts "FiguredBass"
}


% UGH! JUNKME
LyricsVoiceContext= \translator{
	\type "Engraver_group_engraver"
	\consistsend "Axis_group_engraver"
	minimumVerticalExtent = #(cons -1.2 1.2)
	extraVerticalExtent = ##f
	verticalExtent = ##f 
	\name LyricsVoice 
	\consists "Separating_line_group_engraver"
	\consists "Lyric_engraver"
	\consists "Extender_engraver"
	\consists "Hyphen_engraver"
	\consists "Stanza_number_engraver"
	\consists "Skip_req_swallow_translator"
	phrasingPunctuation = #".,:!?\""
	
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
	\consists Vertical_align_engraver %need this for getting folded repeats right.


	\consistsend "Axis_group_engraver"
	minimumVerticalExtent = ##f
	extraVerticalExtent = ##f
	verticalExtent = ##f 
	
	\accepts "LyricsVoice"
}


ChordNamesContext = \translator {
	\type "Engraver_group_engraver"
	\name ChordNames




	\consists "Output_property_engraver"	
	\consists "Separating_line_group_engraver"
	\consists "Chord_name_engraver"
	\consists "Skip_req_swallow_translator"
	\consistsend "Axis_group_engraver"
	minimumVerticalExtent = ##f
	extraVerticalExtent = ##f
	verticalExtent = ##f 
}



StupidScore = \translator {
 	\type "Score_engraver"
	\name Score
	\consists "Note_heads_engraver"
}




HaraKiriStaffContext = \translator {
	\StaffContext
	\remove "Axis_group_engraver"
	\consistsend "Hara_kiri_engraver"
	\consists "Instrument_name_engraver"
	\accepts "Voice"

	% hara kiri & auto knee don't work together.
	Beam \override #'auto-knee-gap = #'()
}

%{
  The HaraKiriStaffContexts doesn't override \name,
  so it is still named `Staff'.

  %\translator { \HaraKiriStaffContext }
%}




ScoreContext = \translator {
	\type Score_engraver
	\name Score
	localKeySignature = #'()
	
	\consists "Repeat_acknowledge_engraver"
	\consists "Staff_collecting_engraver"

	% move the alias along with the engraver.

	%% TODO? add this alias from Timing_engraver::initialize() ? 
	\consists "Timing_engraver"
	\alias Timing
	
	\consists "Output_property_engraver"
	\consists "System_start_delimiter_engraver"
	\consists "Mark_engraver"	
	\consists "Break_align_engraver"
	\consists "Spacing_engraver"
	\consists "Vertical_align_engraver"
	\consists "Lyric_phrasing_engraver"
	\consists "Bar_number_engraver"
	\consists "Span_arpeggio_engraver"

	\accepts "Staff"
	\accepts "TabStaff"
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
	changeMoment = #`(,(make-moment 0 0) . ,(make-moment 1 512))
	systemStartDelimiter =#'SystemStartBar


	%  name, glyph id, clef position 
	% where is c0 in this clef?

	clefGlyph = #"clefs-G"
	clefPosition = #-2
	centralCPosition = #-6
	
        automaticPhrasing = ##t
	automaticMelismata = ##t
	
	defaultBarType = #"|"
	barNumberVisibility = #default-bar-number-visibility

	explicitClefVisibility = #all-visible
	explicitKeySignatureVisibility = #all-visible
	autoBeamSettings = #auto-beam-settings
	autoBeaming = ##t
	scriptDefinitions = #default-script-alist

	verticalAlignmentChildCallback = #Align_interface::alignment_callback

	pedalSustainStrings = #'("Ped." "*Ped." "*")
	pedalUnaCordaStrings = #'("una corda" "" "tre corde")

	%% these are in ordinary italic font, including the *, but they are unlikely to be used, 
	%% as the default pedal-style for SostenutoPedal is 'mixed': i.e.  Sost. Ped_____________________ 
	pedalSostenutoStrings = #'("Sost. Ped." "*Sost. Ped." "*") 

	tupletNumberFormatFunction = #denominator-tuplet-formatter
	
	subdivideBeams = ##f
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
	  span-bar
	  breathing-sign
	  clef
	  key-signature
	  staff-bar
	  time-signature
	  custos
	)
	barCheckSynchronize = ##t

	\grobdescriptions #all-grob-descriptions
}

OrchestralScoreContext= \translator {
	\ScoreContext
}
EasyNotation =  \translator {
	\ScoreContext
	NoteHead \override #'molecule-callback = #Note_head::brew_ez_molecule
}

% retain for compatibility reasons (FIXME: convert-ly)
GraceContext = \translator {
	\type "Engraver_group_engraver"
}

FiguredBassContext = \translator {
	\type "Engraver_group_engraver"
	\name FiguredBass 
	\consists "Figured_bass_engraver"	
	\consistsend "Axis_group_engraver"
}


TabVoiceContext =   \translator {
      \VoiceContext
      \name "TabVoice"
      \denies "Thread"
      \consists "Tab_note_heads_engraver"

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
      \accepts "TabVoice"
      
      % 6 strings
      StaffSymbol \override #'line-count  = #6
      StaffSymbol \override #'staff-space = #1.5
      % One may change the strings tuning as following :
      % The lenght of the list must be equal to the number of string
      %TabNoteHead \override #'string-tunings = #'(10 10 10 10 10 10)
      
      % Special "TAB" clef
      clefGlyph = #"clefs-tab"
      clefPosition = #0
      
      % Don't draw stems over the tabature figures !
      Stem \override #'avoid-note-head = ##t
      
      % No accidental in tablature !
      \remove Accidental_engraver
      stringTunings   = #'(-20 -15 -10 -5 -1 4)
      tablatureFormat = #fret-number-tablature-format
}
   
