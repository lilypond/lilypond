%
% setup for Request->Element conversion. Guru-only
%

StaffContext=\translator {
	\type "Engraver_group_engraver";
	\name Staff ;
	\consists "Output_property_engraver";	
	barAuto = ##t
	voltaVisibility = ##t
	Generic_property_list = #generic-staff-properties
	\consists "Property_engraver";
	
	\consists "Multi_measure_rest_engraver";
	\consists "Bar_engraver";
 % Bar_engraver must be first so default bars aren't overwritten
% with empty ones.

	voltaPadding = #5  % urg, in \pt
	voltaMinimumSpace = #25  % urg, in \pt

	StaffMinimumVerticalExtent = #(cons -4.0 4.0)

	\consists "Repeat_engraver";


	%  name, glyph id, c0 position
	supportedClefTypes = #'(
	  ("treble" . ("clefs-G" -2))
	  ("violin" . ("clefs-G" -2))
	  ("G" . ("clefs-G" -2))
	  ("G2" . ("clefs-G" -2))
	  ("french" . ("clefs-G" -4 ))
	  ("soprano" . ("clefs-C" -4 ))
	  ("mezzosoprano" . ("clefs-C" -2 ))
	  ("alto" . ("clefs-C" 0 ))
	  ("tenor" . ("clefs-C" 2 ))
	  ("baritone" . ("clefs-C" 4 ))
	  ("varbaritone"  . ("clefs-F" 0))
	  ("bass" . ("clefs-F" 2 ))
	  ("F" . ( "clefs-F" 2))
	  ("subbass" . ("clefs-F" 4))
	)
	% where is c0 in this clef?
	clefPitches = #'(("clefs-G" . -4)
	  ("clefs-C" . 0)
	  ("clefs-F" . 4))
	  
	\consists "Clef_engraver";
	\consists "Key_engraver";
	\consists "Time_signature_engraver";
	\consists "Staff_symbol_engraver";
	\consists "Collision_engraver";
	\consists "Rest_collision_engraver";
	\consists "Local_key_engraver";

	\consistsend "Axis_group_engraver";



%{
	The Instrument_name_engraver puts the name of the instrument
	(\property Staff.instrument; Staff.instr for subsequent lines)
	to the left of a staff.
	Usually, you only want this in the full score, not in the parts.

	\consists "Instrument_name_engraver";
%}

	defaultClef = #"treble"

	\consists "Separating_line_group_engraver";
	  
	\accepts "Voice";
	dynamicStyle = #"dynamic"
};

\translator{\StaffContext }
\translator {
	\type "Engraver_group_engraver";
	\name ChoirStaff;
	alignmentReference = \center;
	\consists "System_start_delimiter_engraver";
	systemStartDelimiterGlyph = #'bracket



	\accepts "Staff";
	\accepts "RhythmicStaff";
	\accepts "GrandStaff";
	\accepts "PianoStaff";

		
	\accepts "Lyrics";
	\accepts "ChordNames";
}


RhythmicStaffContext=\translator{
	\type "Engraver_group_engraver";
	numberOfStaffLines  = #1
	\consists "Property_engraver";
	\consists "Output_property_engraver";	

	Generic_property_list = #generic-staff-properties
	
	barSize =   4.0 * \interline ; 
	\consists "Pitch_squash_engraver";
	\consists "Separating_line_group_engraver";	
	\name RhythmicStaff;

	voltaPadding = #5  % urg, in \pt
	voltaMinimumSpace = #15  % urg, in \pt
	\consists "Repeat_engraver";
	\consists "Bar_engraver";
	\consists "Time_signature_engraver";
	\consists "Staff_symbol_engraver";
	\consistsend "Axis_group_engraver";
	\accepts "Voice";
};
\translator{\RhythmicStaffContext}

VoiceContext = \translator {
	\type "Engraver_group_engraver";
	\name Voice;

	dynamicPadding = #3  % urg, in \pt
	dynamicMinimumSpace = #6  % urg, in \pt

	Generic_property_list = #generic-voice-properties
	\consists "Output_property_engraver";	

	\consists "Dynamic_engraver";   % must come before text_engraver.
	\consists "Property_engraver";
	
	\consists "Breathing_sign_engraver";
 	\consists "Rest_engraver";
	\consists "Dot_column_engraver";
	\consists "Stem_engraver";
	\consists "Beam_engraver";
	\consists "Auto_beam_engraver";
	\include "auto-beam-settings.ly";

	\consists "Chord_tremolo_engraver";
	\consists "Melisma_engraver";
	textScriptPadding = #3.0
	\consists "Text_engraver";


	startSustain = #"Ped."
	stopSustain = #"*"
	stopStartSustain = #"*Ped."
	startUnaChorda = #"una chorda"
	stopUnaChorda = #"tre chorde"

	\consists "Piano_pedal_engraver";
	\consists "Script_engraver";
	\consists "Script_column_engraver";
	\consists "Rhythmic_column_engraver";
	\consists "Slur_engraver";
	\consists "Tie_engraver";
	\consists "Tuplet_engraver";
	\consists "Grace_position_engraver";
	\consists "Skip_req_swallow_translator";
	\accepts Thread; % bug if you leave out this!
	\accepts Grace;
};

GraceContext=\translator {
	\type "Grace_engraver_group";
	\name "Grace";
	\consists "Output_property_engraver";	

	Generic_property_list = #generic-grace-properties
	
	\consists "Note_heads_engraver";
	\consists "Local_key_engraver";
	\consists "Stem_engraver";
	\consists "Beam_engraver";
	\consists "Slur_engraver";
	
	\consists "Auto_beam_engraver";
	\include "auto-beam-settings.ly";
	\consists "Align_note_column_engraver";

	\consists "Rhythmic_column_engraver";
	\consists "Dynamic_engraver";
	\consists "Text_engraver";

	\consists "Property_engraver";

	stemStyle = #"grace"
	flagStyle = #"grace" 
	weAreGraceContext = ##t 
	fontSize = #-1
	
	stemLength = #6.0
	verticalDirection = \up ;
	graceAccidentalSpace= 1.5 * \staffspace;
};

\translator{\GraceContext}
\translator {\VoiceContext}

ThreadContext = \translator{
	\type Engraver_group_engraver;
	\consists "Note_heads_engraver" ;
	\consists "Output_property_engraver";	
	Generic_property_list = #generic-thread-properties
	\consists "Property_engraver";
	\name Thread;
};

\translator{\ThreadContext}
GrandStaffContext=\translator{
	\type "Engraver_group_engraver";
	\name GrandStaff;
	\consists "Span_bar_engraver";
	\consists "System_start_delimiter_engraver";
	systemStartDelimiterGlyph = #'brace
	
	\consists "Property_engraver";	
	Generic_property_list = #generic-grand-staff-properties
	\accepts "Staff";
};
\translator{\GrandStaffContext}

PianoStaffContext = \translator{\GrandStaffContext
	alignmentReference = \center;

	\consists "Vertical_align_engraver";
	minVerticalAlign = 3.0*\staffheight;
	maxVerticalAlign = 3.0*\staffheight;

%	\consistsend "Axis_group_engraver";
	\name "PianoStaff";
	
};
\translator{\PianoStaffContext}
StaffGroupContext= \translator {
	\type "Engraver_group_engraver";
	\consists "Span_bar_engraver";
	\consists "Output_property_engraver";	
	\consists "System_start_delimiter_engraver";
       systemStartDelimiterGlyph = #'bracket
       




	\name StaffGroup;
	\accepts "Staff";
	\accepts "RhythmicStaff";
	\accepts "GrandStaff";
	\accepts "PianoStaff";
	
	\accepts "Lyrics";
	\accepts "ChordNames";
};

\translator { \StaffGroupContext }

% UGH! JUNKME
LyricsVoiceContext= \translator{
	\type "Engraver_group_engraver";
	\consistsend "Axis_group_engraver";

	\name LyricVoice ;
	\consists "Separating_line_group_engraver";
	\consists "Lyric_engraver";
	\consists "Extender_engraver";
	\consists "Hyphen_engraver";
};
\translator{ \LyricsVoiceContext }

LyricsContext = \translator {
	\type "Engraver_group_engraver";
	\name Lyrics;
	\consists Vertical_align_engraver; %need this for getting folded repeats right.
	Generic_property_list = #generic-lyrics-properties
	\consists "Property_engraver";
	\consistsend "Axis_group_engraver";
	
	\accepts "LyricVoice";
};
\translator { \LyricsContext }

ChordNameVoiceContext = \translator {
	\type "Engraver_group_engraver";
	\name ChordNameVoice ;

	\consists "Output_property_engraver";	
	\consistsend "Axis_group_engraver";
	\consists "Separating_line_group_engraver";
	\consists "Chord_name_engraver";
};
\translator {\ChordNameVoiceContext}

ChordNameContext = \translator {
	\type "Engraver_group_engraver";
	\name ChordNames;

	Generic_property_list = #generic-chord-staff-properties
	\consists "Property_engraver";	
	\consists "Output_property_engraver";	
	\accepts "ChordNameVoice";
	\consistsend "Axis_group_engraver";
	};
\translator { \ChordNameContext }


ScoreWithNumbers = \translator {
 	\type "Score_engraver";

	% uncomment to bar numbers on a whole system.
	\consists "Bar_number_engraver";
};

StupidScore = \translator {
 	\type "Score_engraver";
	\name Score;
	\consists "Note_heads_engraver";
};



BarNumberingStaffContext = \translator {
	\StaffContext
	\consists "Mark_engraver";
};

HaraKiriStaffContext = \translator {
	\StaffContext
	\remove "Axis_group_engraver";
	\consistsend "Hara_kiri_engraver";	  
	\consists "Instrument_name_engraver";
	\accepts "Voice";
};
%{
  The HaraKiriStaffContexts doesn't override \name,
  so it is still named `Staff'.

  %\translator { \HaraKiriStaffContext }
%}

OrchestralPartStaffContext = \translator {
	\StaffContext
	\consists "Mark_engraver";
};

ScoreContext = \translator {
	\type Score_engraver;
	\name Score;

	marginScriptPadding = #10  % urg, in \pt

	\consists "Timing_engraver";
	\consists "Output_property_engraver";	

	%bracketCollapseHeight = #10  % \pt
	\consists "System_start_delimiter_engraver";
	
%	\consists "Score_priority_engraver";
	\consists "Break_align_engraver";
	breakAlignOrder = #'(
	  Instrument_name
	  Left_edge_item
	  Span_bar
	  Breathing_sign
	  Clef_item
	  Key_item
	  Staff_bar
	  Time_signature
	)
	\consists "Spacing_engraver";

	\consists "Vertical_align_engraver";
	\consists "Bar_number_engraver";
	alignmentReference = \down;
	defaultClef = #"treble"
	defaultBarType = #"|"
       systemStartDelimiterGlyph = #'bar-line

       %
       % what order to print accs.  We could compute this, 
       % but computing is more work than putting it here.
       %
       % Flats come first, then sharps.
       keyAccidentalOrder = #'(
         (6 . -1) (2  . -1) (5 . -1 ) (1  . -1) (4  . -1) (0  . -1) (3  . -1)
	 (3  . 1) (0 . 1) (4 . 1) (1 . 1) (5 . 1) (2 . 1) (6 . 1)
       )
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% default settings, mainly for breakable items
	% in alphabetical order
	% TODO: uniform naming.;  
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%
	% distances are given in stafflinethickness (thicknesses) and staffspace (distances)
	%
	
	
	basicBarProperties = #`(
		(break-align-symbol . Staff_bar)
		(molecule-callback . ,Bar::scheme_molecule)	   
		(visibility-lambda . ,begin-of-line-invisible)
		(breakable . #t)

		;;
		;; Ross. page 151 lists other values, we opt for a leaner look
		;; 
		(kern . 3.0)
		(thin-kern . 3.0)
		(hair-thickness . 1.6)
		(thick-thickness . 6.0)
	)

	basicBarNumberProperties = #`(
		(molecule-callback . ,Text_item::scheme_molecule)
		(breakable . #t)
		(visibility-lambda . ,begin-of-line-visible)
	)

	basicBeamProperties = #`(
		(molecule-callback . ,Beam::scheme_molecule)
		(beam-thickness . 0.42) ; interline!
	)

	basicBreakAlignProperties = #`(
		(breakable . #t)
	)

	
	basicBreathingSignProperties = #`(
		(break-align-symbol . Breathing_sign)
		(breakable . #t )
		(molecule-callback . ,Breathing_sign::scheme_molecule)
		(visibility-lambda . ,begin-of-line-invisible)
	)
	 basicClefItemProperties = #`(
   	   (molecule-callback . ,Score_element::scheme_molecule)
	   (breakable . #t)
	   (break-align-symbol . Clef_item)
	   (visibility-lambda . ,begin-of-line-visible) 
	 )
	basicSlurProperties = #`(
		(molecule-callback . ,Slur::scheme_molecule)
	)
	basicChordNameProperties = #`(
		(molecule-callback . ,Chord_name::scheme_molecule)
	)
	basicCollisionProperties = #`(
		(axes 0 1)
	)
	basicCrescendoProperties = #`(
		(molecule-callback . ,Crescendo::scheme_molecule)
	)
	basicDotsProperties = #`(
		(molecule-callback . ,Dots::scheme_molecule)
		(dot-count . 1) 
	)
	basicDynamicLineSpannerProperties = #`(

	)
	basicDynamicTextProperties	 = # `(
		(style . "dynamic")
		(molecule-callback . ,Text_item::scheme_molecule)
		(script-priority . 100)
		(self-alignment-Y . 0)
	)
	leftEdgeBasicProperties = #`(
	  (break-align-symbol . Left_edge_item)
	  (breakable . #t)
	)
	basicHyphenSpannerProperties = #`(
	(thickness . 1.0)
	(height . 0.4)
	(minimum-length .  0.5) 
		(molecule-callback . ,Hyphen_spanner::scheme_molecule)
	)
	basicKeyProperties = #`(
  	  (molecule-callback . ,Key_item::scheme_molecule)
	  (break-align-symbol . Key_item)
	  (visibility-lambda . ,begin-of-line-visible)
	  (breakable . #t)
	  )	
	basicLocalKeyProperties = #`(
		(molecule-callback . ,Local_key_item::scheme_molecule)
		(left-padding . 0.2)
		(right-padding . 0.4)
	)
	basicLyricExtenderProperties = #`(
		(molecule-callback . ,Lyric_extender::scheme_molecule)
	)
	basicLyricTextProperties = #`(
		(molecule-callback . ,Text_item::scheme_molecule)
		(non-rhythmic . #t)
	)
	basicMarkProperties = #`(
	  (molecule-callback . ,Text_item::scheme_molecule)	
	  (breakable . #t)
	  (visibility-lambda . ,end-of-line-invisible)
	)
	basicMultiMeasureRestProperties = #`(
		(molecule-callback . ,Multi_measure_rest::scheme_molecule)
		(staff-position . 0)
	)
	basicNoteColumnProperties = #`(
		(axes 0 1)
	)
	basicNoteHeadProperties = #`(
		(molecule-callback . ,Note_head::scheme_molecule)
	)
	basicOctavateEightProperties  = #`(
		(self-alignment-X . 0)
		(text . "8")
		(visibility-lambda . ,begin-of-line-visible) 
		(molecule-callback . ,Text_item::scheme_molecule)
		(style . "italic")
	)
	basicPedalProperties = #`(
		(molecule-callback . ,Text_item::scheme_molecule)
		(style . "italic")
		(no-spacing-rods . #t)
		(self-alignment-X . 0)
				
	)
	basicTextProperties = #`( )
	basicRestProperties = #`( 
		(molecule-callback . ,Rest::scheme_molecule)
	)
	
	basicRestCollisionProperties = #`(
	)
	basicScriptProperties	 = #`(
		(molecule-callback . ,Script::scheme_molecule)
	)
 		
	basicSlurProperties = #`(
		(molecule-callback . ,Slur::scheme_molecule)
	)
	basicSustainPedalProperties = #`(
		(no-spacing-rods . #t)
		(molecule-callback . ,Sustain_pedal::scheme_molecule)
		(self-alignment-X . 0)
	)	
	basicSystemStartDelimiterProperties = #`(
		(molecule-callback . ,System_start_delimiter::scheme_molecule)
		(collapse-height . 1.0)
		(thickness . 1.6)
		(arch-height . 1.5)
		(arch-angle . 50.0)
		(arch-thick . 0.25)
		(arch-width . 1.5)
		(bracket-thick . 0.25)
		(bracket-width . 2.0)
	)
	basicStemProperties = #`(
		(molecule-callback . ,Stem::scheme_molecule)
	)
	staffSymbolBasicProperties = #`(
		(molecule-callback . ,Staff_symbol::scheme_molecule)
		(staff-space . 1.0 )
		(line-count . 5 )
	)
	basicTextScriptProperties = #`(
		(molecule-callback . ,Text_item::scheme_molecule)
		(no-spacing-rods . #t)
	)
	basicTimeSignatureProperties = #`(
		(molecule-callback . ,Time_signature::scheme_molecule)
		(break-align-symbol . Time_signature)
		(visibility-lambda . ,all-visible)
		(breakable . #t)
	)
	basicTieProperties = #`(
		(molecule-callback . ,Tie::scheme_molecule)
	)
	basicTieColumnProperties = #`(
		
	)
	basicTupletSpannerProperties = #`(
		(number-gap . 2.0)   
		(thick . 1.0) 
		(molecule-callback . ,Tuplet_spanner::scheme_molecule)
	)	
	basicStemTremoloProperties = #`(
	   	(molecule-callback . ,Stem_tremolo::scheme_molecule)
		(beam-width . 4.0) ; interline!
		(beam-thickness . 0.42) ; interline!		
	)

   	basicSingleMaltGroupingItemProperties = #`(

	)
	basicInstrumentNameProperties = #`(
		(breakable . #t)
		(molecule-callback . ,Text_item::scheme_molecule)		
		(break-align-symbol . Instrument_name)
		(visibility-lambda . ,begin-of-line-visible)
	)
	basicVerticalAxisGroupProperties = #`(
		(axes 1)
	)
	basicVoltaSpannerProperties = #`(
		(molecule-callback . ,Volta_spanner::scheme_molecule)
	)
	
	\accepts "Staff";
	\accepts "StaffGroup";
	\accepts "RhythmicStaff";	
	\accepts "Lyrics";
	\accepts "ChordNames";
	\accepts "GrandStaff";
	\accepts "ChoirStaff";
	\accepts "PianoStaff";
	\accepts "NoteNames";


	markVisibilityFunction = #end-of-line-invisible
};

\translator { \ScoreContext }

OrchestralScoreContext= \translator {
	\ScoreContext

	barScriptPadding = #2.0		% dimension \pt
	markScriptPadding = #4.0

	\consists "Mark_engraver";
};

\translator {
	\type "Engraver_group_engraver";
	\name NoteNames;
	\consistsend "Axis_group_engraver";
	\consists "Note_name_engraver";
}
