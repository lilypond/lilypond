
\version "1.3.93"
	%
% setup for Request->Element conversion. Guru-only
%

StaffContext=\translator {
	\type "Engraver_group_engraver";
	\name Staff ;
	\consists "Output_property_engraver";	
	Generic_property_list = #generic-staff-properties
	\consists "Property_engraver";
	
	\consists "Multi_measure_rest_engraver";

	\consists "Bar_engraver";
 % Bar_engraver must be first so default bars aren't overwritten
% with empty ones.


%	\consists "Repeat_engraver";
	\consists "Volta_engraver";
	\consists "Separating_line_group_engraver";	



	\consists "Clef_engraver";
	\consists "Key_engraver";
	\consists "Time_signature_engraver";
	\consists "Staff_symbol_engraver";
	\consists "Collision_engraver";
	\consists "Rest_collision_engraver";
	\consists "Local_key_engraver";
	\consists "Piano_pedal_engraver";
	\consists "Arpeggio_engraver";

	\consistsend "Axis_group_engraver";

%{
	The Instrument_name_engraver puts the name of the instrument
	(\property Staff.instrument; Staff.instr for subsequent lines)
	to the left of a staff.

	This is commented out, so you don't get funny things on the
	PianoStaff	
	\consists "Instrument_name_engraver";
%}


	  
	\accepts "Voice";
}

ChoirStaffContext = \translator {
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
	
	\consists "Property_engraver";
	\consists "Output_property_engraver";	

	Generic_property_list = #generic-staff-properties

	\consists "Pitch_squash_engraver";
	\consists "Separating_line_group_engraver";	
	\name RhythmicStaff;
	Bar \push #'bar-size = #4
	VoltaBracket \push #'minimum-space =  #15  % urg, in \pt
	VoltaBracket \push #'padding =  #5  % urg, in \pt
	StaffSymbol \push #'line-count = #1	

%	\consists "Repeat_engraver";
	\consists "Volta_engraver";
	\consists "Bar_engraver";
	\consists "Time_signature_engraver";
	\consists "Staff_symbol_engraver";
	\consistsend "Axis_group_engraver";
	\accepts "Voice";
}


VoiceContext = \translator {
	\type "Engraver_group_engraver";
	\name Voice;

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

	\consists "Chord_tremolo_engraver";
	\consists "Melisma_engraver";
	\consists "Text_engraver";
	\consists "A2_engraver";
	\consists "Voice_devnull_engraver";

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
}

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
	\consists "Align_note_column_engraver";

	\consists "Rhythmic_column_engraver";

	\consists "Dynamic_engraver";% in Grace ???
	\consists "Text_engraver"; % in Grace ???

	\consists "Property_engraver";

	Stem \push  #'style = #"grace"
	Stem \push  #'flag-style = #"grace"
	Stem \push  #'stem-length = #6.0
	Stem \push  #'direction = #1

	NoteHead \push #'font-size = #-1
	Stem \push #'font-size = #-1
	Stem \push #'stem-shorten = #'(0)
	Beam \push #'font-size = #-1
	TextScript \push #'font-size = #-1
	Slur \push #'font-size = #-1
	Accidentals \push #'font-size = #-1
	Beam \push #'beam-thickness = #0.3
	Beam \push #'beam-space-function = #(lambda (x) 0.5)

	Stem \push #'lengths = #(map (lambda (x) (* 0.8 x)) '(3.5 3.5 3.5 4.5 5.0))
	Stem \push #'beamed-lengths =
		#'(0.0 2.5 2.0 1.5)
	Stem \push #'minimum-beamed-lengths
		 = #(map (lambda (x) (* 0.8 x)) '(0.0 2.5 2.0 1.5))

	weAreGraceContext = ##t   
	graceAccidentalSpace= 1.5 ; % in staff space
}

ThreadContext = \translator{
	\type Engraver_group_engraver;
	\consists "Thread_devnull_engraver";
	\consists "Note_heads_engraver";
	\consists "Output_property_engraver";	
	Generic_property_list = #generic-thread-properties
	\consists "Property_engraver";
	\name Thread;
}

GrandStaffContext=\translator{
	\type "Engraver_group_engraver";
	\name GrandStaff;
	\consists "Span_bar_engraver";
	\consists "Span_arpeggio_engraver";
	\consists "System_start_delimiter_engraver";
	systemStartDelimiterGlyph = #'brace
	
	\consists "Property_engraver";	
	Generic_property_list = #generic-grand-staff-properties
	\accepts "Staff";
}

PianoStaffContext = \translator{
	\GrandStaffContext
	\name "PianoStaff";

	\consists "Vertical_align_engraver";

	alignmentReference = \center;
	VerticalAlignment \push #'threshold = #'(12 . 12) 

%	\consistsend "Axis_group_engraver";
}

StaffGroupContext= \translator {
	\type "Engraver_group_engraver";
	\name StaffGroup;

	\consists "Span_bar_engraver";
	\consists "Span_arpeggio_engraver";
	\consists "Output_property_engraver";	
	systemStartDelimiterGlyph = #'bracket
	\consists "System_start_delimiter_engraver";
	\accepts "Staff";
	\accepts "RhythmicStaff";
	\accepts "GrandStaff";
	\accepts "PianoStaff";
	
	\accepts "Lyrics";
	\accepts "ChordNames";
}


% UGH! JUNKME
LyricsVoiceContext= \translator{
	\type "Engraver_group_engraver";
	\consistsend "Axis_group_engraver";
	LyricVoiceMinimumVerticalExtent = #(cons -1.2 1.2)

	\name LyricVoice ;
	\consists "Separating_line_group_engraver";
	\consists "Lyric_engraver";
	\consists "Extender_engraver";
	\consists "Hyphen_engraver";
	\consists "Stanza_number_engraver";
	phrasingPunctuation = #".,;:!?\""
	
}
NoteNamesContext = \translator {
	\type "Engraver_group_engraver";
	\name NoteNames;
	\consistsend "Axis_group_engraver";
	\consists "Note_name_engraver";
	\consists "Separating_line_group_engraver";
}

LyricsContext = \translator {
	\type "Engraver_group_engraver";
	\name Lyrics;
	\consists Vertical_align_engraver; %need this for getting folded repeats right.
	Generic_property_list = #generic-lyrics-properties
	\consists "Property_engraver";
	\consistsend "Axis_group_engraver";
	
	\accepts "LyricVoice";
}

ChordNameVoiceContext = \translator {
	\type "Engraver_group_engraver";
	\name ChordNameVoice ;

	\consists "Output_property_engraver";	
	\consistsend "Axis_group_engraver";
	\consists "Separating_line_group_engraver";
	\consists "Chord_name_engraver";
}
ChordNameContext = \translator {
	\type "Engraver_group_engraver";
	\name ChordNames;

	Generic_property_list = #generic-chord-staff-properties
	\consists "Property_engraver";	
	\consists "Output_property_engraver";	
	\accepts "ChordNameVoice";
	\consistsend "Axis_group_engraver";
	}


ScoreWithNumbers = \translator {
 	\type "Score_engraver";

	% uncomment to bar numbers on a whole system.
	\consists "Bar_number_engraver";
}

StupidScore = \translator {
 	\type "Score_engraver";
	\name Score;
	\consists "Note_heads_engraver";
}



BarNumberingStaffContext = \translator {
	\StaffContext
	\consists "Mark_engraver";
}

HaraKiriStaffContext = \translator {
	\StaffContext
	\remove "Axis_group_engraver";
	\consistsend "Hara_kiri_engraver";	  
	\consists "Instrument_name_engraver";
	\accepts "Voice";
}
%{
  The HaraKiriStaffContexts doesn't override \name,
  so it is still named `Staff'.

  %\translator { \HaraKiriStaffContext }
%}

OrchestralPartStaffContext = \translator {
	\StaffContext
	\consists "Mark_engraver";
}

ScoreContext = \translator {
	\type Score_engraver;
	\name Score;
	

	\consists "Repeat_acknowledge_engraver";
	\consists "Timing_engraver";
	\consists "Output_property_engraver";	
	\consists "System_start_delimiter_engraver";
	\consists "Mark_engraver";	
	\consists "Break_align_engraver";
	\consists "Spacing_engraver";
	\consists "Vertical_align_engraver";

	\consists "Lyric_phrasing_engraver";
	\consists "Bar_number_engraver";
	\consists "Span_arpeggio_engraver";

	
	\accepts "Staff";
	\accepts "StaffGroup";
	\accepts "RhythmicStaff";	
	\accepts "Lyrics";
	\accepts "ChordNames";
	\accepts "GrandStaff";
	\accepts "ChoirStaff";
	\accepts "PianoStaff";
	\accepts "NoteNames";

	soloText = #"Solo"
	soloIIText = #"Solo II"
	aDueText = #"\\`a2"
	soloADue = ##t
	splitInterval = #'(0 . 1)
	changeMoment = #`(,(make-moment 0 0) . ,(make-moment 1 512))

	defaultClef = #"treble"

	StaffMinimumVerticalExtent = #(cons -4.0 4.0)

	barAuto = ##t
	voltaVisibility = ##t
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
	  	

        automaticPhrasing = ##t;
	alignmentReference = \down;
	defaultClef = #"treble"
	defaultBarType = #"|"
	systemStartDelimiterGlyph = #'bar-line
	explicitClefVisibility = #all-visible
	explicitKeySignatureVisibility = #all-visible
	
	scriptDefinitions = #default-script-alist

	startSustain = #"Ped."
	stopSustain = #"*"
	stopStartSustain = #"*Ped."
	startUnaChorda = #"una chorda"
	stopUnaChorda = #"tre chorde"
	% should make separate lists for stopsustain and startsustain 


       %
       % what order to print accs.  We could compute this, 
       % but computing is more work than putting it here.
       %
       % Flats come first, then sharps.
       keyAccidentalOrder = #'(
         (6 . -1) (2  . -1) (5 . -1 ) (1  . -1) (4  . -1) (0  . -1) (3  . -1)
	 (3  . 1) (0 . 1) (4 . 1) (1 . 1) (5 . 1) (2 . 1) (6 . 1)
       )
	breakAlignOrder = #'(
	  Instrument_name
	  Left_edge_item
	  Span_bar
	  Breathing_sign
	  Clef_item
	  Key_item
	  Staff_bar
	  Time_signature
	  Stanza_number
	)
       
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% TODO: uniform naming.;  
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


	%
	% distances are given in stafflinethickness (thicknesses) and
	% staffspace (distances)
	%
	Arpeggio = #`(
 		(interfaces . (arpeggio-interface))
 		(molecule-callback . ,Arpeggio::brew_molecule)
 		(name . "arpeggio") 
 	)
	BarLine = #`(
		(interfaces . (bar-interface staff-bar-interface))
		(break-align-symbol . Staff_bar)
		(glyph . "|")
		(break-glyph-function . ,default-break-barline)
		(barsize-procedure . ,Bar::get_staff_bar_size)
		(molecule-callback . ,Bar::brew_molecule)	   
		(visibility-lambda . ,all-visible)
		(breakable . #t)
		(before-line-breaking-callback . ,Bar::before_line_breaking)
		;;
		;; Ross. page 151 lists other values, we opt for a leaner look
		;; 
		(kern . 3.0)
		(thin-kern . 3.0)
		(hair-thickness . 1.6)
		(thick-thickness . 6.0)
		(name . "BarLine")
	)

	BarNumber = #`(
		(molecule-callback . ,Text_item::brew_molecule)
		(breakable . #t)
		(visibility-lambda . ,begin-of-line-visible)
		(name . "BarNumber")
		(padding . 1.0)
		(direction . 1)
	)

	Beam = #basic-beam-properties

	BreakAlignment = #`(
		(breakable . #t)
		(interfaces . (break-align-interface))
		(stacking-dir . 1)
		(axes 0)
		(X-offset-callbacks . (,Break_align_interface::self_align_callback))
		(space-alist . ,default-break-align-space-alist) 
		(name . "BreakAlignment")		
	)

	BreakAlignGroup = #`(
		(interfaces . (axis-group-interface))
		(axes  . (0))
		(X-offset-callbacks . (,Break_align_interface::alignment_callback))
		
		(name . "BreakAlignGroup")
	)

	BreathingSign = #`(
		(interfaces . (breathing-sign-interface))
		(break-align-symbol . Breathing_sign)
		(breakable . #t )
		(molecule-callback . ,Breathing_sign::brew_molecule)
		(Y-offset-callbacks . (,Breathing_sign::offset_callback))
		(visibility-lambda . ,begin-of-line-invisible)
		(name . "BreathingSign")
	)

	Clef = #`(
	   (interfaces .  (clef-interface))
   	   (molecule-callback . ,Score_element::brew_molecule)
	   (before-line-breaking-callback . ,Clef::before_line_breaking)
	   (breakable . #t)
	   (break-align-symbol . Clef_item)
	   (visibility-lambda . ,begin-of-line-visible)
	   (Y-offset-callbacks  . (,Staff_symbol_referencer::callback)) 
	   (name . "Clef")
	)

	ChordName = #`(
		(molecule-callback . ,Chord_name::brew_molecule)
		(interfaces . (chord-name-interface))
		(after-line-breaking-callback . ,Chord_name::after_line_breaking)
		(chord-name-function . ,default-chord-name-function)
		(name . "ChordName")  
	)

	NoteCollision = #`(
		(axes 0 1)
		(interfaces . (collision-interface))
		(note-width . 1.65)
		(name . "NoteCollision")
	)

	Crescendo = #`(
		(molecule-callback . ,Crescendo::brew_molecule)
		(interfaces . (crescendo-interface dynamic-interface))
		(thickness . 1.0)
		(shorten-for-letter  .  4.0)
		(height . 0.6666)
		(dash-thickness . 1.2)
		(dash-length . 4.0)
		(name . "Crescendo")
		(self-alignment-Y . 0)
		(Y-offset-callbacks . (,Side_position::aligned_on_self))
	)

	DotColumn = #`(
		(interfaces . (dot-column-interface axis-group-interface ))
		(axes 0 )
		(name . "DotColumn")
	)

	Dots = #`(
		(interfaces . (dot-interface))
		(molecule-callback . ,Dots::brew_molecule)
		(dot-count . 1)
		(staff-position . 0.0)
		(Y-offset-callbacks  . (,Dots::quantised_position_callback ,Staff_symbol_referencer::callback))

		(name . "Dots")		
	)
	
	DynamicText = #`(
		(style . "dynamic")
		(interfaces . (dynamic-interface))
		(Y-offset-callbacks . (,Side_position::aligned_on_self))

		(molecule-callback . ,Text_item::brew_molecule)
		(script-priority . 100)
		(self-alignment-Y . 0)
		(name . "DynamicText")
	)
	
	DynamicLineSpanner = #`(
		(interfaces . (dynamic-interface axis-group-interface side-position-interface))
		(axes . ( 1))
		(padding . 3)
		(minimum-space . 6)
		(name . "dynamic alignment")
	)
	
	LeftEdge = #`(
		(break-align-symbol . Left_edge_item)
		(breakable . #t)
		(name . "LeftEdge")
	)
	
	Fingering = #`(
		(interfaces . (finger-interface text-script-interface text-item-interface side-position-interface))
		(molecule-callback . ,Text_item::brew_molecule)
		(padding . 	3.0)
		(self-alignment-X . 0)
		(name . "Fingering") 
	)

	GraceAlignment = #`(
		(interfaces . (axis-group-interface align-interface))
		(axes . (0))
		(horizontal-space . 1.2)
		(padding . 1.0)
		(before-line-breaking-callback . ,Grace_align_item::before_line_breaking)
		(name . "GraceAlignment")
	)
	
	HaraKiriVerticalGroup = #`(
		(Y-offset-callbacks . (,Hara_kiri_group_spanner::force_hara_kiri_callback))
		(Y-extent-callback . ,Hara_kiri_group_spanner::y_extent)
		(interfaces . (hara-kiri-group-interface))
		(axes 1)
		(name . "HaraKiriVerticalGroup")
	)

	LyricHyphen = #`(
		(interfaces  . (lyric-hyphen-interface))	
		(thickness . 1.0)
		(height . 0.4)
		(minimum-length .  0.5) 
		(molecule-callback . ,Hyphen_spanner::brew_molecule)
		(Y-extent-callback . ,Score_element::point_dimension_callback)
		(name . "LyricHyphen")
	)
	
	InstrumentName = #`(
		(interfaces . (instrument-name-interface))
		(breakable . #t)
		(Y-offset-callbacks . (,Side_position::centered_on_parent))
		(molecule-callback . ,Text_item::brew_molecule)		
		(break-align-symbol . Instrument_name)
		(visibility-lambda . ,begin-of-line-visible)
		(name . "InstrumentName")
	)
	
	KeySignature = #`(
	  (interfaces . (key-item-interface))
  	  (molecule-callback . ,Key_item::brew_molecule)
	  (break-align-symbol . Key_item)
	  (visibility-lambda . ,begin-of-line-visible)
	  (breakable . #t)
	  (name . "KeySignature")
	)
	
	Accidentals = #`(
		(molecule-callback . ,Local_key_item::brew_molecule)
		(left-padding . 0.2)
		(right-padding . 0.4)
		(interfaces . (accidentals-interface))
		(name .  "Accidentals")
	)
	
	LineOfScore = #`(
		(axes . (0 1))
		(interfaces . (axis-group-interface))
		(name .  "LineOfScore")
	)
	
	LyricExtender = #`(
		(interfaces . (lyric-extender-interface))
		(molecule-callback . ,Lyric_extender::brew_molecule)
		(height . 0.8) ; stafflinethickness;
		(right-trim-amount . 0.5)
		(Y-extent-callback . ,Score_element::point_dimension_callback)
		(name . "LyricExtender")
	)
	
	LyricText = #`(
		(interfaces .  (lyric-syllable-interface text-item-interface))
		(molecule-callback . ,Text_item::brew_molecule)
		(X-offset-callbacks . (,Side_position::aligned_on_self))
		(self-alignment-X . 0)
		(non-rhythmic . #t)
		(word-space . 0.6)
		(name . "LyricText") 
	)
	
	RehearsalMark = #`(
	  (interfaces . (mark-interface side-position-interface))
	  (molecule-callback . ,Text_item::brew_molecule)	
	  (breakable . #t)
	  (visibility-lambda . ,end-of-line-invisible)
	  (padding . 4.0)
	  (name . "RehearsalMark")
	)
	
	MultiMeasureRest = #`(
		(interfaces . (multi-measure-rest-interface))
		(spacing-procedure . ,Multi_measure_rest::set_spacing_rods)
		(molecule-callback . ,Multi_measure_rest::brew_molecule)
		(staff-position . 0)
		(expand-limit . 10)
		(padding . 2.0) ; staffspace
		(minimum-width . 12.5) ; staffspace
		(name . "MultiMeasureRest")
	)
	
	NoteColumn = #`(
		(interfaces . (axis-group-interface note-column-interface))
		(axes 0 1)
		(name . "NoteColumn")
	)
	
	NoteHead = #`(
		(interfaces . (note-head-interface rhythmic-head-interface))
		(style . default)
		(molecule-callback . ,Note_head::brew_molecule)
		(Y-offset-callbacks  . (,Staff_symbol_referencer::callback)) 
		(name . "NoteHead")
	)

	NoteName = #`(
		(molecule-callback . ,Text_item::brew_molecule)
		(name . "NoteName")
	)

	OctavateEight  = #`(
		(self-alignment-X . 0)
		(text . "8")
		(visibility-lambda . ,begin-of-line-visible)
		(X-offset-callbacks .
		 (,Side_position::centered_on_parent
		  ,Side_position::aligned_on_self
		  ,Side_position::aligned_side
		  )) 
		(molecule-callback . ,Text_item::brew_molecule)
		(style . "italic")
		(name . "OctavateEight")
	)
	
	PaperColumn = #`(
		(interfaces . (paper-column-interface axis-group-interface))
		(axes 0)
		(rank . -1)
		(name . "PaperColumn")
	)

	Rest = #`(
		(interfaces . (rest-interface rhythmic-head-interface))
		(after-line-breaking-callback . ,Rest::after_line_breaking)
		(molecule-callback . ,Rest::brew_molecule)
		(minimum-beam-collision-distance . 1.5)
		(name . "Rest")
	)
	
	RestCollision = #`(
		(interfaces . (rest-collision-interface))
		(minimum-distance . 0.75)
		(name . "RestCollision")		
	)

	Script = #`(
		(molecule-callback . ,Script::brew_molecule)
		(interfaces . (script-interface side-position-interface))
		(X-offset-callbacks . (,Side_position::centered_on_parent))
		(name . "Script")
	)
	
	ScriptColumn = #`(
		(before-line-breaking-callback . ,Script_column::before_line_breaking)
		(name . "ScriptColumn")
	)
	
	Slur = #default-basic-slur-properties

	SpacingSpanner =#`(
		(spacing-procedure . ,Spacing_spanner::set_springs)

		;; assume that notes at least this long are present.
		(maximum-duration-for-spacing . ,(make-moment 1 8))
		(name . "SpacingSpanner")
	)
	SpanArpeggio = #`(
		(interfaces . (span-arpeggio-interface))
		(molecule-callback . ,Span_arpeggio::brew_molecule)
		(name . "SpanArpeggio") 
	)
	SpanBar = #`(
		(interfaces . (bar-interface span-bar-interface))
		(break-align-symbol . Staff_bar)
		(barsize-procedure . ,Span_bar::get_bar_size) 
		(molecule-callback . ,Bar::brew_molecule)
		(visibility-lambda . ,begin-of-line-invisible)
		(X-extent-callback . ,Span_bar::width_callback)
		(Y-offset-callbacks . (,Span_bar::center_on_spanned_callback))
		
		(breakable . #t)
		(glyph . "|")
		(before-line-breaking-callback . ,Span_bar::before_line_breaking)
		;; ugh duplication! 
		
		;;
		;; Ross. page 151 lists other values, we opt for a leaner look
		;; 
		(kern . 3.0)
		(thin-kern . 3.0)
		(hair-thickness . 1.6)
		(thick-thickness . 6.0)
		(name . "SpanBar")
	)

	StanzaNumber = #`(
		(breakable . #t)
		(molecule-callback . ,Text_item::brew_molecule)		
		(break-align-symbol . Clef_item)
		(visibility-lambda . ,begin-of-line-visible)
		(name . "StanzaNumber")
	)

	StaffSymbol = #`(
		(interfaces . (staff-symbol-interface ))
		(molecule-callback . ,Staff_symbol::brew_molecule)
		(staff-space . 1.0)
		(line-count . 5 )
		(name . "StaffSymbol")
	)

	SystemStartDelimiter = #`(
		(molecule-callback . ,System_start_delimiter::brew_molecule)
		(after-line-breaking-callback . ,System_start_delimiter::after_line_breaking)
		(collapse-height . 1.0)
		(thickness . 1.6)
		(arch-height . 1.5)
		(arch-angle . 50.0)
		(arch-thick . 0.25)
		(arch-width . 1.5)
		(bracket-thick . 0.25)
		(bracket-width . 2.0)
		(name . "SystemStartDelimiter")
	)

	TextScript = #`(
		(molecule-callback . ,Text_item::brew_molecule)
		(no-spacing-rods . #t)
		(interfaces . (text-script-interface text-item-interface side-position-interface))
		(padding . 	3.0)
		(name . "TextScript") 
	)

	Tie = #`(
		(interfaces . (tie-interface))
		(molecule-callback . ,Tie::brew_molecule)
		(spacing-procedure . ,Tie::set_spacing_rods)
		(staffline-clearance . 0.24)
		(details . ((ratio . 0.333) (height-limit . 1.0)))
		(thickness . 1.2)
		(x-gap . 0.2)
		(minimum-length  . 2.5)
		(name . "Tie")
	)

	TieColumn = #`(
		(after-line-breaking-callback . ,Tie_column::after_line_breaking)
		(interfaces . (tie-column-interface))
		(name . "TieColumn")		
	)

	TimeSignature = #`(
		(interfaces . (time-signature-interface))
		(molecule-callback . ,Time_signature::brew_molecule)
		(break-align-symbol . Time_signature)
		(visibility-lambda . ,all-visible)
		(breakable . #t)
		(name . "TimeSignature")
	)

	TupletBracket = #`(
		(number-gap . 2.0)   
		(delta-y . 0)
		(thick . 1.0)
		(after-line-breaking-callback . ,Tuplet_spanner::after_line_breaking)
		(molecule-callback . ,Tuplet_spanner::brew_molecule)
		(interfaces . (tuplet-spanner-interface))
		(name "TupletBracket")
	)	

	SostenutoPedal = #`(
		(molecule-callback . ,Text_item::brew_molecule)
		(X-offset-callbacks . (,Side_position::aligned_on_self))
		(Y-offset-callbacks .
		 (,Side_position::aligned_side
		  ,Side_position::centered_on_parent))

		(style . "italic")
		(no-spacing-rods . #t)
		(self-alignment-X . 0)
		(name  . "SostenutoPedal")
				
	)

	Stem = #`(
		(interfaces . (stem-interface))
		(before-line-breaking-callback . ,Stem::before_line_breaking)
		(molecule-callback . ,Stem::brew_molecule)
		(thickness . 0.8)
		(beamed-lengths . (0.0 2.5 2.0 1.5))
		(beamed-minimum-lengths . (0.0 1.5 1.25 1.0))
		
;;  Stems in unnatural (forced) direction should be shortened,
;;  according to [Roush & Gourlay].  Their suggestion to knock off
;;  a whole staffspace seems a bit drastical: we'll do half.

		(lengths . (3.5 3.5 3.5 4.5 5.0))
		(stem-shorten . (0.5))
		; if stem is on middle line, choose this direction.
		(default-neutral-direction . 1)
		(X-offset-callbacks . (,Stem::off_callback))		
		(name . "Stem")
	)

	StemTremolo = #`(
	   	(molecule-callback . ,Stem_tremolo::brew_molecule)
		(beam-width . 2.0) ; staff-space
		(beam-thickness . 0.42) ; staff-space
		(beam-space-function . ,default-beam-space-function)
		(name . "StemTremolo")
	)

	SeparationItem = #`(
		(interfaces . (separation-item-interface))
		(name . "SeparationItem")
	)

	SeparatingGroupSpanner = #`(
		(interfaces . (separation-spanner-interface))
		(spacing-procedure . ,Separating_group_spanner::set_spacing_rods)
		(name . "SeparatingGroupSpanner")
	)

	SustainPedal = #`(
		(interfaces . (sustain-pedal-interface  side-position-interface))
		(no-spacing-rods . #t)
		(molecule-callback . ,Sustain_pedal::brew_molecule)
		(self-alignment-X . 0)
		(X-offset-callbacks . (,Side_position::aligned_on_self))
		(Y-offset-callbacks .
		 (,Side_position::aligned_side
		  ,Side_position::centered_on_parent))

		(name . "SustainPedal")		
	)	

	UnaChordaPdeal = #`(
		(molecule-callback . ,Text_item::brew_molecule)
		(style . "italic")
		(no-spacing-rods . #t)
		(self-alignment-X . 0)
		(X-offset-callbacks . (,Side_position::aligned_on_self))
		(Y-offset-callbacks .
		 (,Side_position::aligned_side
		  ,Side_position::centered_on_parent))

		(name . "UnaChordaPedal")
	)

	VoltaBracket = #`(
		(molecule-callback . ,Volta_spanner::brew_molecule)
		(interfaces . (volta-spanner-interface side-position-interface))
		(direction . 1)
		(padding . 5)
		(thickness . 1.6)  ;  stafflinethickness
		(height . 2.0) ; staffspace;
		(minimum-space . 25)
		(name . "VoltaBracket")
	)	

	VerticalAlignment = #`(
		(axes 1)
		(interfaces . (align-interface axis-group-interface))
		(Y-extent-callback . ,Axis_group_interface::group_extent_callback)
		(X-extent-callback . #f)
		(stacking-dir . -1)
		(name . "VerticalAlignment")
	)

	VerticalAxisGroup = #`(
		(axes 1)
		(interfaces . (axis-group-interface))
		(name . "VerticalAxisGroup")
	)


	\include "auto-beam-settings.ly";

}

OrchestralScoreContext= \translator {
	\ScoreContext
}

