(define (number-or-string? x) (or (number? x) (string? x)))
(define (engraver-description name description created-elts properties)
  (list name description created-elts properties)
  )

(define (translator-property-description symbol type? description)
  (list symbol type? description)
  )

(define engraver-description-alist
  (list
   (cons
    'Stem_engraver
    (engraver-description
     "Stem_engraver"
     "Create stems and single-stem tremolos"
     '(Stem StemTremolo)
     (list
      (translator-property-description 'tremoloFlags integer? "")
      (translator-property-description 'stemLeftBeamCount integer? "
Specify the number of beams to draw on the left side of the next note.
Overrides automatic beaming.  The value is only used once, and then it
is erased.
")
      (translator-property-description 'stemRightBeamCount integer? "idem, for the right side")    
      )))
   
   (cons
    'Hyphen_engraver
    (engraver-description
     "Hyphen_engraver"
     "Create lyric hyphens"
     '(LyricHyphen)
     (list
      )))

   (cons
    'Extender_engraver
    (engraver-description
     "Extender_engraver"
     "Create lyric extenders"
     '(LyricExtender)
     (list
      )))

   
   (cons
    'Separating_line_group_engraver
    (engraver-description
     "Separating_line_group_engraver"
     "Generates objects for computing spacing parameters."
     '(SeparationItem SeparatingGroupSpanner)
     (list
      )))

   (cons
    'Axis_group_engraver
    (engraver-description
     "Axis_group_engraver"
     "Group all objects created in this context in a VerticalAxisGroup spanner."
     '(VerticalAxisGroup)
     (list
      (translator-property-description
       'CONTEXTNAMEVerticalExtent number-pair? "hard coded vertical extent.
The format is a pair of dimensions, for example, this sets the sizes
of a staff to 10 (5+5) staffspaces high.

@example
\property Staff.StaffVerticalExtent = #(-5.0 . 5.0)
@end example

 [fixme, naming]")
      (translator-property-description
       'CONTEXTNAMEMinimumVerticalExtent number-pair?
       "minimum vertical extent, same format as CONTEXTNAMEVerticalExtent [fixme, naming]")
      (translator-property-description
       'CONTEXTNAMExtraVerticalExtent number-pair?
       "extra vertical extent, same format
CONTEXTNAMEMinimumVerticalExtent [fixme, naming]") )))

   (cons
    'Hara_kiri_engraver
    (engraver-description
     "Hara_kiri_engraver"
     "Like Axis_group_engraver, but make a hara kiri spanner, and add
interesting items (ie. note heads, lyric syllables and normal rests)"
     '(HaraKiriVerticalGroup)
     '()
     ))

   
   (cons
    'Local_key_engraver
    (engraver-description
     "Local_key_engraver"
     "Make accidentals.  Catches note heads, ties and notices key-change
   events.  Due to interaction with ties (which don't come together
   with note heads), this needs to be in a context higher than Tie_engraver.
   (FIXME)."
     '(Accidentals)
     (list
      (translator-property-description 'localKeySignature list? "the key signature at this point  in the measure")
      (translator-property-description 'forgetAccidentals boolean? "do
not set localKeySignature when a note alterated differently from
localKeySignature is found.

Causes accidentals to be printed at every note instead of
remembered for the duration of a measure.
")
      (translator-property-description 'noResetKey boolean? "Do not
reset local key to the value of keySignature at the start of a measure,
as determined by measurePosition.

Do not reset the key at the start of a measure.  Accidentals will be
printed only once and are in effect until overridden, possibly many
measures later.
")
      
      )))

   
   (cons
    'Volta_engraver
    (engraver-description
     "Volta_engraver"
     "Make volta brackets"
     '(VoltaBracket)
     (list
      (translator-property-description 'repeatCommands list?
				       "This property is read to find any command of the form (volta . X), where X is a string or #f")
      (translator-property-description 'voltaSpannerDuration moment?
				       "maximum duration of the volta bracket.

    Set to a duration to control the size of the brackets printed by
@code{\alternative}.  It specifies the number of whole notes duration
to use for the brackets.  This can be used to shrink the length of
brackets in the situation where one alternative is very large.  It may
have odd effects if the specified duration is longer than the music
given in an @code{\alternative}.
")
      )
     ))

   (cons
    'Clef_engraver
    (engraver-description
     "Clef_engraver"
     "Determine and set reference point for pitches"
     '(Clef OctavateEight)
     (list
      (translator-property-description 'clefPosition number? "Where should the center of the symbol go?")
      (translator-property-description 'clefGlyph string? "Name of the symbol within the music font")
      (translator-property-description 'centralCPosition number? "Place of the central C. Usually determined by looking at clefPosition and clefGlyph.")
      (translator-property-description 'clefOctavation integer? "Add
this much extra octavation. Values of 7 and -7 are common.")
      (translator-property-description 'explicitClefVisibility procedure? "visibility-lambda function for clef changes.")
      (translator-property-description 'clefPitches list? "an alist mapping GLYPHNAME to the position of the central C for that symbol")

      )))
   
   (cons
    'A2_engraver
    (engraver-description
     "A2_engraver"
     "Part combine engraver for orchestral scores."
     '(TextScript)
     (list
      (translator-property-description 'combineParts boolean? "try to combine parts?")
      (translator-property-description 'soloADue  boolean? "set Solo/A due texts?")
      (translator-property-description 'soloText  string? "text for begin of solo")
      (translator-property-description 'soloIIText  string? "text for begin of solo for voice ``two''")
      (translator-property-description 'aDueText string? "text for begin of a due")
      (translator-property-description 'split-interval number-pair? "always split into two voices for contained intervals")
      (translator-property-description 'unison  boolean? "set if unisono is detected  ")
      (translator-property-description 'solo  boolean? "set if solo is detected")
      (translator-property-description 'unisilence  boolean? "set if unisilence is detected")
      (translator-property-description 'unirhythm  boolean? "set if unirhythm is detected")
      )))

   (cons
    'Align_note_column_engraver
    (engraver-description
     "Align_note_column_engraver"
     "Generate object to put grace notes from left to right."
     '(GraceAlignment)
     (list
      
      (translator-property-description 'graceAlignPosition dir? "put the grace note before or after the main note?")
      (translator-property-description 'graceAccidentalSpace number? "amount space to alot for an accidental")
      )))
   
   (cons
    'Arpeggio_engraver
    (engraver-description
     "Arpeggio_engraver"
     "Generate an Arpeggio from a Arpeggio_req"
     '(Arpeggio)
     (list
      )))

   (cons
    'Auto_beam_engraver
    (engraver-description
     "Auto_beam_engraver"
     "Generate beams based on measure characteristics and observed Stems.
Uses beatLength, measureLength and measurePosition to decide when to start and stop a beam.
"
     '(
       Beam)
     (list
      (translator-property-description 'noAutoBeaming boolean? "  If set to true then beams are not generated automatically.
")
      (translator-property-description 'autoBeamSettings list? "
Specifies when automatically generated beams should begin and end.  The elements have the format:

@example

   function shortest-duration-in-beam time-signature

where

    function = begin or end
    shortest-duration-in-beam = numerator denominator; eg: 1 16
    time-signature = numerator denominator, eg: 4 4

unspecified or wildcard entries for duration or time-signature
are given by * *

The user can override beam begin or end time by pushing a wildcard entries
'(begin * * * *) or '(end * * * *) resp., eg:

    \property Voice.autoBeamSettings \push #'(end * * * *) = #(make-moment 1 4)

The head of the list:
    '(
     ((end * * 3 2) . ,(make-moment 1 2))
     ((end 1 16 3 2) . ,(make-moment 1 4))
     ((end 1 32 3 2) . ,(make-moment 1 8))
     ...
    )

@end example"))))

   (cons
    'Bar_engraver
    (engraver-description
     "Bar_engraver"
     "Create barlines. This engraver is controlled through the
@code{whichBar} property. If it has no bar line to create, it will forbid a linebreak at this point"
     '(BarLine)
     (list
      (translator-property-description 'whichBar string? "This property is read to determine what type of barline to create.
Example:
@example
\\property Staff.whichBar = \"|:\"
@end example
will create a start-repeat bar in this staff only 
")
      (translator-property-description 'staffsFound list? "list of all staff-symbols found.")
      )))


   (cons
    'Bar_number_engraver
    (engraver-description
     "Bar_number_engraver"
     "A bar number is created whenever measurePosition is zero. It is
put on top of all staffs, and appears only at  left side of the staff."
     '(BarNumber)
     (list
      (translator-property-description 'currentBarNumber integer? "this is read to determine
 the number to put on the bar ")
      )))


   (cons
    'Beam_engraver
    (engraver-description
     "Beam_engraver"
     "Handles Beam_requests by engraving Beams.    If omitted, then notes will be
    printed with flags instead of beams."
     '(Beam)
     (list
      (translator-property-description 'beamMelismaBusy boolean? "Signal if a beam is set when automaticMelismata is set")
      )))

   (cons
    'Break_align_engraver
    (engraver-description
     "Break_align_engraver"
     "Align graphic elements with corresponding break-align-symbols into groups, and order the groups according to breakAlignOrder"
     '(BreakAlignment BreakAlignGroup LeftEdge)
     (list
      (translator-property-description 'breakAlignOrder list?
				       "Defines the order in which
prefatory matter (clefs, key signatures) appears, eg. this puts the
key signatures after the bar lines:

@example
	\\property Score.breakAlignOrder = #'(
	  Span_bar
	  Breathing_sign
	  Clef_item
	  Staff_bar
	  Key_item
	  Time_signature
	)
@end example
")
      )))


   (cons
    'Breathing_sign_engraver
    (engraver-description
     "Breathing_sign_engraver"
     ""
     '(BreathingSign)
     (list
      )))


   (cons
    'Chord_name_engraver
    (engraver-description
     "Chord_name_engraver"
     "Catch Note_req's, Tonic_reqs, Inversion_reqs, Bass_req
and generate the appropriate chordname."
     '(ChordName)
     (list
      (translator-property-description 'chordInversion boolean? " Determines whether LilyPond should look for chord inversions when
    translating from notes to chord names.  Set to 1 to find
    inversions.  The default is 0 which does not look for
    inversions.")
      (translator-property-description 'drarnChords boolean? "")
      )))


   (cons
    'Chord_tremolo_engraver
    (engraver-description
     "Chord_tremolo_engraver"
     "Generates beams for the \repeat X tremolo ... construct"
     '(Beam)
     (list
      )))



   (cons
    'Collision_engraver
    (engraver-description
     "Collision_engraver"
     ""
     '(NoteCollision
       )
     (list
      )))

   (cons
    'Custos_engraver
    (engraver-description
     "Custos_engraver"
     ""
     '(Custos)
     (list
      )))


   (cons
    'Dot_column_engraver
    (engraver-description
     "Dot_column_engraver"
     " Engraves dots on dotted notes shifted to the right of the note.
If omitted, then dots appear on top of the notes.
"
     '(DotColumn
       )
     (list
      )))


   (cons
    'Dynamic_engraver
    (engraver-description
     "Dynamic_engraver"
     ""
     '(DynamicLineSpanner
       DynamicText Crescendo
       TextSpanner)
     (list
      )))




   (cons
    'Grace_position_engraver
    (engraver-description
     "Grace_position_engraver"
     "Attach a grace note alignment to a note-column "
     '()
     (list
      )))

   (cons
    'Grace_engraver_group
    (engraver-description
     "Grace_engraver_group"
     "An engraver that creates a `shielded' context-tree with separate notion of time"
     '()
     (list
      )))


   (cons
    'Instrument_name_engraver
    (engraver-description
     "Instrument_name_engraver"
     " Prints the name of the instrument (specified by
    @code{Staff.instrument} and @code{Staff.instr}) at the left of the
    staff."
     '(InstrumentName)
     (list
      (translator-property-description 'instrument string? " If @code{Instrument_name_engraver}
@cindex Instrument_name_engraver
 is
    added to the Staff translator, then the @code{instrument} property
    is used to label the first line of the staff and the @code{instr}
    property is used to label subsequent lines.  If the
    @code{midiInstrument} property is not set, then @code{instrument}
    is used to determine the instrument for MIDI output.")
      (translator-property-description 'instr string? "see @code{instrument}")
      )))

   (cons
    'Engraver_group_engraver
    (engraver-description
     "Engraver_group_engraver"
     "A group of engravers taken together"
     '()
     (list
      )))

   (cons
    'Key_engraver
    (engraver-description
     "Key_engraver"
     ""
     '(KeySignature
       )
     (list
      
      (translator-property-description 'keySignature list? "")
      (translator-property-description 'keyOctaviation boolean? "")
      (translator-property-description 'explicitKeySignatureVisibility procedure? "")
      (translator-property-description 'createKeyOnClefChange boolean? "")
      (translator-property-description 'keyAccidentalOrder list? "")
      (translator-property-description 'keySignature list? "")
      )))

   (cons 'Lyric_engraver
	 (engraver-description
	  "Lyric_engraver"
	  ""
	  '()
	  (list
	   ;; FIXME
	   )))

   (cons 'Lyric_phrasing_engraver
	 (engraver-description
	  "Lyric_phrasing_engraver"
	  ""
	  '()
	  (list
	   (translator-property-description 'automaticPhrasing boolean? "")
	   (translator-property-description 'weAreGraceContext boolean? "")
	   (translator-property-description 'melismaEngraverBusy boolean? "")
	   (translator-property-description 'associatedVoice string? "")
	   (translator-property-description 'phrasingPunctuation string? "")
	   )))

   (cons
    'Mark_engraver
    (engraver-description
     "Mark_engraver"
     ""
     '(RehearsalMark)
     (list

      (translator-property-description 'rehearsalMark number-or-string? "")
      (translator-property-description 'staffsFound list? "")
      )))


   (cons
    'Melisma_engraver
    (engraver-description
     "Melisma_engraver"
     ""
     '()
     (list

      (translator-property-description 'melismaBusy boolean? "")
      (translator-property-description 'slurMelismaBusy boolean? "")
      (translator-property-description 'tieMelismaBusy boolean? "")
      (translator-property-description 'beamMelismaBusy boolean? "")
      )))


   (cons
    'Multi_measure_rest_engraver
    (engraver-description
     "Multi_measure_rest_engraver"
     "Engraves multi-measure rests that are produced with @code{R}.  Reads
measurePosition and currentBarNumber to determine what number to print over the MultiMeasureRest
   "
     '(MultiMeasureRest)
     (list
      )))


   (cons
    'Note_heads_engraver
    (engraver-description
     "Note_heads_engraver"
     "Generate one or more noteheads from Music of type Note_req."
     '(NoteHead Dots)
     (list
      )))


   (cons
    'Note_name_engraver
    (engraver-description
     "Note_name_engraver"
     ""
     '(NoteName)
     (list
      )))


   (cons
    'Output_property_engraver
    (engraver-description
     "Output_property_engraver"
     "Interpret Music of Output_property type, and apply a function
to any Graphic objects that satisfies the predicate."
     '()
     (list
      )))


   (cons
    'Piano_pedal_engraver
    (engraver-description
     "Piano_pedal_engraver"
     "engrave Piano pedals symbols."
     '(SostenutoPedal SustainPedal UnaChordaPedal)
     (list
      
      	(translator-property-description 'startSustain string? "")
	(translator-property-description 'stopSustain  string? "")
	(translator-property-description 'stopStartSustain  string? "")
	(translator-property-description 'startUnaChorda  string? "")
	(translator-property-description 'stopUnaChorda string? "")
      )))

   (cons 
    'Pitch_squash_engraver
    (engraver-description
     "Pitch_squash_engraver"
     "Treat all pitches as middle C.  Note that the notes move, but
the locations of accidentals stay the same. 
Set the position field of all note heads to zero. This useful for
making a single line staff that demonstrates the rhythm of a melody."
     '()
     (list
      (translator-property-description 'squashedPosition integer? " Vertical position of
squashing.")
      )))
   
   (cons
    'Property_engraver
    (engraver-description
     "Property_engraver"
"This is a engraver that converts \property settings into
back-end element-property settings. Example: Voice.stemLength will set
#'length in all Stem objects.

Due to CPU and memory requirements, the use of this engraver is deprecated."
     '()
     (list
      (translator-property-description 'Generic_property_list list? "description of the conversion.

Defines names and types for generic properties. These are properties
than can be plugged into the backend directly. See the init file
@file{generic-property.scm} for details.  For internal use only,
deprecated.
")
      )))


   (cons
    'Repeat_acknowledge_engraver
    (engraver-description
     "Repeat_acknowledge_engraver"
     
     "Acknowledge repeated music, and convert the contents of
repeatCommands ainto an appropriate setting for whichBar"
     '()
     (list
      (translator-property-description 'repeatCommands list? "")
      (translator-property-description 'whichBar string? "")
 
      )))


   (cons
    'Rest_collision_engraver
    (engraver-description
     "Rest_collision_engraver"
     "Handles collisions of rests."
     '(RestCollision)
     (list
      )))


   (cons
    'Rest_engraver
    (engraver-description
     "Rest_engraver"
     ""
      '(Rest Dots)
   (list
      )))


   (cons
    'Rhythmic_column_engraver
    (engraver-description
     "Rhythmic_column_engraver"
     "Generates NoteColumn, an objects that groups stems, noteheads and rests."
     '(NoteColumn)
     (list
      )))


   (cons
    'Script_column_engraver
    (engraver-description
     "Script_column_engraver"
     ""
     '(ScriptColumn)
     (list
      )))


   (cons
    'Script_engraver
    (engraver-description
     "Script_engraver"
     "    Handles note ornaments generated by @code{\script}.  
"
     '(Script)
     (list
      (translator-property-description 'scriptDefinitions list? "
Description of scripts to use.  (fixme) 
")

      (translator-property-description 'scriptHorizontal boolean? "    Put scripts left or right of note heads.  Support for this is
    limited.  Accidentals will collide with scripts.
    
")
      )))

   (cons
    'Score_engraver
    (engraver-description
     "Score_engraver"
     "Top level engraver. Takes care of generating columns and the complete  system (ie. LineOfScore)

This engraver decides whether a column is breakable. The default is
that a column is always breakable. However, when every Bar_engraver
that does not have a barline at a certain point will call
Score_engraver::forbid_breaks to stop linebreaks.  In practice, this
means that you can make a breakpoint by creating a barline (assuming
that there are no beams or notes that prevent a breakpoint.)

"
     '(LineOfScore PaperColumn NonMusicalPaperColumn)
     (list
      (translator-property-description 'currentMusicalColumn ly-element? "")
      (translator-property-description 'currentCommandColumn ly-element? "")
      )))
   
   (cons 'Skip_req_swallow_translator
	 (engraver-description
	  "Skip_req_swallow_translator"
	  ""
	  '()
	  (list
	   ;; FIXME
	   )))

   (cons
    'Slur_engraver
    (engraver-description
     "Slur_engraver"
     "Build slurs from Slur_reqs"
     '(Slur)

     (list
      (translator-property-description 'slurBeginAttachment symbol? "translates to the car of Slur.element-property 'attachment.")
      (translator-property-description 'slurEndAttachment symbol? "translates to the cdr of Slur.element-property 'attachment.")
      (translator-property-description 'slurMelismaBusy boolean? "Signal a slur if automaticMelismata is set")
      )))


   (cons
    'Spacing_engraver
    (engraver-description
     "Spacing_engraver"
     "make a SpacingSpanner and do bookkeeping of shortest starting and playing notes  "
     '(SpacingSpanner)
     (list
      )))


   (cons
    'Span_arpeggio_engraver
    (engraver-description
     "Span_arpeggio_engraver"
     ""
     '(Arpeggio)
     (list
      (translator-property-description 'connectArpeggios boolean? " If
set, connect all arpeggios that are found.  In this way, you can make
arpeggios that cross staffs.
")
      )))


   (cons
    'Span_bar_engraver
    (engraver-description
     "Span_bar_engraver"
     "This engraver makes cross-staff barlines: It catches all normal
bar lines, and draws a single span-bar across them."

     '(SpanBar)
     (list
      )))


   (cons
    'Staff_symbol_engraver
    (engraver-description
     "Staff_symbol_engraver"
     "create the constellation of five (default) staff lines."
     '(StaffSymbol)
     (list
      )))


   (cons
    'Stanza_number_engraver
    (engraver-description
     "Stanza_number_engraver"
     ""
     '(StanzaNumber
       )
     (list
      (translator-property-description 'stz string? "")
      (translator-property-description 'stanza string? "Stanza `number' to print at start of a verse")
      )))



   (cons
    'System_start_delimiter_engraver
    (engraver-description
     "System_start_delimiter_engraver"
     "creates a SystemStartDelimiter spanner"
     '(SystemStartDelimiter)
     (list
      )))


   (cons
    'Text_engraver
    (engraver-description
     "Text_engraver"
     "Create text-scripts"
     '(TextScript)
     (list
      (translator-property-description 'scriptHorizontal boolean? "    Put scripts left or right of note heads.  Support for this is
    limited.  Accidentals will collide with scripts.
    
")
      (translator-property-description 'textNonEmpty boolean? " If set
to true then text placed above or below the staff is not assumed to
have zero width.  @code{fatText} and @code{emptyText} are predefined
settings.
")
      )))


   (cons
    'Text_spanner_engraver
    (engraver-description
     "Text_spanner_engraver"
     "Create text spanner from a  Span_req "
     '(TextSpanner)
     (list
      )))


   (cons
    'Thread_devnull_engraver
    (engraver-description
     "Thread_devnull_engraver"
     "Kill elements whenever we are Voice called `two' and
either unison, unisilence or soloADue is set"
     '()
     '()))


   (cons
    'Tie_engraver
    (engraver-description
     "Tie_engraver"
     "Generate ties between noteheads of equal pitch."
     '(Tie TieColumn)
     (list
      

      (translator-property-description 'sparseTies boolean? "only create one tie per chord.")
      (translator-property-description 'tieMelismaBusy boolean? "Signal ties when automaticMelismata is set")
      )))


   (cons
    'Time_signature_engraver
    (engraver-description
     "Time_signature_engraver"
     "Create a TimeSignature whenever @code{timeSignatureFraction} changes"
     '(TimeSignature)
     (list
      )))


   (cons
    'Timing_engraver
    (engraver-description
     "Timing_engraver"
     " Responsible for synchronizing timing information from staffs. 
    Normally in @code{Score}.  In order to create polyrhythmic music,
    this engraver should be removed from @code{Score} and placed in
    @code{Staff}."
     '()
     (list
      (translator-property-description 'timeSignatureFraction number-pair? "
pair of numbers,  signifying the time signature. For example #'(4 . 4) is a 4/4time signature.")   
      (translator-property-description 'barCheckNoSynchronize boolean?
"If set, don't reset measurePosition when finding a bbarcheck. This
makes bar-checks for polyphonic music easier.")

      (translator-property-description 'barNonAuto boolean? " If set to true then bar lines will not be printed
    automatically; they must be explicitly created with @code{\bar}
    keywords.  Unlike with the @code{\cadenza} keyword, measures are
    still counted.  Bar generation will resume according to that
    count if this property is set to zero.
")
      (translator-property-description 'whichBar string? "if not set
explicitly (by \property or \bar), this is set according to values of
defaultBarType, barAlways, barNonAuto and measurePosition.
 ")
      
      (translator-property-description 'barAlways boolean? " If set to true a bar line is drawn after each note.
")
      (translator-property-description 'defaultBarType string? "Sets the default type of bar line. See Section XREF-barlines [FIXME] 
    for a list of available bar types.
")
      (translator-property-description 'skipBars boolean? " Set to true to skip the empty bars that are produced by
    multimeasure notes and rests.  These bars will not appear on the
    printed output.  If not set (the default)  multimeasure
    notes and rests expand into their full length, printing the appropriate
    number of empty bars so that synchronization with other voices is
    preserved.

@c my @vebatim patch would help...
@example
@@lilypond[fragment,verbatim,center]
r1 r1*3 R1*3\property Score.skipBars=1 r1*3 R1*3

@@end lilypond
@end example

")
      (translator-property-description 'timing boolean? " Keep administration of measure length, position, bar number, etc?
Switch off for cadenzas.")
      (translator-property-description 'oneBeat moment? "  How long does one beat in the current time signature last?")
      (translator-property-description 'measureLength moment? "  How long does one measure in the current time signature last?")
      (translator-property-description 'measurePosition moment? "
  How much of the current measure (measured in whole notes) have we had?
")
      (translator-property-description 'currentBarNumber integer? "Contains the current barnumber. This property is incremented at
every barline.
")
      )))


   (cons
    'Tuplet_engraver
    (engraver-description
     "Tuplet_engraver"
     "Catch Time_scaled_music and generate appropriate bracket  "
     '(
       TupletBracket)
     (list
      (translator-property-description 'tupletSpannerDuration moment? "
Normally a tuplet bracket is as wide as the
@code{\times} expression that gave rise to it. By setting this
property, you can make brackets last shorter. Example

@example
@@lilypond[verbatim,fragment]
\context Voice \times 2/3 @{
  \property Voice.tupletSpannerDuration = #(make-moment 1 4)
  [c8 c c] [c c c]
@}
@@end lilypond
@end example
")
      (translator-property-description 'tupletInvisible boolean? "
    If set to true, tuplet bracket creation is switched off
entirely. This has the same effect as setting both
@code{tupletNumberVisibility} and @code{tupletBracketVisibility} to
@code{#f}, but as this does not even create elements, this setting
uses less memory and time.")
      )))


   (cons
    'Vertical_align_engraver
    (engraver-description
     "Vertical_align_engraver"
     "Catch Vertical axis groups and stack them."
     '(VerticalAlignment)
     (list
      )))


   (cons
    'Voice_devnull_engraver
    (engraver-description
     "Voice_devnull_engraver"
     "Kill off certain items and spanners if we're Voice `two' and unison or unisilence is set."
     '()
     (list
      )))
   ))




(define context-description-alist
  '(
(Grace . "
    The context for handling grace notes.  It is instantiated
    automatically when you use @code{\grace}.  Basically, it is an
    `embedded' miniature of the Score context.  Since this context
    needs special interaction with the rest of LilyPond, you should
    not explicitly instantiate it.
")
(LyricVoice . "
    Corresponds to a voice with lyrics.  Handles the printing of a
    single line of lyrics.
")
(Thread . "
    Handles note heads, and is contained in the Voice context.  You
    have to instantiate this explicitly if you want to adjust the
    style of individual note heads.
")
(Voice . "
    Corresponds to a voice on a staff.  This context handles the
    conversion of dynamic signs, stems, beams, super- and subscripts,
    slurs, ties, and rests.

    You have to instantiate this explicitly if you want to have
    multiple voices on the same staff.")

(ChordNamesVoice . "
    A voice with chord names.  Handles printing of a line of chord
    names.")

(ChordNames . "
    Typesets chord names.  Can contain @code{ChordNamesVoice}
    contexts.")

(Lyrics . "
    Typesets lyrics.  It can contain @code{LyricVoice} contexts.
")
(Staff . "
    Handles clefs, bar lines, keys, accidentals.  It can contain
    @code{Voice} contexts.
")
(RhythmicStaff . "
    A context like @code{Staff} but for printing rhythms.  Pitches are
    ignored; the notes are printed on one line.  It can contain
    @code{Voice} contexts.
")
(GrandStaff . "
    Contains @code{Staff} or @code{RhythmicStaff} contexts.  It adds a
    brace on the left side, grouping the staffs together.  The bar
    lines of the contained staffs are connected vertically.  It can
    contain @code{Staff} contexts.")

(PianoStaff . "
    Just like @code{GrandStaff} but with @code{minVerticalAlign} set
    equal to @code{maxVerticalAlign} so that interstaff beaming and
    slurring can be used.")

(StaffGroup . "
    Contains @code{Staff} or @code{RhythmicStaff} contexts.  Adds a
    bracket on the left side, grouping the staffs together.  The bar
    lines of the contained staffs are connected vertically.  It can
    contain @code{Staff}, @code{RhythmicStaff}, @code{GrandStaff}, or
    @code{Lyrics} contexts.
")
(ChoirStaff . "
    Identical to @code{StaffGroup} except that the contained staffs
    are not connected vertically.
")
(Score . "
    This is the top level notation context.  No other context can
    contain a @code{Score} context.  This context handles the
    administration of time signatures.  It also makes sure that items
    such as clefs, time signatures, and key-signatures are aligned
    across staffs.  It can contain @code{Lyrics}, @code{Staff},
    @code{RhythmicStaff}, @code{GrandStaff}, @code{StaffGroup}, and
    @code{ChoirStaff} contexts.

    You cannot explicitly instantiate a Score context (since it is
    not contained in any other context).  It is instantiated
    automatically when an output definition (a @code{\score} or
    @code{\paper} block) is processed.
")
)
)
