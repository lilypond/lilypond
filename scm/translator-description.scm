
(define (engraver-description name description created-elts properties)
  (list name description created-elts properties)
  )


(define engraver-description-alist
  (list
   (cons
    'Stem_engraver
    (engraver-description
     "Stem_engraver"
     "Create stems and single-stem tremolos.  It also works together with
the beam engraver for overriding beaming."
     '(Stem StemTremolo)
     '(tremoloFlags
      stemLeftBeamCount
      stemRightBeamCount    
      )))
   
   (cons
    'Hyphen_engraver
    (engraver-description
     "Hyphen_engraver"
     "Create lyric hyphens"
     '(LyricHyphen)
     '(
      )))

   (cons
    'Extender_engraver
    (engraver-description
     "Extender_engraver"
     "Create lyric extenders"
     '(LyricExtender)
     '(
      )))

   
   (cons
    'Separating_line_group_engraver
    (engraver-description
     "Separating_line_group_engraver"
     "Generates objects for computing spacing parameters."
     '(SeparationItem SeparatingGroupSpanner)
     '(
      )))

   (cons
    'Axis_group_engraver
    (engraver-description
     "Axis_group_engraver"
     "Group all objects created in this context in a VerticalAxisGroup spanner."
     '(VerticalAxisGroup)
     '(VerticalExtent MinimumVerticalExtent ExtraVerticalExtent)
     ))

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
     '(
      localKeySignature
      forgetAccidentals
      noResetKey
      
      )))

   
   (cons
    'Volta_engraver
    (engraver-description
     "Volta_engraver"
     "Make volta brackets"
     '(VoltaBracket)
     '(repeatCommands voltaSpannerDuration)
     ))

   (cons
    'Clef_engraver
    (engraver-description
     "Clef_engraver"
     "Determine and set reference point for pitches"
     '(Clef OctavateEight)
     '( clefPosition clefGlyph centralCPosition clefOctavation explicitClefVisibility )))
   
   (cons
    'A2_engraver
    (engraver-description
     "A2_engraver"
     "Part combine engraver for orchestral scores.

The markings @emph{a2}, @emph{Solo} and @emph{Solo II}, are
created by this engraver.  It also acts upon instructions of the part
combiner.  Another thing that the this engraver, is forcing of stem,
slur and tie directions, always when both threads are not identical;
up for the musicexpr called @code{one}, down for the musicexpr called
@code{two}.

"
     '(TextScript)
     '(
      combineParts
      noDirection
      soloADue
      soloText
      soloIIText
      aDueText
      split-interval
      unison
      solo
      unisilence
      unirhythm
      )))

   (cons
    'Arpeggio_engraver
    (engraver-description
     "Arpeggio_engraver"
     "Generate an Arpeggio from a Arpeggio_req"
     '(Arpeggio)
     '(
      )))

   (cons
   'Auto_beam_engraver
    (engraver-description
     "Auto_beam_engraver"
     "Generate beams based on measure characteristics and observed
Stems.  Uses beatLength, measureLength and measurePosition to decide
when to start and stop a beam.  Overriding beaming is done through
@ref{Stem_engraver} properties stemLeftBeamCount and
stemRightBeamCount.
"
     '(
       Beam)
     '(
      noAutoBeaming
      autoBeamSettings)))

   (cons
    'Bar_engraver
    (engraver-description
     "Bar_engraver"
     "Create barlines. This engraver is controlled through the
@code{whichBar} property. If it has no bar line to create, it will forbid a linebreak at this point"
     '(BarLine)
     '(
      whichBar
      stavesFound
      )))


   (cons
    'Bar_number_engraver
    (engraver-description
     "Bar_number_engraver"
     "A bar number is created whenever measurePosition is zero. It is
put on top of all staves, and appears only at  left side of the staff."
     '(BarNumber)
     '(
      currentBarNumber
      )))


   (cons
    'Beam_engraver
    (engraver-description
     "Beam_engraver"
     "Handles Beam_requests by engraving Beams.    If omitted, then notes will be
    printed with flags instead of beams."
     '(Beam)
     '(
      beamMelismaBusy
      )))

   (cons
    'Break_align_engraver
    (engraver-description
     "Break_align_engraver"
     "Align grobs with corresponding break-align-symbols into groups, and order the groups according to breakAlignOrder"
     '(BreakAlignment BreakAlignGroup LeftEdge)
     '(
      breakAlignOrder
      
      )))


   (cons
    'Breathing_sign_engraver
    (engraver-description
     "Breathing_sign_engraver"
     ""
     '(BreathingSign)
     '(
      )))


   (cons
    'Chord_name_engraver
    (engraver-description
     "Chord_name_engraver"
     "Catch Note_req's, Tonic_reqs, Inversion_reqs, Bass_req
and generate the appropriate chordname."
     '(ChordName)
     '(chordChanges)))


   (cons
    'Chord_tremolo_engraver
    (engraver-description
     "Chord_tremolo_engraver"
     "Generates beams for the \repeat X tremolo ... construct"
     '(Beam)
     '(
      )))



   (cons
    'Collision_engraver
    (engraver-description
     "Collision_engraver"
     ""
     '(NoteCollision
       )
     '(
      )))

   (cons
    'Custos_engraver
    (engraver-description
     "Custos_engraver"
     ""
     '(Custos)
     '(
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
     '(
      )))


   (cons
    'Dynamic_engraver
    (engraver-description
     "Dynamic_engraver"
     ""
     '(DynamicLineSpanner
       DynamicText Hairpin
       TextSpanner)
     '(
      )))






   (cons
    'Instrument_name_engraver
    (engraver-description
     "Instrument_name_engraver"
     " Prints the name of the instrument (specified by
    @code{Staff.instrument} and @code{Staff.instr}) at the left of the
    staff."
     '(InstrumentName)
     '(
      instrument
      instr
      )))

   (cons
    'Engraver_group_engraver
    (engraver-description
     "Engraver_group_engraver"
     "A group of engravers taken together"
     '()
     '(
      )))

   (cons
    'Key_engraver
    (engraver-description
     "Key_engraver"
     ""
     '(KeySignature
       )
     '( keySignature explicitKeySignatureVisibility createKeyOnClefChange keyAccidentalOrder keySignature )))

   (cons 'Lyric_engraver
	 (engraver-description
	  "Lyric_engraver"
	  ""
	  '()
	  '(
	   ;; FIXME
	   )))

   (cons 'Lyric_phrasing_engraver
	 (engraver-description
	  "Lyric_phrasing_engraver"
	  ""
	  '()
	  '(
	   automaticPhrasing

	   melismaEngraverBusy
	   associatedVoice
	   phrasingPunctuation
	   )))

   (cons
    'Mark_engraver
    (engraver-description
     "Mark_engraver"
     ""
     '(RehearsalMark)
     '(

      rehearsalMark
      stavesFound
      )))


   (cons
    'Melisma_engraver
    (engraver-description
     "Melisma_engraver"
     ""
     '()
     '(

      melismaBusy
      slurMelismaBusy
      tieMelismaBusy
      beamMelismaBusy
      )))


   (cons
    'Multi_measure_rest_engraver
    (engraver-description
     "Multi_measure_rest_engraver"
     "Engraves multi-measure rests that are produced with @code{R}.  Reads
measurePosition and currentBarNumber to determine what number to print over the MultiMeasureRest
   "
     '(MultiMeasureRest)
     '(currentBarNumber currentCommandColumn measurePosition
      )))

   (cons
    'Note_heads_engraver
    (engraver-description
     "Note_heads_engraver"
     "Generate one or more noteheads from Music of type Note_req."
     '(NoteHead Dots)
     '(
      )))

   (cons
    'Note_head_line_engraver
    (engraver-description
     "Note_head_line_engraver"
     "Engrave a line between two note heads, for example a glissando.
If followVoice is set, staff switches also generate a line."
     '(Glissando VoiceFollower)
     '(followVoice)))

   (cons
    'Note_name_engraver
    (engraver-description
     "Note_name_engraver"
     ""
     '(NoteName)
     '(
      )))


   (cons
    'Output_property_engraver
    (engraver-description
     "Output_property_engraver"
     "Interpret Music of Output_property type, and apply a function
to any Graphic objects that satisfies the predicate."
     '()
     '(
      )))


   (cons
    'Piano_pedal_engraver
    (engraver-description
     "Piano_pedal_engraver"
     "Engrave piano pedal symbols."
     '(SostenutoPedal SustainPedal UnaCordaPedal)
     '(pedalSostenutoStrings pedalSustainStrings pedalUnaCordaStrings
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
     '(
      squashedPosition
      )))
   
   (cons
    'Porrectus_engraver
    (engraver-description
     "Porrectus_engraver"
     "Join adjacent notes to a porrectus ligature."
     '(Porrectus)
     '(
      )))


   (cons
    'Property_engraver
    (engraver-description
     "Property_engraver"
"This is a engraver that converts \property settings into
back-end grob-property settings. Example: Voice.stemLength will set
#'length in all Stem objects.

Due to CPU and memory requirements, the use of this engraver is deprecated."
     '()
     '(Generic_property_list)
      ))


   (cons
    'Repeat_acknowledge_engraver
    (engraver-description
     "Repeat_acknowledge_engraver"
     
     "Acknowledge repeated music, and convert the contents of
repeatCommands ainto an appropriate setting for whichBar"
     '()
     '(
      repeatCommands
      whichBar
 
      )))


   (cons
    'Rest_collision_engraver
    (engraver-description
     "Rest_collision_engraver"
     "Handles collisions of rests."
     '(RestCollision)
     '(
      )))


   (cons
    'Rest_engraver
    (engraver-description
     "Rest_engraver"
     ""
      '(Rest Dots)
   '(
      )))


   (cons
    'Rhythmic_column_engraver
    (engraver-description
     "Rhythmic_column_engraver"
     "Generates NoteColumn, an objects that groups stems, noteheads and rests."
     '(NoteColumn)
     '(
      )))


   (cons
    'Script_column_engraver
    (engraver-description
     "Script_column_engraver"
     ""
     '(ScriptColumn)
     '(
      )))


   (cons
    'Script_engraver
    (engraver-description
     "Script_engraver"
     "    Handles note ornaments generated by @code{\script}.  
"
     '(Script)
     '(
      scriptDefinitions 
      scriptHorizontal
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
     '(
      currentMusicalColumn
      currentCommandColumn
      )))
   
   (cons 'Skip_req_swallow_translator
	 (engraver-description
	  "Skip_req_swallow_translator"
	  ""
	  '()
	  '(
	   ;; FIXME
	   )))

   (cons
    'Slur_engraver
    (engraver-description
     "Slur_engraver"
     "Build slurs from Slur_reqs"
     '(Slur)

     '(
      slurBeginAttachment
      slurEndAttachment
      slurMelismaBusy
      )))


   (cons
    'Spacing_engraver
    (engraver-description
     "Spacing_engraver"
     "make a SpacingSpanner and do bookkeeping of shortest starting and playing notes  "
     '(SpacingSpanner)
     '(
      )))


   (cons
    'Span_arpeggio_engraver
    (engraver-description
     "Span_arpeggio_engraver"
     ""
     '(Arpeggio)
     '(
      connectArpeggios
      )))


   (cons
    'Span_bar_engraver
    (engraver-description
     "Span_bar_engraver"
     "This engraver makes cross-staff barlines: It catches all normal
bar lines, and draws a single span-bar across them."

     '(SpanBar)
     '(
      )))


   (cons
    'Staff_symbol_engraver
    (engraver-description
     "Staff_symbol_engraver"
     "create the constellation of five (default) staff lines."
     '(StaffSymbol)
     '(
      )))


   (cons
    'Stanza_number_engraver
    (engraver-description
     "Stanza_number_engraver"
     ""
     '(StanzaNumber
       )
     '(
      stz
      stanza
      )))



   (cons
    'System_start_delimiter_engraver
    (engraver-description
     "System_start_delimiter_engraver"
     "creates a SystemStartDelimiter spanner"
     '(SystemStartDelimiter)
     '(
      )))


   (cons
    'Text_engraver
    (engraver-description
     "Text_engraver"
     "Create text-scripts"
     '(TextScript)
     '(
      scriptHorizontal
      textNonEmpty
      )))


   (cons
    'Text_spanner_engraver
    (engraver-description
     "Text_spanner_engraver"
     "Create text spanner from a  Span_req "
     '(TextSpanner)
     '(
      )))


   (cons
    'Thread_devnull_engraver
    (engraver-description
     "Thread_devnull_engraver"
     "Kill elements whenever we are Voice called `two' and either
unison, unisilence or soloADue is set.@footnote{On unix systems, the
file @file{/dev/null} is special device: anything written to it is
discarded.}. This engraver works closely together with the part
combiner.  When the part combiner notices that two threads are
identical, it tells the @code{Thread_devnull_engraver} to discard
everything in the second thread.
"

     '()
     '()))


   (cons
    'Tie_engraver
    (engraver-description
     "Tie_engraver"
     "Generate ties between noteheads of equal pitch."
     '(Tie TieColumn)
     '(sparseTies
      tieMelismaBusy
      )))


   (cons
    'Time_signature_engraver
    (engraver-description
     "Time_signature_engraver"
     "Create a TimeSignature whenever @code{timeSignatureFraction} changes"
     '(TimeSignature)
     '(
      )))


   (cons
    'Timing_engraver
    (engraver-description
     "Timing_engraver"
     " Responsible for synchronizing timing information from staves. 
    Normally in @code{Score}.  In order to create polyrhythmic music,
    this engraver should be removed from @code{Score} and placed in
    @code{Staff}."
     '()
     '(
      timeSignatureFraction
      barCheckNoSynchronize
      barNonAuto
      whichBar      
      barAlways
      defaultBarType
      skipBars
      timing
      oneBeat
      measureLength
      measurePosition 
      currentBarNumber
      )))


   (cons
    'Tuplet_engraver
    (engraver-description
     "Tuplet_engraver"
     "Catch Time_scaled_music and generate appropriate bracket  "
     '( TupletBracket)
     '(tupletNumberFormatFunction tupletSpannerDuration tupletInvisible)))


   (cons
    'Vertical_align_engraver
    (engraver-description
     "Vertical_align_engraver"
     "Catch Vertical axis groups and stack them."
     '(VerticalAlignment)
     '(
      )))


   (cons
    'Voice_devnull_engraver
    (engraver-description
     "Voice_devnull_engraver"
     "Kill off certain items and spanners if we're Voice `two' and unison or unisilence is set."
     '()
     '(
      )))
   ))


(set! engraver-description-alist
      (sort engraver-description-alist alist<?))

(define context-description-alist
  '(
(Grace . "
    The context for handling grace notes.  It used to be instantiated
    automatically when you use @code{\grace}.  Basically, it is an
    `embedded' miniature of the Score context.  Since this context
    needs special interaction with the rest of LilyPond, you should
    not explicitly instantiate it.

   DEPRECATED.
")
(LyricsVoice . "
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
    Typesets lyrics.  It can contain @code{LyricsVoice} contexts.
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
    brace on the left side, grouping the staves together.  The bar
    lines of the contained staves are connected vertically.  It can
    contain @code{Staff} contexts.")

(PianoStaff . "
    Just like @code{GrandStaff} but with @code{minVerticalAlign} set
    equal to @code{maxVerticalAlign} so that interstaff beaming and
    slurring can be used.")

(StaffGroup . "
    Contains @code{Staff} or @code{RhythmicStaff} contexts.  Adds a
    bracket on the left side, grouping the staves together.  The bar
    lines of the contained staves are connected vertically.  It can
    contain @code{Staff}, @code{RhythmicStaff}, @code{GrandStaff}, or
    @code{Lyrics} contexts.
")
(ChoirStaff . "
    Identical to @code{StaffGroup} except that the contained staves
    are not connected vertically.
")
(Score . "
    This is the top level notation context.  No other context can
    contain a @code{Score} context.  This context handles the
    administration of time signatures.  It also makes sure that items
    such as clefs, time signatures, and key-signatures are aligned
    across staves.  It can contain @code{Lyrics}, @code{Staff},
    @code{RhythmicStaff}, @code{GrandStaff}, @code{StaffGroup}, and
    @code{ChoirStaff} contexts.

    You cannot explicitly instantiate a Score context (since it is
    not contained in any other context).  It is instantiated
    automatically when an output definition (a @code{\score} or
    @code{\paper} block) is processed.
")
)
)

(set! context-description-alist
      (sort context-description-alist alist<?))

