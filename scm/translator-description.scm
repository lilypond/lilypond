
;;
;; TODO: this should come from the C++ code, really.
;;
(define engraver-description-alist-old
'((A2_engraver 
  (name . "A2_engraver")
  (description . "Part combine engraver for orchestral scores.

The markings @emph{a2}, @emph{Solo} and @emph{Solo II}, are
created by this engraver.  It also acts upon instructions of the part
combiner.  Another thing that the this engraver, is forcing of stem,
slur and tie directions, always when both threads are not identical;
up for the musicexpr called @code{one}, down for the musicexpr called
@code{two}.

")
  (grobs-created TextScript)
  (interfaces-acked all)
  (properties-read combineParts noDirection soloADue soloText soloIIText aDueText split-interval unison solo unisilence unirhythm)
  )
 (Arpeggio_engraver 

  (name . "Arpeggio_engraver")
  (description . "Generate an Arpeggio from a Arpeggio_req")
  (grobs-created Arpeggio)
  (interfaces-acked grob-interface)
  (properties-read)
  )
 (Auto_beam_engraver 

  (name . "Auto_beam_engraver")
  (description . "Generate beams based on measure characteristics and observed
Stems.  Uses beatLength, measureLength and measurePosition to decide
when to start and stop a beam.  Overriding beaming is done through
@ref{Stem_engraver} properties stemLeftBeamCount and
stemRightBeamCount.
")
  (grobs-created Beam)
  (interfaces-acked grob-interface)
  (properties-read noAutoBeaming autoBeamSettings)
  )
 (Axis_group_engraver 

  (name . "Axis_group_engraver")
  (description . "Group all objects created in this context in a VerticalAxisGroup spanner.")
  (grobs-created VerticalAxisGroup)
  (interfaces-acked grob-interface)
  (properties-read VerticalExtent MinimumVerticalExtent ExtraVerticalExtent)
  )
 (Bar_engraver 

  (name . "Bar_engraver")
  (description . "Create barlines. This engraver is controlled through the
@code{whichBar} property. If it has no bar line to create, it will forbid a linebreak at this point")
  (grobs-created BarLine)
  (interfaces-acked grob-interface)
  (properties-read whichBar stavesFound)
  )
 (Bar_number_engraver 

  (name . "Bar_number_engraver")
  (description . "A bar number is created whenever measurePosition is zero. It is
put on top of all staves, and appears only at  left side of the staff.")
  (grobs-created BarNumber)
  (interfaces-acked grob-interface)
  (properties-read currentBarNumber)
  )
 (Beam_engraver 

  (name . "Beam_engraver")
  (description . "Handles Beam_requests by engraving Beams.    If omitted, then notes will be
printed with flags instead of beams.")
  (grobs-created Beam)
  (interfaces-acked grob-interface)
  (properties-read beamMelismaBusy)
  )
 (Break_align_engraver 

  (name . "Break_align_engraver")
  (description . "Align grobs with corresponding break-align-symbols into groups, and order the groups according to breakAlignOrder")
  (grobs-created BreakAlignment BreakAlignGroup LeftEdge)
  (interfaces-acked grob-interface)
  (properties-read breakAlignOrder)
  )
 (Breathing_sign_engraver 

  (name . "Breathing_sign_engraver")
  (description . "")
  (grobs-created BreathingSign)
  (interfaces-acked grob-interface)
  (properties-read)
  )
 (Chord_name_engraver 

  (name . "Chord_name_engraver")
  (description . "Catch Note_req's, Tonic_reqs, Inversion_reqs, Bass_req
and generate the appropriate chordname.")
  (grobs-created ChordName)
  (interfaces-acked grob-interface)
  (properties-read chordChanges)
  )
 (Chord_tremolo_engraver 

  (name . "Chord_tremolo_engraver")
  (description . "Generates beams for  tremolo repeats.")
  (grobs-created Beam)
  (interfaces-acked grob-interface)
  (properties-read)
  )
 (Clef_engraver 

  (name . "Clef_engraver")
  (description . "Determine and set reference point for pitches")
  (grobs-created Clef OctavateEight)
  (interfaces-acked grob-interface)
  (properties-read clefPosition clefGlyph centralCPosition clefOctavation explicitClefVisibility)
  )
 (Collision_engraver 

  (name . "Collision_engraver")
  (description . "")
  (grobs-created NoteCollision)
  (interfaces-acked grob-interface)
  (properties-read)
  )
 (Custos_engraver 

  (name . "Custos_engraver")
  (description . "")
  (grobs-created Custos)
  (interfaces-acked grob-interface)
  (properties-read)
  )
 (Dot_column_engraver 

  (name . "Dot_column_engraver")
  (description . " Engraves dots on dotted notes shifted to the right of the note.
If omitted, then dots appear on top of the notes.
")
  (grobs-created DotColumn)
  (interfaces-acked grob-interface)
  (properties-read)
  )
 (Dynamic_engraver 

  (name . "Dynamic_engraver")
  (description . "")
  (grobs-created DynamicLineSpanner DynamicText Hairpin TextSpanner)
  (interfaces-acked grob-interface)
  (properties-read)
  )
 (Engraver_group_engraver 

  (name . "Engraver_group_engraver")
  (description . "A group of engravers taken together")
  (grobs-created)
  (interfaces-acked grob-interface)
  (properties-read)
  )
 (Extender_engraver 

  (name . "Extender_engraver")
  (description . "Create lyric extenders")
  (grobs-created LyricExtender)
  (interfaces-acked grob-interface)
  (properties-read)
  )
 (Font_size_engraver
  (name . "Font_size_engraver")
  (description . "Puts fontSize into font-relative-size grob property.")
  (grobs-created )
  (interfaces-acked grob-interface)
  (properties-read fontSize)
  )
 (Hara_kiri_engraver 

  (name . "Hara_kiri_engraver")
  (description . "Like Axis_group_engraver, but make a hara kiri spanner, and add
interesting items (ie. note heads, lyric syllables and normal rests)
")
  (grobs-created HaraKiriVerticalGroup)
  (interfaces-acked grob-interface)
  (properties-read)
  )
 (Hyphen_engraver 

  (name . "Hyphen_engraver")
  (description . "Create lyric hyphens")
  (grobs-created LyricHyphen)
  (interfaces-acked grob-interface)
  (properties-read)
  )
 (Instrument_name_engraver 

  (name . "Instrument_name_engraver")
  (description . " Prints the name of the instrument (specified by
@code{Staff.instrument} and @code{Staff.instr})
at the left of the
staff.")
  (grobs-created InstrumentName)
  (interfaces-acked bar-line-interface dynamic-interface axis-group-interface)
  (properties-read instrument instr)
  )
 (Key_engraver 

  (name . "Key_engraver")
  (description . "")
  (grobs-created KeySignature)
  (interfaces-acked grob-interface)
  (properties-read keySignature explicitKeySignatureVisibility createKeyOnClefChange keyAccidentalOrder keySignature)
  )
 (Local_key_engraver 
  (name . "Local_key_engraver")
  (description . "Make accidentals.  Catches note heads, ties and notices key-change
events.  Due to interaction with ties (which don't come together
with note heads), this needs to be in a context higher than Tie_engraver. FIXME")
  (grobs-created Accidentals)
  (interfaces-acked grob-interface)
  (properties-read localKeySignature forgetAccidentals noResetKey)
  )
 (Lyric_engraver 

  (name . "Lyric_engraver")
  (description . "")
  (grobs-created)
  (interfaces-acked grob-interface)
  (properties-read)
  )
 (Lyric_phrasing_engraver 

  (name . "Lyric_phrasing_engraver")
  (description . "")
  (grobs-created)
  (interfaces-acked grob-interface)
  (properties-read automaticPhrasing melismaEngraverBusy associatedVoice phrasingPunctuation)
  )
 (Mark_engraver 

  (name . "Mark_engraver")
  (description . "")
  (grobs-created RehearsalMark)
  (interfaces-acked grob-interface)
  (properties-read rehearsalMark stavesFound)
  )
 (Melisma_engraver 

  (name . "Melisma_engraver")
  (description . "")
  (grobs-created)
  (interfaces-acked grob-interface)
  (properties-read melismaBusy slurMelismaBusy tieMelismaBusy beamMelismaBusy)
  )
 (Multi_measure_rest_engraver 

  (name . "Multi_measure_rest_engraver")
  (description . "Engraves multi-measure rests that are produced with @code{R}.  Reads
measurePosition and currentBarNumber to determine what number to print over the MultiMeasureRest
")
  (grobs-created MultiMeasureRest)
  (interfaces-acked grob-interface)
  (properties-read currentBarNumber currentCommandColumn measurePosition)
  )
 (Note_head_line_engraver 

  (name . "Note_head_line_engraver")
  (description . "Engrave a line between two note heads, for example a glissando.
If followVoice is set, staff switches also generate a line.")
  (grobs-created Glissando VoiceFollower)
  (interfaces-acked grob-interface)
  (properties-read followVoice)
  )
 (Note_heads_engraver 

  (name . "Note_heads_engraver")
  (description . "Generate one or more noteheads from Music of type Note_req.")
  (grobs-created NoteHead Dots)
  (interfaces-acked grob-interface)
  (properties-read)
  )
 (Note_name_engraver 

  (name . "Note_name_engraver")
  (description . "")
  (grobs-created NoteName)
  (interfaces-acked grob-interface)
  (properties-read)
  )
 (Output_property_engraver 

  (name . "Output_property_engraver")
  (description . "Interpret Music of Output_property type, and apply a function
to any Graphic objects that satisfies the predicate.")
  (grobs-created)
  (interfaces-acked grob-interface)
  (properties-read)
  )
 (Percent_repeat_engraver
  (name . "Percent_repeat_engraver")
  (description . "Make beat, whole bar and double bar repeats.")
  (grobs-created PercentRepeat RepeatSlash DoublePercentRepeat)
  (interfaces-acked grob-interface)
  (properties-read measureLength currentCommandColumn)
  )
 
 (Piano_pedal_engraver 
  (name . "Piano_pedal_engraver")
  (description . "Engrave piano pedal symbols.")
  (grobs-created SostenutoPedal SustainPedal UnaCordaPedal)
  (interfaces-acked rhythmic-head-interface stem-interface)
  (properties-read pedalSostenutoStrings pedalSustainStrings pedalUnaCordaStrings)
  )
 (Pitch_squash_engraver 
  (name . "Pitch_squash_engraver")
  (description . "Treat all pitches as middle C.  Note that the notes move, but
the locations of accidentals stay the same. 
Set the position field of all note heads to zero. This useful for
making a single line staff that demonstrates the rhythm of a melody.")
  (grobs-created)
  (interfaces-acked grob-interface)
  (properties-read squashedPosition)
  )
 (Phrasing_slur_engraver
  (name . "Phrasing_slur_engraver")
  (description . "Print phrasing slurs. Similar to Slur_engraver")
  (grobs-created PhrasingSlur)
  (interfaces-acked grob-interface)
  (properties-read slurBeginAttachment slurEndAttachment slurMelismaBusy)
 )  
 (Porrectus_engraver 

  (name . "Porrectus_engraver")
  (description . "Join adjacent notes to a porrectus ligature.")
  (grobs-created Porrectus)
  (interfaces-acked grob-interface)
  (properties-read)
  )
 (Property_engraver 

  (name . "Property_engraver")
  (description . "This is a engraver that converts property settings into
back-end grob-property settings. Example: Voice.stemLength will set
#'length in all Stem objects.

Due to CPU and memory requirements, the use of this engraver is deprecated.")
  (grobs-created)
  (interfaces-acked grob-interface)
  (properties-read Generic_property_list)
  )
 (Repeat_acknowledge_engraver 

  (name . "Repeat_acknowledge_engraver")
  (description . "Acknowledge repeated music, and convert the contents of
repeatCommands ainto an appropriate setting for whichBar")
  (grobs-created)
  (interfaces-acked grob-interface)
  (properties-read repeatCommands whichBar)
  )
 (Rest_collision_engraver 

  (name . "Rest_collision_engraver")
  (description . "Handles collisions of rests.")
  (grobs-created RestCollision)
  (interfaces-acked grob-interface)
  (properties-read)
  )
 (Rest_engraver 

  (name . "Rest_engraver")
  (description . "")
  (grobs-created Rest Dots)
  (interfaces-acked grob-interface)
  (properties-read)
  )
 (Rhythmic_column_engraver 

  (name . "Rhythmic_column_engraver")
  (description . "Generates NoteColumn, an objects that groups stems, noteheads and rests.")
  (grobs-created NoteColumn)
  (interfaces-acked grob-interface)
  (properties-read)
  )
 (Score_engraver 

  (name . "Score_engraver")
  (description . "Top level engraver. Takes care of generating columns and the complete  system (ie. LineOfScore)


This engraver decides whether a column is breakable. The default is
that a column is always breakable. However, when every Bar_engraver
that does not have a barline at a certain point will call
Score_engraver::forbid_breaks to stop linebreaks.  In practice, this
means that you can make a breakpoint by creating a barline (assuming
that there are no beams or notes that prevent a breakpoint.)


")
  (grobs-created LineOfScore PaperColumn NonMusicalPaperColumn)
  (interfaces-acked grob-interface)
  (properties-read currentMusicalColumn currentCommandColumn)
  )
 (Script_column_engraver 

  (name . "Script_column_engraver")
  (description . "")
  (grobs-created ScriptColumn)
  (interfaces-acked grob-interface)
  (properties-read)
  )
 (Script_engraver 

  (name . "Script_engraver")
  (description . "    Handles note ornaments generated by @code{\\script}.  
")
  (grobs-created Script)
  (interfaces-acked grob-interface)
  (properties-read scriptDefinitions scriptHorizontal)
  )
 (Separating_line_group_engraver 

  (name . "Separating_line_group_engraver")
  (description . "Generates objects for computing spacing parameters.")
  (grobs-created SeparationItem SeparatingGroupSpanner)
  (interfaces-acked grob-interface)
  (properties-read)
  )
 (Skip_req_swallow_translator 

  (name . "Skip_req_swallow_translator")
  (description . "")
  (grobs-created)
  (interfaces-acked grob-interface)
  (properties-read)
  )
 (Slur_engraver 

  (name . "Slur_engraver")
  (description . "Build slurs from Slur_reqs")
  (grobs-created Slur)
  (interfaces-acked grob-interface)
  (properties-read slurBeginAttachment slurEndAttachment slurMelismaBusy)
  )
 (Spacing_engraver 

  (name . "Spacing_engraver")
  (description . "make a SpacingSpanner and do bookkeeping of shortest starting and playing notes  ")
  (grobs-created SpacingSpanner)
  (interfaces-acked grob-interface)
  (properties-read)
  )
 (Span_arpeggio_engraver 

  (name . "Span_arpeggio_engraver")
  (description . "")
  (grobs-created Arpeggio)
  (interfaces-acked grob-interface)
  (properties-read connectArpeggios)
  )
 (Span_bar_engraver 

  (name . "Span_bar_engraver")
  (description . "This engraver makes cross-staff barlines: It catches all normal
bar lines, and draws a single span-bar across them.")
  (grobs-created SpanBar)
  (interfaces-acked grob-interface)
  (properties-read)
  )
 (Staff_symbol_engraver 

  (name . "Staff_symbol_engraver")
  (description . "create the constellation of five (default)
staff lines.")
  (grobs-created Sta
		 ffSymbol)
  (interfaces-acked grob-interface)
  (properties-read)
  )
 (Stanza_number_engraver 

  (name . "Stanza_number_engraver")
  (description . "")
  (grobs-created StanzaNumber)
  (interfaces-acked grob-interface)
  (properties-read stz stanza)
  )
 (Stem_engraver 

  (name . "Stem_engraver")
  (description . "Create stems and single-stem tremolos.  It also works together with
the beam engraver for overriding beaming.")
  (grobs-created Stem StemTremolo)
  (interfaces-acked rhythmic-head-interface)
  (properties-read tremoloFlags stemLeftBeamCount stemRightBeamCount)
  )
 (System_start_delimiter_engraver 

  (name . "System_start_delimiter_engraver")
  (description . "creates a system start delimiter (ie. SystemStart@{Bar,Brace,Bracket@} spanner")
  (grobs-created SystemStartBar SystemStartBrace SystemStartBracket)
  (interfaces-acked grob-interface)
  (properties-read)
  )
 (Text_engraver 

  (name . "Text_engraver")
  (description . "Create text-scripts")
  (grobs-created TextScript)
  (interfaces-acked grob-interface)
  (properties-read scriptHorizontal textNonEmpty)
  )
 (Text_spanner_engraver 

  (name . "Text_spanner_engraver")
  (description . "Create text spanner from a  Span_req ")
  (grobs-created TextSpanner)
  (interfaces-acked grob-interface)
  (properties-read)
  )
 (Thread_devnull_engraver 

  (name . "Thread_devnull_engraver")
  (description . "Kill elements whenever we are Voice called `two' and either
unison, unisilence or soloADue is set.@footnote{On unix systems, the
file @file{/dev/null} is special device: anything written to it is
discarded.}. This engraver works closely together with the part
combiner.  When the part combiner notices that two threads are
identical, it tells the @code{Thread_devnull_engraver} to discard
everything in the second thread.
")
  (grobs-created)
  (interfaces-acked grob-interface)
  (properties-read)
  )
 (Tie_engraver 

  (name . "Tie_engraver")
  (description . "Generate ties between noteheads of equal pitch.")
  (grobs-created Tie TieColumn)
  (interfaces-acked grob-interface)
  (properties-read sparseTies tieMelismaBusy)
  )
 (Time_signature_engraver 

  (name . "Time_signature_engraver")
  (description . "Create a TimeSignature whenever @code{timeSignatureFraction} changes")
  (grobs-created TimeSignature)
  (interfaces-acked grob-interface)
  (properties-read)
  )
 (Timing_engraver 

  (name . "Timing_engraver")
  (description . " Responsible for synchronizing timing information from staves. 
Normally in @code{Score}.  In order to create polyrhythmic music,
this engraver should be removed from @code{Score} and placed in
@code{Staff}.")
  (grobs-created)
  (interfaces-acked grob-interface)
  (properties-read timeSignatureFraction barCheckNoSynchronize barNonAuto whichBar barAlways defaultBarType skipBars timing oneBeat measureLength measurePosition currentBarNumber)
  )
 (Tuplet_engraver 

  (name . "Tuplet_engraver")
  (description . "Catch Time_scaled_music and generate appropriate bracket  ")
  (grobs-created TupletBracket)
  (interfaces-acked grob-interface)
  (properties-read tupletNumberFormatFunction tupletSpannerDuration tupletInvisible)
  )
 (Vertical_align_engraver 

  (name . "Vertical_align_engraver")
  (description . "Catch Vertical axis groups and stack them.")
  (grobs-created VerticalAlignment)
  (interfaces-acked grob-interface)
  (properties-read)
  )
 (Voice_devnull_engraver 

  (name . "Voice_devnull_engraver")
  (description . "Kill off certain items and spanners if we're Voice `two' and unison or unisilence is set.")
  (grobs-created)
  (interfaces-acked grob-interface)
  (properties-read)
  )
 (Volta_engraver 

  (name . "Volta_engraver")
  (description . "Make volta brackets")
  (grobs-created VoltaBracket)
  (interfaces-acked grob-interface)
  (properties-read repeatCommands voltaSpannerDuration)
  )
 )
)
(define (alist<? x y)
  (string<? (symbol->string (car x))
	    (symbol->string (car y))))

;(set! engraver-description-alist
;      (sort engraver-description-alist alist<?))


(define (humane-listify l)
  (cond
   ((null? l) "")
   ((null? (cdr l)) (symbol->string (car l)))
   (else (string-append (symbol->string (car l)) " " (human-listify (cdr l)))
	 
	 )))
   
(define (print-entry x)
  (define (mungle x)
    (string-append (string-downcase (regexp-substitute/global #f "_" x 'pre "-" 'post)) ".cc"))
  
  (let ((cop (open-file (mungle (car x)) "a")))
    (define (w y)
      (write y cop))
    (define (d y)
      (display y cop))
    
    (d "ENTER_DESCRIPTION(")
    (d (car x))
    (d ",\n/* descr */       ")
    (w (cdr (assoc 'description (cdr x))))
    (d ",\n/* creats*/       ")
    (w (human-listify (cdr (assoc 'grobs-created (cdr x)))))
    (d ",\n/* acks  */       ")
    (w (human-listify (cdr (assoc 'interfaces-acked (cdr x)))))
    (d ",\n/* reads */       ")	
    (w (human-listify (cdr (assoc 'properties-read (cdr x)))))
    (d ",\n/* write */       \"\");\n")
))

;(map print-entry engraver-description-alist)

