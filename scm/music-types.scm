
(define-public music-descriptions
  `(
    (AbortEvent
     . (
	(description .  "Abort currently running spanners.")
	(internal-class-name . "Event")
	(span-type . "abort")
	(types . (general-music event abort-event))
	))
    (AbsoluteDynamicEvent
     . (
	(description . "")
	(internal-class-name . "Event")
	(types . (general-music event dynamic-event absolute-dynamic-event))
	))
    (ArpeggioEvent 
     . (
	(description .  "Make an arpeggio on this note. Syntax:
@var{note}-@code{\\arpeggio}")
	(internal-class-name .  "Event")
	(types . (general-music arpeggio-event event))
	))

    ;; todo: use articulation-event for slur as well.
    ;; separate non articulation scripts  
    (ArticulationEvent
     . (
	(description .  "")

	(internal-class-name . "Event")
	(types . (general-music event articulation-event script-event))
	)) 
    (AutoChangeMusic
     . (
	(description .  "")

	(internal-class-name . "Music_wrapper")
	(iterator-ctor . ,Auto_change_iterator::constructor)
	(types . (general-music music-wrapper-music auto-change-instruction))
	))
    (BarCheck
     . (
	(description .  "")
	(internal-class-name . "Music")
	(types . (general-music bar-check))
	(iterator-ctor . ,Bar_check_iterator::constructor)
	))
    (BassFigureEvent
     . (
	(description .  "")

	(internal-class-name . "Event")
	(types . (general-music event rhythmic-event bass-figure-event))
	))
    (BeamEvent
     . (
	(description .  "")

	(internal-class-name . "Event")
	(types . (general-music event beam-event span-event))
	))
    (BreakEvent
     . (
	(description .  "")

	(internal-class-name . "Event")
	(types . (general-music break-event event))
	))
    (BreathingSignEvent
     . (
	(description .  "")

	(internal-class-name . "Event")
	(types . (general-music event breathing-event))
	)) 
    (BusyPlayingEvent
     . (
	(description .  "")

	(internal-class-name . "Event")
	(types . (general-music event busy-playing-event))
	)) 
    (ContextSpeccedMusic
     . (
	(description .  "")
	(iterator-ctor . ,Context_specced_music_iterator::constructor)
	(internal-class-name . "Music_wrapper")
	(types . (context-specification general-music music-wrapper-music))
	))
    (CrescendoEvent
     . (
	(description .  "")

	(internal-class-name . "Event")
	(types . (general-music dynamic-event crescendo-event event))
	)) 
    (DecrescendoEvent
     . (
	(description .  "")

	(internal-class-name . "Event")
	(types . (general-music dynamic-event decrescendo-event event))
	))
 
    (ExtenderEvent
     . (
	(description .  "")

	(internal-class-name . "Event")
	(types . (general-music extender-event event))
	))   
    (GlissandoEvent
     . (
	(description .  "")

	(internal-class-name . "Event")
	(types . (general-music glissando-event event))
	)) 
    (GraceMusic
     . (
	(description .  "")

	(internal-class-name . "Grace_music")
	(iterator-ctor . ,Grace_iterator::constructor)
	(types . (grace-music music-wrapper-music general-music))
	))
    (NoteGroupingEvent
     . (
	(description . "Start or stop grouping brackets.")
	(internal-class-name . "Event")
	(types . (general-music event note-grouping-event))
	))
    (HyphenEvent
     . (
	(description .  "")

	(internal-class-name . "Event")
	(types . (general-music hyphen-event event))
	))   
    (KeyChangeEvent
     . (
	(description .  "")

	(internal-class-name . "Key_change_req")
	(types . (general-music key-change-event event))
	)) 
    (LyricCombineMusic
     . (
	(description .  "")

	(internal-class-name . "Lyric_combine_music")
	(types . (general-music lyric-combine-music))
	(iterator-ctor . ,Lyric_combine_music_iterator::constructor)
	))

    (LyricEvent
     . (
	(description .  "A lyric syllable. Must be entered in lyrics mode, i.e.
@code{\\lyrics @{ twinkle4 twinkle4 @} } .")

	(internal-class-name . "Event")
	(types . (general-music rhythmic-event lyric-event event))
	))
    (LigatureEvent
     . (
	(description .  "")

	(internal-class-name . "Event")
	(span-type . ligature)
	(types . (general-music event span-event ligature-event))
	))
    (MarkEvent
     . (
	(description .  "")

	(internal-class-name . "Event")
	(types . (general-music mark-event event))
	))  
    (MelismaEvent
     . (
	(description .  "")

	(internal-class-name . "Event")
	(types . (general-music span-event melisma-playing-event event))
	)) 
    (MelismaPlayingEvent
     . (
	(description .  "")

	(internal-class-name . "Event")
	(types . (general-music event))
	))
    (MultiMeasureRestEvent
     . (
	(description . "Rests that may be compressed into Multi rests. Syntax
@code{R2.*4} for 4 measures in 3/4 time. Note the capital R.")
	(internal-class-name . "Event")
	(types . (general-music event multi-measure-rest-event))
	))
    (Music
     . (
	(description .  "")

	(internal-class-name . "Music")
	(types . (general-music)) 
	))
    (NoteEvent
     . (
	(description .  "")

	(internal-class-name . "Event")
	(types . (general-music event note-event rhythmic-event melodic-event))
	))
    (OverrideProperty
     . (
	(description .  "")

	(internal-class-name . "Music")
	(types . (general-music layout-instruction))
	(iterator-ctor . ,	Push_property_iterator::constructor)
	))

    (PartCombineMusic
     . (
	(description .  "")

	(internal-class-name . "Simultaneous_music")
	(types . (general-music part-combine-music))
	(iterator-ctor . ,Part_combine_music_iterator::constructor)
	))
    (PhrasingSlurEvent
     . (
	(description . "Start or end phrasing slur. Syntax NOTE \\(  and \\) NOTE")
	(internal-class-name . "Event")
	(types . (general-music span-event phrasing-slur-event slur-event))
	))
    (PropertySet
     . (
	(description .  "")
	(internal-class-name . "Music")
	(types . (layout-instruction general-music))
	(iterator-ctor . ,Property_iterator::constructor)
	)
     )
    (PropertyUnset
     . (
	(description .  "")

	(internal-class-name . "Music")
	(types . (layout-instruction general-music))
	(iterator-ctor . ,Property_unset_iterator::constructor)
	)
     )
    (PorrectusEvent
     . (
	(description .  "")

	(internal-class-name . "Event")
	(types . (general-music porrectus-event event))
	))
    (RepeatedMusic
     . (
	(description .  "")

	(type .  repeated-music)
	(types . (general-music repeated-music))
	))
    (Event
     . (
	(description .  "")

	(internal-class-name . "Event")
	(types . (general-music event))
	)) 
    (RestEvent
     . (
	(description .  "")

	(internal-class-name . "Event")
	(types . (general-music event rhythmic-event rest-event))
	)) 
    (SequentialMusic
     . (
	(description .  "Music expressions concatenated. Syntax \\sequential @{..@} or simply @{..@} .")

	(internal-class-name . "Sequential_music")
	(iterator-ctor . ,Sequential_music_iterator::constructor)
	(types . (general-music sequential-music))
	))
    (SimultaneousMusic
     . (
	(description .  "Music playing together. Syntax: \\simultaneous @{ .. @} or < .. >.")

	(internal-class-name . "Simultaneous_music")
	(iterator-ctor . ,Simultaneous_music_iterator::constructor)
	
	(types . (general-music simultaneous-music))
	))
    (SlurEvent
     . (
	(description . "Start or end slur. Syntax NOTE(  and )NOTE")
	(internal-class-name . "Event")
	(types . (general-music span-event slur-event))
	))

    (RevertProperty
     . (
	(description .  "")

	(internal-class-name . "Music")
	(types . (general-music layout-instruction))
	(iterator-ctor . ,	Pop_property_iterator::constructor)
	))
    
    (OutputPropertySetMusic
     . (
	(description .  "")

	(internal-class-name . "Music")
	(iterator-ctor . ,Output_property_music_iterator::constructor)
	(types . (general-music layout-instruction))
	))
    
    (TextSpanEvent
     . (
	(description . "Start a text spanner like 8va.....|")
	(internal-class-name . "Event")
	(types . (general-music span-event text-span-event))
	))
    (TranslatorChange
     . (
	(description .  "")
	(internal-class-name . "Music")
	(iterator-ctor . , Change_iterator::constructor)
	(types . (general-music translator-change-instruction))
	))
    
    (TimeScaledMusic
     . (
	(description .  "")
	(internal-class-name . "Time_scaled_music")
	(iterator-ctor . ,Time_scaled_music_iterator::constructor)
	(types . (time-scaled-music music-wrapper-music general-music))
	))
    
    (TransposedMusic
     . (
	(description .  "")
	(internal-class-name . "Transposed_music")
	(types . (music-wrapper-music general-music transposed-music))
	))

    (UntransposableMusic
     . (
	(description .  "")

	(internal-class-name . "Untransposable_music")
	(types . (music-wrapper-music general-music untransposable-music)) 
	))

    (UnrelativableMusic
     . (
	(description .  "")
	(internal-class-name . "Un_relativable_music")
	(types . (music-wrapper-music general-music unrelativable-music))
	))

    (RelativeOctaveMusic
     . (
	(description .  "")

	(internal-class-name . "Relative_octave_music")
	(types . (music-wrapper-music general-music relative-octave-music))
	))

    
    (EventChord
     . (
	(description .  "")

	(internal-class-name . "Simultaneous_music")
	(iterator-ctor . ,Event_chord_iterator::constructor)
	(types . (general-music event-chord simultaneous-music))
	)
     )
    
    (ScriptEvent
     . (
	(description .  "")

	(internal-class-name . "Event")
	(types . (general-music event))
	)) 
    (SkipEvent
     . (
	(description .  "")

	(internal-class-name . "Event")
	(types . (general-music event rhythmic-event skip-event))
	)) 
    (SpanEvent
     . (
	(description .  "")

	(internal-class-name . "Event")
	(types . (general-music event))
	)) 
    (SustainPedalEvent
     . (
	(description . "")
	(internal-class-name . "Event")
	(types . (general-music pedal-event sustain-pedal-event))
	))
    (SostenutoEvent
     . (
	(description . "")
	(internal-class-name . "Event")
	(types . (general-music pedal-event sostenuto-pedal-event))
	))
    (UnaCordaEvent
     . (
	(description . "")
	(internal-class-name . "Event")
	(types . (general-music pedal-event una-corda-pedal-event))
	))
    (StringNumberEvent
     . (
	(description .  "")

	(internal-class-name . "Event")
	(types . (general-music string-number-event event))
	)) 
    (TempoEvent
     . (
	(description .  "")

	(internal-class-name . "Event")
	(types . (general-music tempo-event event))
	)) 
    (TextScriptEvent
     . (
	(description .  "")
	(internal-class-name . "Event")
	(types . (general-music script-event text-script-event event))
	)) 
    (TieEvent
     . (
	(description .  "A tie. Entered as ~.")
	(internal-class-name . "Event")
	(types . (general-music tie-event event))
	))
    (TremoloEvent
     . (
	(description . "Un measured tremolo.")
	(internal-class-name . "Event")
	(types . (general-music event tremolo-event))
	))
    (VoiceSeparator
     . (
	(description .  "")

	(internal-class-name . "Music")
	(types . (separator general-music))
	))

    (VoltaRepeatedMusic
     . (
	(iterator-ctor . ,Volta_repeat_iterator::constructor)
	(internal-class-name . "Repeated_music")
	(description . "")
	(start-moment-function .  ,Repeated_music::first_start)
	(length . ,Repeated_music::volta_music_length)
	(types . (general-music repeated-music volta-repeated-music))
	))
    
    (UnfoldedRepeatedMusic
     . (
	(iterator-ctor . ,Unfolded_repeat_iterator::constructor)
	(description .  "")
	(start-moment-function .  ,Repeated_music::first_start)
	(internal-class-name . "Repeated_music")
	(types . (general-music repeated-music unfolded-repeated-music))
	(length . ,Repeated_music::unfolded_music_length)
	))
    (PercentRepeatedMusic
     . (
	(internal-class-name . "Repeated_music")
	(description .  "")
	(iterator-ctor . ,Percent_repeat_iterator::constructor)
	(start-moment-function .  ,Repeated_music::first_start)
	(length . ,Repeated_music::unfolded_music_length)
	(types . (general-music repeated-music percent-repeated-music))
	))
    
    (TremoloRepeteadMusic
     . (
	(iterator-ctor . ,Chord_tremolo_iterator::constructor)
	(description .  "")
	(internal-class-name . "Repeated_music")
	(start-moment-function .  ,Repeated_music::first_start)

	;; the length of the repeat is handled by shifting the note logs
	(length . ,Repeated_music::folded_music_length)
	(types . (general-music repeated-music tremolo-repeated-music))
	
	))
    (FoldedRepeatedMusic
     . (
	(internal-class-name . "Repeated_music")
	(description .  "")
	(iterator-ctor  . ,Folded_repeat_iterator::constructor)
	(start-moment-function .  ,Repeated_music::minimum_start)
	(length . ,Repeated_music::folded_music_length)
	(types . (general-music repeated-music folded-repeated-music))
	))
    ))

(set! music-descriptions
      (sort music-descriptions alist<?))

(define music-name-to-property-table (make-vector 59 '()))


;; init hash table,
;; transport description to an object property.
(set!
 music-descriptions
 (map (lambda (x)
	(set-object-property! (car x)
			      'music-description
			      (cdr (assq 'description (cdr x))))
	(let
	    ((l (cdr x)))
	  (set! l (assoc-set! l 'name (car x)))
	  (set! l (assq-remove!  l 'description))
	  (hashq-set! music-name-to-property-table (car x) l)
	  (cons (car x) l)
	  ))
      music-descriptions))



(define-public (make-music-by-name x)
  (if (not (symbol? x))
      (error (format "Not a symbol: ~a" x)))
  (let*
      (
       (props (hashq-ref music-name-to-property-table x '()))
       (name (if (pair? props)
		 (cdr (assoc 'internal-class-name props))
		 (error "Can not find music object" x)))
       )

    (if (eq? props '())
	(ly-warn (format "Could not find music type `~a'" x)))  
    (ly-make-bare-music name props)
    ))


(define-public (old-span-event->event name)
  (let
      (
       (entry   (assoc
		 name
		 '(
		   ("text" . TextSpanEvent)
		   ("decrescendo" . DecrescendoEvent)
		   ("crescendo" . CrescendoEvent)
		   ("Sustain" . SustainPedalEvent)
		   ("slur" . SlurEvent)
		   ("UnaCorda" . UnaCordaEvent)
		   ("Sostenuto" . SostenutoEvent)
		   )))
       )
    (if (eq? entry #f)
	(error (format "Could not find span type ~a" name))
	
	(make-music-by-name (cdr entry))
	)
    ))

(define-public (make-repeated-music name)
  (let*
      (
       (handle (assoc
		name
		'(("volta" . VoltaRepeatedMusic)
		  ("unfold" . UnfoldedRepeatedMusic)
		  ("percent" . PercentRepeatedMusic)
		  ("tremolo" . TremoloRepeteadMusic)
		  ("fold" . FoldedRepeatedMusic)
		  )))
       (music-name
	(if (pair? handle)
	    (cdr handle)
	    (begin
	      (ly-warn
	       (string-append "Unknown repeat type `" name
			      "'\nSee music-types.scm for supported repeats"))
	      'VoltaRepeatedMusic)
	    )
	)
       )

    (make-music-by-name music-name)
    ))

