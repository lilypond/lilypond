
(define-public music-descriptions
  `(
    (ArpeggioEvent 
     . (
	(name . "Arpeggio_req")
	(types . (general-music event))
	))
    (ArticulationEvent
     . (
	(name . "Articulation_req")
	(types . (general-music event articulation-event script-event))
	)) 
    (BassFigureEvent
     . (
	(name . "Bass_figure_req")
	(types . (general-music event rhythmic-event bass-figure-event))
	))  
    (BreakEvent
     . (
	(name . "Break_req")

	(types . (general-music event))
	)) 
    (BreathingSignEvent
     . (
	(name . "Breathing_sign_req")

	(types . (general-music event breathing-event))
	)) 
    (BusyPlayingEvent
     . (
	(name . "Busy_playing_req")

	(types . (general-music event busy-playing-event))
	)) 
    (ExtenderEvent
     . (
	(name . "Extender_req")
	(types . (general-music event))
	))   
    (GlissandoEvent
     . (
	(name . "Glissando_req")
	(types . (general-music event))
	)) 
   (GraceMusic
     . (
	(name . "Grace_music")
	(iterator-ctor . ,Grace_iterator::constructor)
	(types . (grace-music music-wrapper-music general-music))
	))
   (HyphenEvent
     . (
	(name . "Hyphen_req")
	(types . (general-music event))
	))   
    (KeyChangeEvent
     . (
	(name . "Key_change_req")
	(types . (general-music event))
	)) 
    (LyricEvent
     . (
	(name . "Lyric_req")
	(types . (general-music rhythmic-event event))
	)) 
    (MarkEvent
     . (
	(name . "Mark_req")
	(types . (general-music event))
	))  
    (MelismaEvent
     . (
	(name . "Melisma_playing_req")
	(types . (general-music span-event event))
	)) 
    (Melisma_playingEvent
     . (
	(name . "Melisma_req")
	(types . (general-music event))
	))
    (Music
     . (
	(name . "Music")
	(types . (general-music)) 
	))
    (NoteEvent
     . (
	(name . "Note_req")
	(types . (general-music event rhythmic-event melodic-event))
	))
    (PorrectusEvent
     . (
	(name . "Porrectus_req")
	(types . (general-music event))
	))
    (RepeatedMusic
     . (
	(name . "Repeated_music")
	(type .  repeated-music)
	(types . (general-music repeat-music))
	))
    (Request
     . (
	(name . "Request")
	(types . (general-music event))
	)) 
    (RestEvent
     . (
	(name . "Rest_req")
	(types . (general-music event rhythmic-event ))
	)) 
    (RhythmicEvent
     . (
	(name . "Rhythmic_req")
	(types . (general-music rhythmic-event  event))
	)) 
    (SequentialMusic
     . (
	(name . "Sequential_music")
	(iterator-ctor . ,Sequential_music_iterator::constructor)
	(types . (general-music sequential-music))
	))
    (SimultaneousMusic
     . (
	(name . "Simultaneous_music")
	(iterator-ctor . ,Simultaneous_music_iterator::constructor)
	
	(types . (general-music simultaneous-music))
	))
    (PropertySet
     . (
	(name . "Music")
	(types . (layout-instruction general-music))
	(iterator-ctor . ,Property_iterator::constructor)
	)
     )
     (PropertyUnset
     . (
	(name . "Music")
	(types . (layout-instruction general-music))
	(iterator-ctor . ,Property_unset_iterator::constructor)
	)
     )
     (VoiceSeparator
      . (
	 (name . "Music")
	 (types . (separator general-music))
	 ))
     (BarCheck
      . (
	 (name . "Music")
	 (types . (general-music bar-check))
	 (iterator-ctor . ,Bar_check_iterator::constructor)
	 ))
     (OverrideProperty
      . (
	 (name . "Music")
	 (types . (general-music layout-instruction))
	 (iterator-ctor . ,	Push_property_iterator::constructor)
	 ))

     (RevertProperty
      . (
	 (name . "Music")
	 (types . (general-music layout-instruction))
	 (iterator-ctor . ,	Pop_property_iterator::constructor)
	 ))
     
    (OutputPropertySetMusic
     . (
	(name . "Music")
	(iterator-ctor . ,Output_property_music_iterator::constructor)
	(types . (general-music layout-instruction))
	))
    (ContextSpeccedMusic
     . (
	(name . "Context_specced_music")
	(types . (context-specification general-music music-wrapper-music))
	))
    (AutoChangeMusic
     . (
	(name . "Music_wrapper")
	(iterator-ctor . ,Auto_change_iterator::constructor)
	(types . (general-music music-wrapper-music auto-change-instruction))
	))
    (TranslatorChange
     . (
	(name . "Music")
	(iterator-ctor . , Change_iterator::constructor)
	(types . (general-music translator-change-instruction))
	))
 

    (TimeScaledMusic
     . (
	(name . "Time_scaled_music")
	(iterator-ctor . ,Time_scaled_music_iterator::constructor)
	(types . (time-scaled-music music-wrapper-music general-music))
	))
    (TransposedMusic
     . (
	(name . "Transposed_music")
	(types . (music-wrapper-music general-music transposed-music))
	))

    (UntransposableMusic
     . (
	(name . "Untransposable_music")
	(types . (music-wrapper-music general-music untransposable-music)) 
	))

    (UnrelativableMusic
     . (
	(name . "Un_relativable_music")
	(types . (music-wrapper-music general-music unrelativable-music))
	))

    (RelativeOctaveMusic
     . (
	(name . "Relative_octave_music")
	(types . (music-wrapper-music general-music relative-octave-music))
	))

    (LyricCombineMusic
     . (
	(name . "Lyric_combine_music")
	(types . (general-music lyric-combine-music))
	(iterator-ctor . ,Lyric_combine_music_iterator::constructor)
	))

    (PartCombineMusic
     . (
	(name . "Part_combine_music")
	(types . (general-music part-combine-music))
	(iterator-ctor . ,Part_combine_music_iterator::constructor)
     ))
    (RequestChord
     . (
	(name . "Request_chord")
	(iterator-ctor . ,Request_chord_iterator::constructor)
	(types . (general-music simultaneous-music))
	)
     )
     
    (ScriptEvent
     . (
	(name . "Script_req")

	(types . (general-music event))
	)) 
    (SkipEvent
     . (
	(name . "Skip_req")

	(types . (general-music event rhythmic-event ))
	)) 
    (SpanEvent
     . (
	(name . "Span_req")
	(types . (general-music event))
	)) 
    (StringNumberEvent
     . (
	(name . "String_number_req")

	(types . (general-music event))
	)) 
    (TempoEvent
     . (
	(name . "Tempo_req")

	(types . (general-music event))
	)) 
    (TextScriptEvent
     . (
	(name . "Text_script_req")
	(types . (general-music script-event text-script-event event))
	)) 
    (TieEvent
     . (
	(name . "Tie_req")
	(types . (general-music tie-event event))
	))
    ))



(define music-name-to-property-table (make-vector 59))
(map (lambda (x)
       (hashq-set! music-name-to-property-table (car x) (cdr x))
       )
     music-descriptions)

(define-public (make-music-by-name x)
  (if (not (symbol? x))
      (misc-error "Not a symbol: ~s" x))
  (let*
      (
       (props (hashq-ref music-name-to-property-table x '()))
       (name (if (pair? props) (cdr (assoc 'name props)) "Music"))
       )

    (if (eq? props '())
	(ly-warn (format "Could not find music type `~a'" x)))  
    (ly-make-bare-music name props)
  ))
