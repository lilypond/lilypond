
(define-public music-descriptions
  `(
    (AbortEvent
     . (
	(internal-class-name . "Span_req")
	(span-type . "abort")
	(types . (general-music event abort-event))
	))
    (ArpeggioEvent 
     . (
	(internal-class-name .  "Arpeggio_req")
	(types . (general-music event))
	))
    (ArticulationEvent
     . (
	(internal-class-name . "Articulation_req")
	(types . (general-music event articulation-event script-event))
	)) 
    (BassFigureEvent
     . (
	(internal-class-name . "Bass_figure_req")
	(compress-procedure . ,music-duration-compress)
	(length . ,music-duration-length) 
	(types . (general-music event rhythmic-event bass-figure-event))
	))
    (BreakEvent
     . (
	(internal-class-name . "Break_req")

	(types . (general-music event))
	)) 
    (BreathingSignEvent
     . (
	(internal-class-name . "Breathing_sign_req")

	(types . (general-music event breathing-event))
	)) 
    (BusyPlayingEvent
     . (
	(internal-class-name . "Busy_playing_req")

	(types . (general-music event busy-playing-event))
	)) 
    (ExtenderEvent
     . (
	(internal-class-name . "Extender_req")
	(types . (general-music event))
	))   
    (GlissandoEvent
     . (
	(internal-class-name . "Glissando_req")
	(types . (general-music event))
	)) 
   (GraceMusic
     . (
	(internal-class-name . "Grace_music")
	(iterator-ctor . ,Grace_iterator::constructor)
	(types . (grace-music music-wrapper-music general-music))
	))
   (HyphenEvent
     . (
	(internal-class-name . "Hyphen_req")
	(types . (general-music event))
	))   
    (KeyChangeEvent
     . (
	(internal-class-name . "Key_change_req")
	(types . (general-music event))
	)) 
    (LyricEvent
     . (
	(internal-class-name . "Lyric_req")
	(types . (general-music rhythmic-event event))
	))
    (LigatureEvent
     . (
	(internal-class-name . "Span_req")
	(span-type . ligature)
	(types . (general-music event span-event ligature-event))
	))
    (MarkEvent
     . (
	(internal-class-name . "Mark_req")
	(types . (general-music event))
	))  
    (MelismaEvent
     . (
	(internal-class-name . "Melisma_playing_req")
	(types . (general-music span-event event))
	)) 
    (Melisma_playingEvent
     . (
	(internal-class-name . "Melisma_req")
	(types . (general-music event))
	))
    (Music
     . (
	(internal-class-name . "Music")
	(types . (general-music)) 
	))
    (NoteEvent
     . (
	(internal-class-name . "Request")
	(length . ,music-duration-length) 
	(compress-procedure . ,music-duration-compress)
	(types . (general-music event note-event rhythmic-event melodic-event))
	))
    (PorrectusEvent
     . (
	(internal-class-name . "Porrectus_req")
	(types . (general-music event))
	))
    (RepeatedMusic
     . (
	(internal-class-name . "Repeated_music")
	(type .  repeated-music)
	(types . (general-music repeat-music))
	))
    (Request
     . (
	(internal-class-name . "Request")
	(types . (general-music event))
	)) 
    (RestEvent
     . (
	(internal-class-name . "Request")
	(length . ,music-duration-length)
	(compress-procedure . ,music-duration-compress)
	(types . (general-music event rhythmic-event rest-event))
	)) 
    (RhythmicEvent
     . (
	(internal-class-name . "Rhythmic_req")
	(length . ,music-duration-length) 
	(compress-procedure . ,music-duration-compress)
	(types . (general-music rhythmic-event  event))
	)) 
    (SequentialMusic
     . (
	(internal-class-name . "Sequential_music")
	(iterator-ctor . ,Sequential_music_iterator::constructor)
	(types . (general-music sequential-music))
	))
    (SimultaneousMusic
     . (
	(internal-class-name . "Simultaneous_music")
	(iterator-ctor . ,Simultaneous_music_iterator::constructor)
	
	(types . (general-music simultaneous-music))
	))
    (PropertySet
     . (
	(internal-class-name . "Music")
	(types . (layout-instruction general-music))
	(iterator-ctor . ,Property_iterator::constructor)
	)
     )
     (PropertyUnset
     . (
	(internal-class-name . "Music")
	(types . (layout-instruction general-music))
	(iterator-ctor . ,Property_unset_iterator::constructor)
	)
     )
     (VoiceSeparator
      . (
	 (internal-class-name . "Music")
	 (types . (separator general-music))
	 ))
     (BarCheck
      . (
	 (internal-class-name . "Music")
	 (types . (general-music bar-check))
	 (iterator-ctor . ,Bar_check_iterator::constructor)
	 ))
     (OverrideProperty
      . (
	 (internal-class-name . "Music")
	 (types . (general-music layout-instruction))
	 (iterator-ctor . ,	Push_property_iterator::constructor)
	 ))
     (RevertProperty
      . (
	 (internal-class-name . "Music")
	 (types . (general-music layout-instruction))
	 (iterator-ctor . ,	Pop_property_iterator::constructor)
	 ))
     
    (OutputPropertySetMusic
     . (
	(internal-class-name . "Music")
	(iterator-ctor . ,Output_property_music_iterator::constructor)
	(types . (general-music layout-instruction))
	))
    (ContextSpeccedMusic
     . (
	(internal-class-name . "Context_specced_music")
	(types . (context-specification general-music music-wrapper-music))
	))
    (AutoChangeMusic
     . (
	(internal-class-name . "Music_wrapper")
	(iterator-ctor . ,Auto_change_iterator::constructor)
	(types . (general-music music-wrapper-music auto-change-instruction))
	))
    (TranslatorChange
     . (
	(internal-class-name . "Music")
	(iterator-ctor . , Change_iterator::constructor)
	(types . (general-music translator-change-instruction))
	))
    (TimeScaledMusic
     . (
	(internal-class-name . "Time_scaled_music")
	(iterator-ctor . ,Time_scaled_music_iterator::constructor)
	(types . (time-scaled-music music-wrapper-music general-music))
	))
    (TransposedMusic
     . (
	(internal-class-name . "Transposed_music")
	(types . (music-wrapper-music general-music transposed-music))
	))

    (UntransposableMusic
     . (
	(internal-class-name . "Untransposable_music")
	(types . (music-wrapper-music general-music untransposable-music)) 
	))

    (UnrelativableMusic
     . (
	(internal-class-name . "Un_relativable_music")
	(types . (music-wrapper-music general-music unrelativable-music))
	))

    (RelativeOctaveMusic
     . (
	(internal-class-name . "Relative_octave_music")
	(types . (music-wrapper-music general-music relative-octave-music))
	))

    (LyricCombineMusic
     . (
	(internal-class-name . "Lyric_combine_music")
	(types . (general-music lyric-combine-music))
	(iterator-ctor . ,Lyric_combine_music_iterator::constructor)
	))

    (PartCombineMusic
     . (
	(internal-class-name . "Part_combine_music")
	(types . (general-music part-combine-music))
	(iterator-ctor . ,Part_combine_music_iterator::constructor)
     ))
    (RequestChord
     . (
	(internal-class-name . "Request_chord")
	(iterator-ctor . ,Request_chord_iterator::constructor)
	(types . (general-music simultaneous-music))
	)
     )
     
    (ScriptEvent
     . (
	(internal-class-name . "Script_req")

	(types . (general-music event))
	)) 
    (SkipEvent
     . (
	(internal-class-name . "Request")
	(types . (general-music event rhythmic-event skip-event))
	)) 
    (SpanEvent
     . (
	(internal-class-name . "Span_req")
	(types . (general-music event))
	)) 
    (StringNumberEvent
     . (
	(internal-class-name . "String_number_req")

	(types . (general-music event))
	)) 
    (TempoEvent
     . (
	(internal-class-name . "Tempo_req")

	(types . (general-music event))
	)) 
    (TextScriptEvent
     . (
	(internal-class-name . "Text_script_req")
	(types . (general-music script-event text-script-event event))
	)) 
    (TieEvent
     . (
	(internal-class-name . "Request")
	(types . (general-music tie-event event))
	))
    ))



(define music-name-to-property-table (make-vector 59 '()))

(map (lambda (x)
       (hashq-set! music-name-to-property-table (car x)
		   (assoc-set! (cdr x) 'name (car x)))
       )
     music-descriptions)



(define-public (make-music-by-name x)
  (if (not (symbol? x))
      (misc-error "Not a symbol: ~s" x))
  (let*
      (
       (props (hashq-ref music-name-to-property-table x '()))
       (name (if (pair? props)
		 (cdr (assoc 'internal-class-name props))
		 (misc-error "Can not find music object ~s" x)))
       )

    (if (eq? props '())
	(ly-warn (format "Could not find music type `~a'" x)))  
    (ly-make-bare-music name props)
  ))

