
(define-public music-descriptions
  `(
    (AbortEvent
     . (
	(description .  "Abort currently running spanners.")
	(internal-class-name . "Span_req")
	(span-type . "abort")
	(types . (general-music event abort-event))
	))
    (ArpeggioEvent 
     . (
	(description .  "Make an arpeggio on this note.")
	(internal-class-name .  "Request")
	(types . (general-music arpeggio-event event))
	))
    (ArticulationEvent
     . (
	(description .  "")

	(internal-class-name . "Articulation_req")
	(types . (general-music event articulation-event script-event))
	)) 
    (BassFigureEvent
     . (
	(description .  "")

	(internal-class-name . "Request")
	(compress-procedure . ,music-duration-compress)
	(length . ,music-duration-length) 
	(types . (general-music event rhythmic-event bass-figure-event))
	))
    (BeamEvent
     . (
	(description .  "")

	(internal-class-name . "Request")
	(types . (general-music event beam-event span-event))
	))
    (BreakEvent
     . (
	(description .  "")

	(internal-class-name . "Request")
	(types . (general-music break-event event))
	))
    (BreathingSignEvent
     . (
	(description .  "")

	(internal-class-name . "Request")
	(types . (general-music event breathing-event))
	)) 
    (BusyPlayingEvent
     . (
	(description .  "")

	(internal-class-name . "Request")
	(types . (general-music event busy-playing-event))
	)) 
    (ExtenderEvent
     . (
	(description .  "")

	(internal-class-name . "Request")
	(types . (general-music extender-event event))
	))   
    (GlissandoEvent
     . (
	(description .  "")

	(internal-class-name . "Request")
	(types . (general-music glissando-event event))
	)) 
    (GraceMusic
     . (
	(description .  "")

	(internal-class-name . "Grace_music")
	(iterator-ctor . ,Grace_iterator::constructor)
	(types . (grace-music music-wrapper-music general-music))
	))
   (HyphenEvent
     . (
	(description .  "")

	(internal-class-name . "Request")
	(types . (general-music hyphen-event event))
	))   
    (KeyChangeEvent
     . (
	(description .  "")

	(internal-class-name . "Key_change_req")
	(types . (general-music key-change-event event))
	)) 
    (LyricEvent
     . (
	(description .  "")

	(internal-class-name . "Lyric_req")
	(types . (general-music rhythmic-event event))
	))
    (LigatureEvent
     . (
	(description .  "")

	(internal-class-name . "Request")
	(span-type . ligature)
	(types . (general-music event span-event ligature-event))
	))
    (MarkEvent
     . (
	(description .  "")

	(internal-class-name . "Request")
	(types . (general-music mark-event event))
	))  
    (MelismaEvent
     . (
	(description .  "")

	(internal-class-name . "Request")
	(types . (general-music span-event melisma-playing-event event))
	)) 
    (MelismaPlayingEvent
     . (
	(description .  "")

	(internal-class-name . "Request")
	(types . (general-music event))
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

	(internal-class-name . "Request")
	(length . ,music-duration-length) 
	(compress-procedure . ,music-duration-compress)
	(types . (general-music event note-event rhythmic-event melodic-event))
	))
    (PorrectusEvent
     . (
	(description .  "")

	(internal-class-name . "Porrectus_req")
	(types . (general-music event))
	))
    (RepeatedMusic
     . (
	(description .  "")

	(internal-class-name . "Repeated_music")
	(type .  repeated-music)
	(types . (general-music repeat-music))
	))
    (Request
     . (
	(description .  "")

	(internal-class-name . "Request")
	(types . (general-music event))
	)) 
    (RestEvent
     . (
	(description .  "")

	(internal-class-name . "Request")
	(length . ,music-duration-length)
	(compress-procedure . ,music-duration-compress)
	(types . (general-music event rhythmic-event rest-event))
	)) 
    (RhythmicEvent
     . (
	(description .  "")

	(internal-class-name . "Rhythmic_req")
	(length . ,music-duration-length) 
	(compress-procedure . ,music-duration-compress)
	(types . (general-music rhythmic-event  event))
	)) 
    (SequentialMusic
     . (
	(description .  "")

	(internal-class-name . "Sequential_music")
	(iterator-ctor . ,Sequential_music_iterator::constructor)
	(types . (general-music sequential-music))
	))
    (SimultaneousMusic
     . (
	(description .  "")

	(internal-class-name . "Simultaneous_music")
	(iterator-ctor . ,Simultaneous_music_iterator::constructor)
	
	(types . (general-music simultaneous-music))
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
     (VoiceSeparator
      . (
	(description .  "")

	 (internal-class-name . "Music")
	 (types . (separator general-music))
	 ))
     (BarCheck
      . (
	(description .  "")

	 (internal-class-name . "Music")
	 (types . (general-music bar-check))
	 (iterator-ctor . ,Bar_check_iterator::constructor)
	 ))
     (OverrideProperty
      . (
	(description .  "")

	 (internal-class-name . "Music")
	 (types . (general-music layout-instruction))
	 (iterator-ctor . ,	Push_property_iterator::constructor)
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
    (ContextSpeccedMusic
     . (
	(description .  "")

	(internal-class-name . "Context_specced_music")
	(types . (context-specification general-music music-wrapper-music))
	))
    (AutoChangeMusic
     . (
	(description .  "")

	(internal-class-name . "Music_wrapper")
	(iterator-ctor . ,Auto_change_iterator::constructor)
	(types . (general-music music-wrapper-music auto-change-instruction))
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

    (LyricCombineMusic
     . (
	(description .  "")

	(internal-class-name . "Lyric_combine_music")
	(types . (general-music lyric-combine-music))
	(iterator-ctor . ,Lyric_combine_music_iterator::constructor)
	))

    (PartCombineMusic
     . (
	(description .  "")

	(internal-class-name . "Part_combine_music")
	(types . (general-music part-combine-music))
	(iterator-ctor . ,Part_combine_music_iterator::constructor)
     ))
    (RequestChord
     . (
	(description .  "")

	(internal-class-name . "Request_chord")
	(iterator-ctor . ,Request_chord_iterator::constructor)
	(types . (general-music simultaneous-music))
	)
     )
     
    (ScriptEvent
     . (
	(description .  "")

	(internal-class-name . "Script_req")
	(types . (general-music event))
	)) 
    (SkipEvent
     . (
	(description .  "")

	(internal-class-name . "Request")
	(length . ,music-duration-length) 
	(compress-procedure . ,music-duration-compress)
	(types . (general-music event rhythmic-event skip-event))
	)) 
    (SpanEvent
     . (
	(description .  "")

	(internal-class-name . "Span_req")
	(types . (general-music event))
	)) 
    (DecrescendoEvent
     . (
	(description .  "")

	(internal-class-name . "Request")
	(types . (general-music dynamic-event decrescendo-event event))
	)) 
    (CrescendoEvent
     . (
	(description .  "")

	(internal-class-name . "Request")
	(types . (general-music dynamic-event crescendo-event event))
	)) 
    (StringNumberEvent
     . (
	(description .  "")

	(internal-class-name . "Request")
	(types . (general-music string-number-event event))
	)) 
    (TempoEvent
     . (
	(description .  "")

	(internal-class-name . "Request")
	(types . (general-music tempo-event event))
	)) 
    (TextScriptEvent
     . (
	(description .  "")
	(internal-class-name . "Text_script_req")
	(types . (general-music script-event text-script-event event))
	)) 
    (TieEvent
     . (
	(description .  "A tie. Entered as ~.")
	(internal-class-name . "Request")
	(types . (general-music tie-event event))
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
      (misc-error "Not a symbol: ~s" x))
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

