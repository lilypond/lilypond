;;;; define-music-types.scm --
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c)  1998--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
;;;;		     Jan Nieuwenhuizen <janneke@gnu.org>

;; TODO: should link back into user manual.

(define-public music-descriptions
  `(
    (AbsoluteDynamicEvent
     . (
	(description . "Creates a dynamic mark.

Syntax: @var{note}@code{\\x},
where x is one of \\ppp, \\pp, \\p, \\mp, \\mf, \\f, \\ff, \\fff.")
	(internal-class-name . "Event")
	(types . (general-music event dynamic-event absolute-dynamic-event))
	))
    (ApplyContext
     . (
	(description . "Call the argument with the current context during interpreting phase")
	(internal-class-name . "Music")
	(types . (general-music apply-context))
	(iterator-ctor . ,Apply_context_iterator::constructor)
	))
    (ApplyOutputEvent
     . (
	(description . "
Call the argument with all current grobs during interpreting phase.

Syntax

\\applyoutput FUNC

arguments to func are 1. the grob, 2. the originating context,
3. context where FUNC is called.

")
	(internal-class-name . "Event")
	(iterator-ctor . ,Output_property_music_iterator::constructor)
	(types . (general-music layout-instruction))
	))
    (ArpeggioEvent 
     . (
	(description .	"Make an arpeggio on this note.

Syntax:
@var{note}-@code{\\arpeggio}")
	(internal-class-name .	"Event")
	(types . (general-music arpeggio-event event))
	))

    ;; todo: use articulation-event for slur as well.
    ;; separate non articulation scripts  
    (ArticulationEvent
     . (
	(description .  "Adds an articulation marking to a note.  

Syntax:
@var{note}@code{X}@code{Y}, where X is a direction (up @code{^}, down
@code{_}, or LilyPond's choice (no direction specified)), and where Y
is an articulation (such as @code{-.}, @code{->}, @code{\\tenuto},
@code{\\downbow}).  See the user manual for details.")

	(internal-class-name . "Event")
	(types . (general-music event articulation-event script-event))
	)) 
    (AutoChangeMusic
     . (
	(description .	"Used for making voices that switch between piano staves automatically.")

	(internal-class-name . "Music_wrapper")
	(iterator-ctor . ,Auto_change_iterator::constructor)
	(types . (general-music music-wrapper-music auto-change-instruction))
	))
    (BarCheck
     . (
	(description .
		     "Check whether this music coincides with the start of the measure.")
	(internal-class-name . "Music")
	(types . (general-music bar-check))
	(iterator-ctor . ,Bar_check_iterator::constructor)
	))
    (BassFigureEvent
     . (
	(description .	"Print a bass-figure text")

	(internal-class-name . "Event")
	(types . (general-music event rhythmic-event bass-figure-event))
	))
    (BeamEvent
     . (
	(description .  "Starts or stops a beam.  

Syntax for manual control:
c8-[ c c-] c8")
	(internal-class-name . "Event")
	(types . (general-music event beam-event span-event))
	))
    (BreakEvent
     . (
	(description .  "Create a line break, Syntax: \\break or page break, Syntax: \\pagebreak.")

	(internal-class-name . "Event")
	(types . (general-music break-event event))
	))
    (BreathingSignEvent
     . (
	(description .	"Creates a `breath mark' or `comma'.  

Syntax:
@var{note}\\breathe.")

	(internal-class-name . "Event")
	(types . (general-music event breathing-event))
	)) 
    (BusyPlayingEvent
     . (
	(description .	"Used internally to signal beginning and ending of notes.")

	(internal-class-name . "Event")
	(types . (general-music event busy-playing-event))
	))
    (ContextChange
     . (
	(description .	"Change staffs in Piano staff. 

Syntax @code{\\translator Staff = @var{new-id}}.")
	(internal-class-name . "Music")
	(iterator-ctor . , Change_iterator::constructor)
	(types . (general-music translator-change-instruction))
	))

    (ClusterNoteEvent
     . (
	(description .	"A note that is part of a cluster.")
	(internal-class-name . "Event")

					; not a note-event, to ensure that Note_engraver doesn't eat it. 
	(types . (general-music cluster-note-event melodic-event rhythmic-event event))
	))
    
    (ContextSpeccedMusic
     . (
	(description .	"Interpret the argument music within a specific context.")
	(iterator-ctor . ,Context_specced_music_iterator::constructor)
	(internal-class-name . "Music_wrapper")
	(types . (context-specification general-music music-wrapper-music))
	))
    
    (CrescendoEvent
     . (
	(description .	"Begins or ends a crescendo.  

Syntax: @var{note}\\cr
... @var{note}\\rc (you can also use \\<, \\!, \\cresc, and
\\endcresc.  See the user manual for details.).")

	(internal-class-name . "Event")
	(types . (general-music dynamic-event crescendo-event event))
	)) 
    (DecrescendoEvent
     . (
	(description .	"See @ref{CrescendoEvent}.")

	(internal-class-name . "Event")
	(types . (general-music dynamic-event decrescendo-event event))
	))
    
    (ExtenderEvent
     . (
	(description .	"Extend lyrics.")

	(internal-class-name . "Event")
	(types . (general-music extender-event event))
	))
    (FingerEvent
     . (
	(description . "Specify what finger to use for this note.")
	(internal-class-name . "Event")
	(types . (general-music fingering-event event))
	))
    (BeamForbidEvent
     . (
	(description . "Specify that a note may not auto-beamed ")
	(internal-class-name . "Event")
	(types . (general-music event beam-forbid-event))
	))
    (GlissandoEvent
     . (
	(description .	"Start	a glissando on this note.")
	(internal-class-name . "Event")
	(types . (general-music glissando-event event))
	))
    
    (GraceMusic
     . (
	(description .	"Interpret the argument as grace notes. ")
	(internal-class-name . "Music_wrapper")
	(start-callback . ,Grace_music::start_callback)
	(length . ,(ly:make-moment 0 1))
	(iterator-ctor . ,Grace_iterator::constructor)
	(types . (grace-music music-wrapper-music general-music))
	))
    (NoteGroupingEvent
     . (
	(description . "Start or stop grouping brackets.")
	(internal-class-name . "Event")
	(types . (general-music event note-grouping-event))
	))
    (HarmonicEvent
     . (
	(description . "Mark a note as harmonic")
	(internal-class-name . "Event")
	(types . (general-music event harmonic-event))
	))
    (HyphenEvent
     . (
	(description .	"A hyphen between lyric syllables.")

	(internal-class-name . "Event")
	(types . (general-music hyphen-event event))
	))
    
    (KeyChangeEvent
     . (
	(description .	"Change the key signature. 

Syntax: @code{\\key } @var{name} @var{scale}.")

	(internal-class-name . "Key_change_ev")
	(types . (general-music key-change-event event))
	))
    
    (LigatureEvent
     . (
	(description .	"(docme).")

	(internal-class-name . "Event")
	(span-type . ligature)
	(types . (general-music span-event ligature-event event))
	))
    
    (LyricCombineMusic
     . (
	(description .	"Align lyrics to the start of notes.


Syntax @var{\\addlyrics }@var{music} @var{lyrics}.")

	(internal-class-name . "Lyric_combine_music")
	(types . (general-music lyric-combine-music))
	(iterator-ctor . ,Lyric_combine_music_iterator::constructor)
	))
    
    (NewLyricCombineMusic
     . (
	(description .	"Align lyrics to the start of notes.


Syntax @var{\\addlyrics }@var{music} @var{lyrics}.")
	(internal-class-name . "Music")
	(length . ,(ly:make-moment 0 1))
	(types . (general-music lyric-combine-music))
	(iterator-ctor . ,New_lyric_combine_music_iterator::constructor)
	))

    (LyricEvent
     . (
	(description .	"A lyric syllable. Must be entered in lyrics mode, i.e.
@code{\\lyrics @{ twinkle4 twinkle4 @} } .")

	(internal-class-name . "Event")
	(types . (general-music rhythmic-event lyric-event event))
	))
    (MarkEvent
     . (
	(description .	"Insert a rehearsal mark. 

Syntax: @code{\\mark} @var{marker},
e.g. @code{\\mark \"A\"}.")

	(internal-class-name . "Event")
	(types . (general-music mark-event event))
	))
    (MelismaPlayingEvent
     . (
	(description .	"Used internally to signal melismas.")
	(internal-class-name . "Event")
	(types . (general-music melisma-playing-event event))
	))
    (ManualMelismaEvent
     . (
	(description .	"Start or stop a melisma.

Syntax: @code{c4\\melisma d\\melismaEnd}.")
	(internal-class-name . "Event")
	(types . (general-music melisma-span-event event))
	))
    
    (MultiMeasureRestEvent
     . (
	(description . "Rests that may be compressed into Multi rests. 

Syntax
@code{R2.*4} for 4 measures in 3/4 time. Note the capital R.")
	(internal-class-name . "Event")
	(types . (general-music event rhythmic-event multi-measure-rest-event))
	))
    
    (MultiMeasureTextEvent
     . (
	(description . "Texts on mm rests. 

Syntax
@code{R-\\markup @{ \\roman \"bla\" @}}. Note the explicit font switch.")
	(internal-class-name . "Event")
	(types . (general-music event multi-measure-text-event))
	))

    (Music
     . (
	(description .	"Generic type for music expressions.")

	(internal-class-name . "Music")
	(types . (general-music)) 
	))
    (NoteEvent
     . (
	(description .	"A note.")

	(internal-class-name . "Event")
	(types . (general-music event note-event rhythmic-event melodic-event))
	))
    
    (OverrideProperty
     . (
	(description .	"Extend the definition of a graphical object.

SYNTAX

@code{\\propery Foo.Bar \\override} @var{SYMBOL} = @var{VALUE}

")
	(internal-class-name . "Music")
	(types . (general-music layout-instruction))
	(iterator-ctor . ,	Push_property_iterator::constructor)
	))
    (PartCombineMusic
     . (
	(description .	"Combine two parts on a staff, either merged or
as separate voices.")
	(internal-class-name . "Simultaneous_music")
	(length-callback . ,Music_sequence::maximum_length_callback)
	(start-callback . ,Music_sequence::minimum_start_callback)
	(types . (general-music part-combine-music))
	(iterator-ctor . ,Part_combine_iterator::constructor)
	))
    (PhrasingSlurEvent
     . (
	(description . "Start or end phrasing slur. 

Syntax NOTE \\(  and \\) NOTE")
	(internal-class-name . "Event")
	(types . (general-music span-event phrasing-slur-event))
	))
    
    (PropertySet
     . (
	(description .	"Set a context property.

Syntax: @code{\\property @var{context}.@var{prop} = @var{scheme-val}}.")
	(internal-class-name . "Music")
	(types . (layout-instruction general-music))
	(iterator-ctor . ,Property_iterator::constructor)
	))

    (PropertyUnset
     . (
	(description .	"Remove the definition of a context @code{\\property}.")

	(internal-class-name . "Music")
	(types . (layout-instruction general-music))
	(iterator-ctor . ,Property_unset_iterator::constructor)
	))
    
    (PesOrFlexaEvent
     . (
	(description .	"Within a ligature, mark the previous and the
following note to form a pes (if melody goes up) or a flexa (if melody
goes down).")

	(internal-class-name . "Event")
	(types . (general-music pes-or-flexa-event event))
	))

    (QuoteMusic
     . (
	(description . "Quote preprocessed snippets of music. ")
	(internal-class-name . "Music_wrapper")
	(iterator-ctor . ,Quote_iterator::constructor)
	(types . (general-music))
	))
    
    (RelativeOctaveCheck
     . ((description . "Check if a pitch is in the correct octave.")
	(internal-class-name . "Relative_octave_check")
	(types . (general-music relative-octave-check))
	))
    
    (RepeatedMusic
     . (
	(description .	"Repeat music in different ways")
	(type .	 repeated-music)
	(types . (general-music repeated-music))
	))
    
    (Event
     . (
	(description .	"Atomic music event.")

	(internal-class-name . "Event")
	(types . (general-music event))
	))
    
    (RestEvent
     . (
	(description .	"A Rest. 

Syntax @code{r4} for a quarter rest. ")

	(internal-class-name . "Event")
	(types . (general-music event rhythmic-event rest-event))
	)) 
    (RevertProperty
     . (
	(description .	"The opposite of @ref{OverrideProperty}: remove a
previously added property from a graphical object definition
 ")

	(internal-class-name . "Music")
	(types . (general-music layout-instruction))
	(iterator-ctor . ,	Pop_property_iterator::constructor)
	))

    (SequentialMusic
     . (
	(description .	"Music expressions concatenated. 

Syntax \\sequential @{..@} or simply @{..@} .")

	(internal-class-name . "Sequential_music")
	(length-callback . ,Music_sequence::cumulative_length_callback)
	(start-callback . ,Music_sequence::first_start_callback)
	(iterator-ctor . ,Sequential_music_iterator::constructor)
	(types . (general-music sequential-music))
	))
    
    (MultiMeasureRestMusicGroup
     . (
	(description .	"Like sequential-music, but specifically intended
to group start-mmrest, skip, stop-mmrest sequence. 

Syntax @code{R2.*5} for 5 measures in 3/4 time.")
	(internal-class-name . "Sequential_music")
	(length-callback . ,Music_sequence::cumulative_length_callback)
	(start-callback . ,Music_sequence::first_start_callback)
	(iterator-ctor . ,Sequential_music_iterator::constructor)
	(types . (general-music sequential-music))
	))

    (SoloOneEvent
     . (
	(description . "Print Solo.1")
	(internal-class-name . "Event")
	(part-combine-status . solo1)
	(types . (general-music event part-combine-event))
	))
    (SoloTwoEvent
     . (
	(description . "Print Solo.2")
	(internal-class-name . "Event")
	(part-combine-status . solo2)
	(types . (general-music event part-combine-event))
	))
    (UnisonoEvent
     . ((description . "Print a2")
	(internal-class-name .	"Event")
	(part-combine-status . unisono)
	(types . (general-music event part-combine-event))))
    
    (SimultaneousMusic
     . (
	(description .	"Music playing together.

SYNTAX

@code{ \\simultaneous @{ .. @}} or < .. >.")

	(internal-class-name . "Simultaneous_music")
	(iterator-ctor . ,Simultaneous_music_iterator::constructor)
	(start-callback . ,Music_sequence::minimum_start_callback)
	(length-callback . ,Music_sequence::maximum_length_callback)
	
	(types . (general-music simultaneous-music))
	))
    
    (SlurEvent
     . (
	(description . "Start or end slur. 

Syntax NOTE(	 and )NOTE")
	(internal-class-name . "Event")
	(types . (general-music span-event slur-event))
	))

    (StartPlayingEvent
     . (
	(description .	"Used internally to signal beginning of notes.")

	(internal-class-name . "Event")
	(types . (general-music event start-playing-event))
	))
    
    (OutputPropertySetMusic
     . (
	(description .	"Set grob properties in objects
individually. 

Syntax @code{\\outputproperty @var{predicate} @var{prop}
= @var{val}}.")

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
    
    (TrillSpanEvent
     . (
	(description . "Start a trill spanner tr~~~")
	(internal-class-name . "Event")
	(types . (general-music span-event trill-span-event))
	))
    
    (TimeScaledMusic
     . (
	(description .	"Multiply durations, as in tuplets. 

Syntax @code{\\times @var{fraction} @var{music}}, e.g.
@code{\\times 2/3 @{ ... @}} for triplets.
 ")
	(internal-class-name . "Music_wrapper")
	(iterator-ctor . ,Time_scaled_music_iterator::constructor)
	(types . (time-scaled-music music-wrapper-music general-music))
	))
    
    (TransposedMusic
     . (
	(description .	"Music that has been transposed.")
	(internal-class-name . "Transposed_music")
	(types . (music-wrapper-music general-music transposed-music))
	))

    (UntransposableMusic
     . (
	(description .	"Music that can not be transposed.")

	(internal-class-name . "Untransposable_music")
	(types . (music-wrapper-music general-music untransposable-music)) 
	))

    (UnrelativableMusic
     . (
	(description .	"Music that can not be converted from relative to absolute notation.
For example, transposed music.")
	(internal-class-name . "Un_relativable_music")
	(types . (music-wrapper-music general-music unrelativable-music))
	))

    (RelativeOctaveMusic
     . (
	(description .	"Music that was entered in relative octave notation.")

	(internal-class-name . "Relative_octave_music")
	(types . (music-wrapper-music general-music relative-octave-music))
	))
    
    (EventChord
     . (
	(description .	"Internally used to group a set of events.")
	(internal-class-name . "Event_chord")
	(iterator-ctor . ,Event_chord_iterator::constructor)
	(length-callback . ,Music_sequence::maximum_length_callback)
	(types . (general-music event-chord simultaneous-music))
	)
     )
    
    (ScriptEvent
     . (
	(description .	"Add an articulation mark to a note. ")

	(internal-class-name . "Event")
	(types . (general-music event))
	))

    (SkipMusic
     . (
	(description .	"Filler that takes up duration, does not print anything, and also
does not create staffs or voices implicitly.



Syntax: @code{\\skip }@var{duration}.")
	(internal-class-name . "Music")
	(length-callback . ,ly:music-duration-length)
	(iterator-ctor . ,Simple_music_iterator::constructor)
	(types . (general-music event rhythmic-event skip-event))
	))
    
    (SkipEvent
     . (
	(description .	"Filler that takes up duration, but does not print anything.



Syntax: @code{s}@var{duration}")

	(internal-class-name . "Event")
	(types . (general-music event rhythmic-event skip-event))
	))
    (SpanEvent
     . (
	(description .	"Event for anything that is started at a different time than stopped.")

	(internal-class-name . "Event")
	(types . (general-music event))
	))
    
    (SustainEvent
     . (
	(description . "Depress or release sustain pedal. ")
	(internal-class-name . "Event")
	(types . (general-music pedal-event sustain-pedal-event))
	))
    
    (SostenutoEvent
     . (
	(description . "Depress or release sostenuto pedal. ")
	(internal-class-name . "Event")
	(types . (general-music pedal-event sostenuto-pedal-event))
	))
    
    (UnaCordaEvent
     . (
	(description . "Depress or release una-corda pedal.")
	(internal-class-name . "Event")
	(types . (general-music pedal-event una-corda-pedal-event))
	))
    
    (StringNumberEvent
     . (
	(description .	"Specify on which string to play this note. 

Syntax: @code{\\@var{number}}.")

	(internal-class-name . "Event")
	(types . (general-music string-number-event event))
	)) 

    (MetronomeChangeEvent
     . (
	(description .	"Change tempo setting (in beats per minute).")
	(internal-class-name . "Event")
	(types . (general-music metronome-change-event tempo-event event))
	))
    
    (TextScriptEvent
     . (
	(description .	"")
	(internal-class-name . "Event")
	(types . (general-music script-event text-script-event event))
	)) 
    (TieEvent
     . (
	(description .	"A tie.	 Entered as @var{note}-~.")
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
	(description .	"Separate polyphonic voices in simultaneous music. 

Syntax: @code{\\\\}")

	(internal-class-name . "Music")
	(types . (separator general-music))
	))

    (VoltaRepeatedMusic
     . (
	(iterator-ctor . ,Volta_repeat_iterator::constructor)
	(internal-class-name . "Repeated_music")
	(description . "")
	(start-callback .  ,Repeated_music::first_start)
	(length-callback . ,Repeated_music::volta_music_length)
	(types . (general-music repeated-music volta-repeated-music))
	))
    
    (UnfoldedRepeatedMusic
     . (
	(iterator-ctor . ,Unfolded_repeat_iterator::constructor)
	(description .	"")
	(start-callback .  ,Repeated_music::first_start)
	(internal-class-name . "Repeated_music")
	(types . (general-music repeated-music unfolded-repeated-music))
	(length-callback . ,Repeated_music::unfolded_music_length)
	))
    (PercentRepeatedMusic
     . (
	(internal-class-name . "Repeated_music")
	(description .	"Repeats encoded by percents.")
	(iterator-ctor . ,Percent_repeat_iterator::constructor)
	(start-callback .  ,Repeated_music::first_start)
	(length-callback . ,Repeated_music::unfolded_music_length)
	(types . (general-music repeated-music percent-repeated-music))
	))
    
    (TremoloRepeatedMusic
     . (
	(iterator-ctor . ,Chord_tremolo_iterator::constructor)
	(description .	"Repeated notes denoted by tremolo beams.")
	(internal-class-name . "Repeated_music")
	(start-callback .  ,Repeated_music::first_start)

	;; the length of the repeat is handled by shifting the note logs
	(length-callback . ,Repeated_music::folded_music_length)
	(types . (general-music repeated-music tremolo-repeated-music))
	
	))
    
    (FoldedRepeatedMusic
     . (
	(internal-class-name . "Repeated_music")
	(description .	"Repeats with alternatives placed in parallel. ")
	(iterator-ctor	. ,Folded_repeat_iterator::constructor)
	(start-callback .  ,Repeated_music::minimum_start)
	(length-callback . ,Repeated_music::folded_music_length)
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
	(let ((lst (cdr x)))
	  (set! lst (assoc-set! lst 'name (car x)))
	  (set! lst (assq-remove! lst 'description))
	  (hashq-set! music-name-to-property-table (car x) lst)
	  (cons (car x) lst)))
      music-descriptions))

(define-public (make-music name . music-properties)
  "Create a music object of given name, and set its properties
according to `music-properties', a list of alterning property symbols
and values. E.g:
  (make-music 'OverrideProperty 
	      'symbol 'Stem
	      'grob-property 'thickness
	      'grob-value (* 2 1.5))"
  (if (not (symbol? name))
      (error (format "Not a symbol: ~a" name)))
  (let ((props (hashq-ref music-name-to-property-table name '())))
    (if (not (pair? props))
	(error "Can not find music object" name))
    (let ((m (ly:make-bare-music (cdr (assoc 'internal-class-name props)) props)))
      (define (set-props mus-props)
	(if (and (not (null? mus-props))
		 (not (null? (cdr mus-props))))
	    (begin
	      (set! (ly:music-property m (car mus-props)) (cadr mus-props))
	      (set-props (cddr mus-props)))))
      (set-props music-properties)
      m)))

(define-public (make-repeated-music name)
  (let* ((handle (assoc name '(("volta" . VoltaRepeatedMusic)
			       ("unfold" . UnfoldedRepeatedMusic)
			       ("percent" . PercentRepeatedMusic)
			       ("tremolo" . TremoloRepeatedMusic)
			       ("fold" . FoldedRepeatedMusic))))
	 (music-name (if (pair? handle)
			 (cdr handle)
			 (begin
			   (ly:warn "Unknown repeat type `~S'\nSee music-types.scm for supported repeats" name)
			   'VoltaRepeatedMusic))))
    (make-music music-name)))

