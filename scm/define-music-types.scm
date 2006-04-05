;;;; define-music-types.scm --
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 1998--2006 Han-Wen Nienhuys <hanwen@cs.uu.nl>
;;;;		     Jan Nieuwenhuizen <janneke@gnu.org>

;; TODO: should link back into user manual.

(define-public music-descriptions
  `(
    (AbsoluteDynamicEvent
     . (
	(description . "Creates a dynamic mark.

Syntax: @var{note}@code{\\x},
where x is one of \\ppp, \\pp, \\p, \\mp, \\mf, \\f, \\ff, \\fff.")
	(types . (general-music event dynamic-event absolute-dynamic-event))
	))
    (ApplyContext
     . (
	(description . "Call the argument with the current context during interpreting phase")
	(types . (general-music apply-context))
	(iterator-ctor . ,ly:apply-context-iterator::constructor)
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
	(iterator-ctor . ,ly:output-property-music-iterator::constructor)
	(types . (general-music layout-instruction))
	))
    (ArpeggioEvent 
     . (
	(description .	"Make an arpeggio on this note.

Syntax:
@var{note}-@code{\\arpeggio}")
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

	(types . (general-music event articulation-event script-event))
	)) 
    (AutoChangeMusic
     . (
	(description .	"Used for making voices that switch between piano staves automatically.")
	(iterator-ctor . ,ly:auto-change-iterator::constructor)
	(start-callback . ,ly:music-wrapper::start-callback)
	(length-callback . ,ly:music-wrapper::length-callback)
	(types . (general-music music-wrapper-music auto-change-instruction))
	))
    (BarCheck
     . (
	(description .
		     "Check whether this music coincides with the start of the measure.")
	(types . (general-music bar-check))
	(iterator-ctor . ,ly:bar-check-iterator::constructor)
	))
    (BassFigureEvent
     . (
	(description .	"Print a bass-figure text")

	(types . (general-music event rhythmic-event bass-figure-event))
	))
    (BeamEvent
     . (
	(description .  "Starts or stops a beam.  

Syntax for manual control:
c8-[ c c-] c8")
	(types . (general-music event beam-event span-event))
	))
    (BreakEvent
     . (
	(description .  "Create a line break, Syntax: \\break or page break, Syntax: \\pagebreak.")

	(types . (general-music break-event event))
	))
    (BreathingSignEvent
     . (
	(description .	"Creates a `breath mark' or `comma'.  

Syntax:
@var{note}\\breathe.")

	(types . (general-music event breathing-event))
	)) 
    (BusyPlayingEvent
     . (
	(description .	"Used internally to signal beginning and ending of notes.")

	(types . (general-music event busy-playing-event))
	))
    (ContextChange
     . (
	(description .	"Change staffs in Piano staff. 

Syntax @code{\\translator Staff = @var{new-id}}.")
	(iterator-ctor . , ly:change-iterator::constructor)
	(types . (general-music translator-change-instruction))
	))

    (ClusterNoteEvent
     . ((description .	"A note that is part of a cluster.")

	;; not a note-event, to ensure that Note_engraver doesn't eat it. 
	(types . (general-music cluster-note-event melodic-event rhythmic-event event))
	))
    
    (ContextSpeccedMusic
     . (
	(description .	"Interpret the argument music within a specific context.")
	(iterator-ctor . ,ly:context-specced-music-iterator::constructor)
	(length-callback . ,ly:music-wrapper::length-callback)
	(start-callback . ,ly:music-wrapper::start-callback)
	(types . (context-specification general-music music-wrapper-music))
	))
    
    (CrescendoEvent
     . (
	(description .	"Begins or ends a crescendo.  

Syntax: @var{note}\\cr
... @var{note}\\rc (you can also use \\<, \\!, \\cresc, and
\\endcresc.  See the user manual for details.).")

	(types . (general-music dynamic-event crescendo-event event))
	)) 
    (DecrescendoEvent
     . (
	(description .	"See @ref{CrescendoEvent}.")

	(types . (general-music dynamic-event decrescendo-event event))
	))
    
    (ExtenderEvent
     . (
	(description .	"Extend lyrics.")

	(types . (general-music extender-event event))
	))

    
    (EventChord
     . (
	(description .	"Internally used to group a set of events.")
	(iterator-ctor . ,ly:event-chord-iterator::constructor)
	(length-callback . ,ly:music-sequence::maximum-length-callback)
	(to-relative-callback . ,ly:music-sequence::event-chord-relative-callback)
	(types . (general-music event-chord simultaneous-music))
	))

    (FingerEvent
     . (
	(description . "Specify what finger to use for this note.")
	(types . (general-music fingering-event event))
	))
    (BeamForbidEvent
     . (
	(description . "Specify that a note may not auto-beamed ")
	(types . (general-music event beam-forbid-event))
	))
    (GlissandoEvent
     . (
	(description .	"Start	a glissando on this note.")
	(types . (general-music glissando-event event))
	))
    
    (GraceMusic
     . (
	(description .	"Interpret the argument as grace notes. ")
	(start-callback . ,ly:grace-music::start-callback)
	(length . ,ZERO-MOMENT)
	(iterator-ctor . ,ly:grace-iterator::constructor)
	(types . (grace-music music-wrapper-music general-music))
	))
    (NoteGroupingEvent
     . (
	(description . "Start or stop grouping brackets.")
	(types . (general-music event note-grouping-event))
	))
    (HarmonicEvent
     . (
	(description . "Mark a note as harmonic")
	(types . (general-music event harmonic-event))
	))
    (HyphenEvent
     . (
	(description .	"A hyphen between lyric syllables.")

	(types . (general-music hyphen-event event))
	))
    
    (KeyChangeEvent
     . (
	(description .	"Change the key signature. 

Syntax: @code{\\key } @var{name} @var{scale}.")
	(to-relative-callback . ,(lambda (x p) p))
	(types . (general-music key-change-event event))
	))
    (LaissezVibrerEvent
     . ((description . "Don't damp this chord.

Syntax: @var{note}\\laissezVibrer.")

	(types . (general-music event laissez-vibrer-event))
	))
    (LigatureEvent
     . (
	(description .	"(docme).")
	(span-type . ligature)
	(types . (general-music span-event ligature-event event))
	))
    
    (OldLyricCombineMusic
     . (
	(description .	"Align lyrics to the start of notes.

Syntax @var{\\oldaddlyrics }@var{music} @var{lyrics}.")

	(types . (general-music lyric-combine-music))
	(length-callback . ,ly:lyric-combine-music::length-callback)
	(start-callback . ,ly:music-sequence::first-start-callback)
	(iterator-ctor . ,ly:old-lyric-combine-music-iterator::constructor)
	))
    
    (LyricCombineMusic
     . (
	(description .	"Align lyrics to the start of notes.

Syntax @var{\\lyricsto }@var{voicename} @var{lyrics}.")
	(length . ,ZERO-MOMENT)
	(types . (general-music lyric-combine-music))
	(iterator-ctor . ,ly:lyric-combine-music-iterator::constructor)
	))

    (LyricEvent
     . (
	(description .	"A lyric syllable. Must be entered in lyrics mode, i.e.
@code{\\lyrics @{ twinkle4 twinkle4 @} } .")

	(types . (general-music rhythmic-event lyric-event event))
	))
    (MarkEvent
     . (
	(description .	"Insert a rehearsal mark. 

Syntax: @code{\\mark} @var{marker},
e.g. @code{\\mark \"A\"}.")

	(types . (general-music mark-event event))
	))
    (MelismaPlayingEvent
     . (
	(description .	"Used internally to signal melismas.")
	(types . (general-music melisma-playing-event event))
	))
    (ManualMelismaEvent
     . (
	(description .	"Start or stop a melisma.

Syntax: @code{c4\\melisma d\\melismaEnd}.")
	(types . (general-music melisma-span-event event))
	))
    
    (MultiMeasureRestEvent
     . (
	(description . "Rests that may be compressed into Multi rests. 

Syntax
@code{R2.*4} for 4 measures in 3/4 time. Note the capital R.")
	(types . (general-music event rhythmic-event multi-measure-rest-event))
	))
    
    (MultiMeasureRestMusicGroup
     . (
	(description .	"Like sequential-music, but specifically intended
to group start-mmrest, skip, stop-mmrest sequence. 

Syntax @code{R2.*5} for 5 measures in 3/4 time.")
	(length-callback . ,ly:music-sequence::cumulative-length-callback)
	(start-callback . ,ly:music-sequence::first-start-callback)
	(iterator-ctor . ,ly:sequential-music-iterator::constructor)
	(types . (general-music sequential-music))
	))
    
    (MultiMeasureTextEvent
     . (
	(description . "Texts on mm rests. 

Syntax
@code{R-\\markup @{ \\roman \"bla\" @}}. Note the explicit font switch.")
	(types . (general-music event multi-measure-text-event))
	))

    (Music
     . (
	(description .	"Generic type for music expressions.")

	(types . (general-music)) 
	))
    (NoteEvent
     . (
	(description .	"A note.")

	(types . (general-music event note-event rhythmic-event melodic-event))
	))
    
    (OutputPropertySetMusic
     . (
	(description .	"Set grob properties in objects
individually. 

Syntax @code{\\outputproperty @var{predicate} @var{prop}
= @var{val}}.")

	(iterator-ctor . ,ly:output-property-music-iterator::constructor)
	(types . (general-music layout-instruction))
	))
    
    (OverrideProperty
     . (
	(description .	"Extend the definition of a graphical object.

SYNTAX

@code{\\override [ @var{Ctxt} . ] @var{Obj} @var{prop} = @var{val}}
")
	(types . (general-music layout-instruction))
	(iterator-ctor . ,ly:push-property-iterator::constructor)
	))
    (PartCombineMusic
     . (
	(description .	"Combine two parts on a staff, either merged or
as separate voices.")
	(length-callback . ,ly:music-sequence::maximum-length-callback)
	(start-callback . ,ly:music-sequence::minimum-start-callback)
	(types . (general-music part-combine-music))
	(iterator-ctor . ,ly:part-combine-iterator::constructor)
	))
    (PhrasingSlurEvent
     . (
	(description . "Start or end phrasing slur. 

Syntax NOTE \\(  and \\) NOTE")
	(types . (general-music span-event event phrasing-slur-event))
	))
    
    (PropertySet
     . (
	(description .	"Set a context property.

Syntax: @code{\\property @var{context}.@var{prop} = @var{scheme-val}}.")
	(types . (layout-instruction general-music))
	(iterator-ctor . ,ly:property-iterator::constructor)
	))

    (PropertyUnset
     . (
	(description .	"Remove the definition of a context @code{\\property}.")

	(types . (layout-instruction general-music))
	(iterator-ctor . ,ly:property-unset-iterator::constructor)
	))
    
    (PesOrFlexaEvent
     . (
	(description .	"Within a ligature, mark the previous and the
following note to form a pes (if melody goes up) or a flexa (if melody
goes down).")

	(types . (general-music pes-or-flexa-event event))
	))

    (QuoteMusic
     . (
	(description . "Quote preprocessed snippets of music. ")
	(iterator-ctor . ,ly:music-wrapper-iterator::constructor)
	(length-callback . ,ly:music-wrapper::length-callback)
	(start-callback . ,ly:music-wrapper::start-callback)
	(types . (general-music music-wrapper-music))
	))
    
    (RelativeOctaveCheck
     . ((description . "Check if a pitch is in the correct octave.")
	(to-relative-callback . ,ly:relative-octave-check::relative-callback)
	(types . (general-music relative-octave-check))
	))
    
    (RepeatedMusic
     . (
	(description .	"Repeat music in different ways")
	(types . (general-music repeated-music))
	))
    (RepeatTieEvent
     . (
	(description . "Ties for starting a second volta bracket.")
	(types . (general-music event repeat-tie-event))
	))
    (Event
     . (
	(description .	"Atomic music event.")
	(types . (general-music event))
	))
    
    (RestEvent
     . (
	(description .	"A Rest. 

Syntax @code{r4} for a quarter rest. ")

	(types . (general-music event rhythmic-event rest-event))
	)) 
    (RevertProperty
     . (
	(description .	"The opposite of @ref{OverrideProperty}: remove a
previously added property from a graphical object definition
 ")

	(types . (general-music layout-instruction))
	(iterator-ctor . ,	ly:pop-property-iterator::constructor)
	))

    (SequentialMusic
     . (
	(description .	"Music expressions concatenated. 

Syntax \\sequential @{..@} or simply @{..@} .")

	(length-callback . ,ly:music-sequence::cumulative-length-callback)
	(start-callback . ,ly:music-sequence::first-start-callback)
	(iterator-ctor . ,ly:sequential-music-iterator::constructor)
	(types . (general-music sequential-music))
	))

    (SoloOneEvent
     . (
	(description . "Print Solo.1")
	(part-combine-status . solo1)
	(types . (general-music event part-combine-event))
	))
    (SoloTwoEvent
     . (
	(description . "Print Solo.2")
	(part-combine-status . solo2)
	(types . (general-music event part-combine-event))
	))
    (UnisonoEvent
     . ((description . "Print a2")
	(part-combine-status . unisono)
	(types . (general-music event part-combine-event))))
    
    (SimultaneousMusic
     . (
	(description .	"Music playing together.

SYNTAX

@code{ \\simultaneous @{ .. @}} or << .. >>.")

	(iterator-ctor . ,ly:simultaneous-music-iterator::constructor)
	(start-callback . ,ly:music-sequence::minimum-start-callback)
	(length-callback . ,ly:music-sequence::maximum-length-callback)
	(to-relative-callback . ,ly:music-sequence::simultaneous-relative-callback)
	
	(types . (general-music simultaneous-music))
	))
    
    (SlurEvent
     . (
	(description . "Start or end slur. 

Syntax NOTE(	 and NOTE) ")

	(types . (general-music span-event event slur-event))
	))
    
    (StaffSpanEvent
     . ((description . "Start or  stop a staff symbol.")
	(types . (general-music event span-event staff-span-event))
     ))
    
    (StartPlayingEvent
     . (
	(description .	"Used internally to signal beginning of notes.")

	(types . (general-music event start-playing-event))
	))
    
    (TextSpanEvent
     . (
	(description . "Start a text spanner like 8va.....|")
	(types . (general-music span-event event text-span-event))
	))
    
    (TrillSpanEvent
     . (
	(description . "Start a trill spanner tr~~~")
	(types . (general-music span-event event trill-span-event))
	))
    
    (TimeScaledMusic
     . (
	(description .	"Multiply durations, as in tuplets. 

Syntax @code{\\times @var{fraction} @var{music}}, e.g.
@code{\\times 2/3 @{ ... @}} for triplets.
 ")
	(length-callback . ,ly:music-wrapper::length-callback)
	(start-callback . ,ly:music-wrapper::start-callback)
	(iterator-ctor . ,ly:time-scaled-music-iterator::constructor)
	(types . (time-scaled-music music-wrapper-music general-music))
	))
    
    (TransposedMusic
     . (
	(description .	"Music that has been transposed.")
	(iterator-ctor . ,ly:music-wrapper-iterator::constructor)
	(start-callback . ,ly:music-wrapper::start-callback)
	(length-callback . ,ly:music-wrapper::length-callback)
	(to-relative-callback . ,ly:relative-octave-music::no-relative-callback)
	(types . (music-wrapper-music general-music transposed-music))
	))

    (UnrelativableMusic
     . (
	(description .	"Music that can not be converted from relative to absolute notation.
For example, transposed music.")
	(to-relative-callback . ,ly:relative-octave-music::no-relative-callback)
	(iterator-ctor . ,ly:music-wrapper-iterator::constructor)
	(length-callback . ,ly:music-wrapper::length-callback)
	(types . (music-wrapper-music general-music unrelativable-music))
	))

    (RelativeOctaveMusic
     . (
	(description .	"Music that was entered in relative octave notation.")
	(to-relative-callback . ,ly:relative-octave-music::relative-callback)
	(iterator-ctor . ,ly:music-wrapper-iterator::constructor)
	(length-callback . ,ly:music-wrapper::length-callback)
	(start-callback . ,ly:music-wrapper::start-callback)
	(types . (music-wrapper-music general-music relative-octave-music))
	))
    (ScriptEvent
     . (
	(description .	"Add an articulation mark to a note. ")

	(types . (general-music event))
	))

    (SkipMusic
     . (
	(description .	"Filler that takes up duration, does not print anything, and also
does not create staffs or voices implicitly.



Syntax: @code{\\skip }@var{duration}.")
	(length-callback . ,ly:music-duration-length)
	(iterator-ctor . ,ly:simple-music-iterator::constructor)
	(types . (general-music event rhythmic-event skip-event))
	))
    
    (SkipEvent
     . (
	(description .	"Filler that takes up duration, but does not print anything.



Syntax: @code{s}@var{duration}")

	(types . (general-music event rhythmic-event skip-event))
	))
    (SpanEvent
     . (
	(description .	"Event for anything that is started at a different time than stopped.")

	(types . (general-music event))
	))
    
    (SustainEvent
     . (
	(description . "Depress or release sustain pedal. ")
	(types . (general-music event pedal-event sustain-pedal-event))
	))
    
    (SostenutoEvent
     . (
	(description . "Depress or release sostenuto pedal. ")
	(types . (general-music event pedal-event sostenuto-pedal-event))
	))
    
    (UnaCordaEvent
     . (
	(description . "Depress or release una-corda pedal.")
	(types . (general-music event pedal-event una-corda-pedal-event))
	))
    
    (StringNumberEvent
     . (
	(description .	"Specify on which string to play this note. 

Syntax: @code{\\@var{number}}.")

	(types . (general-music string-number-event event))
	)) 

    (MetronomeChangeEvent
     . (
	(description .	"Change tempo setting (in beats per minute).")
	(types . (general-music metronome-change-event tempo-event event))
	))
    
    (TextScriptEvent
     . (
	(description .	"")
	(types . (general-music script-event text-script-event event))
	)) 
    (TieEvent
     . (
	(description .	"A tie.	 Entered as @var{note}-~.")
	(types . (general-music tie-event event))
	))
    (TremoloEvent
     . (
	(description . "Un measured tremolo.")
	(types . (general-music event tremolo-event))
	))
    
    (VoiceSeparator
     . (
	(description .	"Separate polyphonic voices in simultaneous music. 

Syntax: @code{\\\\}")

	(types . (separator general-music))
	))

    (VoltaRepeatedMusic
     . (
	(iterator-ctor . ,ly:volta-repeat-iterator::constructor)
	(description . "")
	(start-callback .  ,ly:repeated-music::first-start)
	(length-callback . ,ly:repeated-music::volta-music-length)
	(types . (general-music repeated-music volta-repeated-music))
	))
    
    (UnfoldedRepeatedMusic
     . (
	(iterator-ctor . ,ly:unfolded-repeat-iterator::constructor)
	(description .	"")
	(start-callback .  ,ly:repeated-music::first-start)
	(types . (general-music repeated-music unfolded-repeated-music))
	(length-callback . ,ly:repeated-music::unfolded-music-length)
	))
    (PercentRepeatedMusic
     . (
	(description .	"Repeats encoded by percents.")
	(iterator-ctor . ,ly:percent-repeat-iterator::constructor)
	(start-callback .  ,ly:repeated-music::first-start)
	(length-callback . ,ly:repeated-music::unfolded-music-length)
	(types . (general-music repeated-music percent-repeated-music))
	))
    
    (TremoloRepeatedMusic
     . (
	(iterator-ctor . ,ly:chord-tremolo-iterator::constructor)
	(description .	"Repeated notes denoted by tremolo beams.")
	(start-callback .  ,ly:repeated-music::first-start)

	;; the length of the repeat is handled by shifting the note logs
	(length-callback . ,ly:repeated-music::folded-music-length)
	(types . (general-music repeated-music tremolo-repeated-music))
	
	))
    
    (FoldedRepeatedMusic
     . (
	(description .	"Repeats with alternatives placed in parallel. ")
	(iterator-ctor	. ,ly:folded-repeat-iterator::constructor)
	(start-callback .  ,ly:repeated-music::minimum-start)
	(length-callback . ,ly:repeated-music::folded-music-length)
	(types . (general-music repeated-music folded-repeated-music))
	))
    ))

(set! music-descriptions
      (sort music-descriptions alist<?))

(define-public music-name-to-property-table (make-vector 59 '()))

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

(define-safe-public (make-music name . music-properties)
  "Create a music object of given name, and set its properties
according to `music-properties', a list of alterning property symbols
and values. E.g:
  (make-music 'OverrideProperty 
	      'symbol 'Stem
	      'grob-property 'thickness
	      'grob-value (* 2 1.5))"
  (if (not (symbol? name))
      (ly:error (_ "symbol expected: ~S") name))
  (let ((props (hashq-ref music-name-to-property-table name '())))
    (if (not (pair? props))
	(ly:error (_ "can't find music object: ~S") name))
    (let ((m (ly:make-music props)))
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
			   (ly:warning (_ "unknown repeat type `~S'") name)
			   (ly:warning (_ "See music-types.scm for supported repeats"))
			   'VoltaRepeatedMusic))))
    (make-music music-name)))

