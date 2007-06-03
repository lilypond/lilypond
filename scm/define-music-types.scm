;;;; define-music-types.scm --
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 1998--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;;		     Jan Nieuwenhuizen <janneke@gnu.org>

;; TODO: should link back into user manual.

(define (mm-rest-child-list music)
  "Generate events for multimeasure rests, to be used by the sequential-iterator"
  (let ((location (ly:music-property music 'origin))
	(duration (ly:music-property music 'duration)))
    (list (make-music 'BarCheck
		      'origin location)
	  (make-event-chord (cons (make-music 'MultiMeasureRestEvent
					      'origin location
					      'duration duration)
				  (ly:music-property music 'articulations)))
	  (make-music 'BarCheck
		      'origin location))))

(define-public music-descriptions
  `(
    (AbsoluteDynamicEvent
     . (
	(description . "Creates a dynamic mark.

Syntax: @var{note}@code{\\x},
where x is one of \\ppp, \\pp, \\p, \\mp, \\mf, \\f, \\ff, \\fff.")
	(types . (general-music event dynamic-event absolute-dynamic-event))
	))

    (AnnotateOutputEvent
     . ((description . "Print an annotation of an output element.")
	(types . (general-music event annotate-output-event))
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

\\applyOutput #'Context FUNC

arguments to func are 1. the grob, 2. the originating context,
3. context where FUNC is called.

")
	(types . (general-music event apply-output-event))
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
    (BendAfterEvent
     . ((description . "A drop/fall/doit jazz articulation")
	(types . (general-music bend-after-event event))))

    (BreathingEvent
     . (
	(description .	"Creates a `breath mark' or `comma'.  

Syntax:
@var{note}\\breathe.")

	(types . (general-music event breathing-event))
	)) 
    (ContextChange
     . (
	(description .	"Change staves in Piano staff. 

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

	(types . (general-music span-event span-dynamic-event crescendo-event event))
	)) 
    (DecrescendoEvent
     . (
	(description .	"See @ref{CrescendoEvent}.")

	(types . (general-music span-event span-dynamic-event decrescendo-event event))
	))
    
    (ExtenderEvent
     . (
	(description .	"Extend lyrics.")

	(types . (general-music extender-event event))
	))

    (Event
     . (
	(description .	"Atomic music event.")
	(types . (general-music event))
	))
        
    (EventChord
     . (
	(description .	"Internally used to group a set of events.")
	(iterator-ctor . ,ly:event-chord-iterator::constructor)
	(length-callback . ,ly:music-sequence::maximum-length-callback)
	(to-relative-callback . ,ly:music-sequence::event-chord-relative-callback)
	(types . (general-music event-chord simultaneous-music))
	))

    
    (FingeringEvent
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
    (LabelEvent
     . ((description . "Place a bookmarking label")
	(types . (general-music label-event event))))
    (LaissezVibrerEvent
     . ((description . "Don't damp this chord.

Syntax: @var{note}\\laissezVibrer.")

	(types . (general-music event laissez-vibrer-event))
	))
    (LigatureEvent
     . (
	(description .	"Start or end a ligature.")
	(span-type . ligature)
	(types . (general-music span-event ligature-event event))
	))
    (LineBreakEvent
     . (
	(description .  "Allow, forbid or force a line break.")
	(types . (general-music line-break-event break-event event))
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
    (MultiMeasureRestMusic
     . (
	(description . "Rests that may be compressed into Multi rests. 

Syntax
@code{R2.*4} for 4 measures in 3/4 time.")
	(iterator-ctor . ,ly:sequential-iterator::constructor)
	(elements-callback . ,mm-rest-child-list)
	(types . (general-music multi-measure-rest))
	))

    (MultiMeasureRestEvent
     . (
	(description . "Used internally by MultiMeasureRestMusic to signal rests")
	(types . (general-music event rhythmic-event multi-measure-rest-event))
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
    
    (OverrideProperty
     . (
	(description .	"Extend the definition of a graphical object.

SYNTAX

@code{\\override [ @var{Ctxt} . ] @var{Obj} @var{prop} = @var{val}}
")
	(types . (general-music layout-instruction-event override-property-event))
	(iterator-ctor . ,ly:push-property-iterator::constructor)
	))
    (PageBreakEvent
     . (
	(description .  "Allow, forbid or force a page break.")
	(types . (general-music break-event page-break-event event))
	))
    (PageTurnEvent
     . (
	(description .  "Allow, forbid or force a page turn.")
	(types . (general-music break-event page-turn-event event))
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
	(types . (layout-instruction-event general-music))
	(iterator-ctor . ,ly:property-iterator::constructor)
	))

    (PropertyUnset
     . (
	(description .	"Remove the definition of a context @code{\\property}.")

	(types . (layout-instruction-event general-music))
	(iterator-ctor . ,ly:property-unset-iterator::constructor)
	))

    (PercentEvent
     . (
	(description .	"Used internally to signal percent repeats.")
	(types . (general-music event percent-event rhythmic-event))
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

	(types . (general-music layout-instruction-event))
	(iterator-ctor . ,	ly:pop-property-iterator::constructor)
	))

    (SequentialMusic
     . (
	(description .	"Music expressions concatenated. 

Syntax \\sequential @{..@} or simply @{..@} .")

	(length-callback . ,ly:music-sequence::cumulative-length-callback)
	(start-callback . ,ly:music-sequence::first-start-callback)
	(elements-callback . ,(lambda (m) (ly:music-property m 'elements)))
	(iterator-ctor . ,ly:sequential-iterator::constructor)
	(types . (general-music sequential-music))
	))

    (SoloOneEvent
     . (
	(description . "Print Solo.1")
	(part-combine-status . solo1)
	(types . (general-music event part-combine-event solo-one-event))
	))
    (SoloTwoEvent
     . (
	(description . "Print Solo.2")
	(part-combine-status . solo2)
	(types . (general-music event part-combine-event solo-two-event))
	))
    (UnisonoEvent
     . ((description . "Print a2")
	(part-combine-status . unisono)
	(types . (general-music event part-combine-event unisono-event))))
    
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
    
    (TransposedMusic
     . (
	(description .	"Music that has been transposed.")
	(iterator-ctor . ,ly:music-wrapper-iterator::constructor)
	(start-callback . ,ly:music-wrapper::start-callback)
	(length-callback . ,ly:music-wrapper::length-callback)
	(to-relative-callback . ,ly:relative-octave-music::no-relative-callback)
	(types . (music-wrapper-music general-music transposed-music))
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

    (TupletSpanEvent
     . (
	(description .  "Used internally to signal where tuplet brackets start and stop.")
	(types . (tuplet-span-event span-event event general-music))
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
does not create staves or voices implicitly.



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

    (SpacingSectionEvent
     . ((description . "Start a new spacing section")
	(types . (general-music event spacing-section-event))))
     
    (SpanEvent
     . (
	(description .	"Event for anything that is started at a different time than stopped.")

	(types . (general-music event))
	))
    
    (SustainEvent
     . (
	(description . "Depress or release sustain pedal. ")
	(types . (general-music event pedal-event sustain-event))
	))
    
    (SostenutoEvent
     . (
	(description . "Depress or release sostenuto pedal. ")
	(types . (general-music event pedal-event sostenuto-event))
	))
    
    (UnaCordaEvent
     . (
	(description . "Depress or release una-corda pedal.")
	(types . (general-music event pedal-event una-corda-event))
	))
    
    (StringNumberEvent
     . (
	(description .	"Specify on which string to play this note. 

Syntax: @code{\\@var{number}}.")

	(types . (general-music string-number-event event))
	)) 

    (StrokeFingerEvent
     . (
	(description .	"Specify with which finger to pluck a string. 

Syntax: @code{\\rightHandFinger @var{text}}.")

	(types . (general-music stroke-finger-event event))
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
    
    (TremoloSpanEvent
     . (
	(description . "Tremolo over two stems")
	(types . (general-music event span-event tremolo-span-event))
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
	(ly:error (_ "cannot find music object: ~S") name))
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

