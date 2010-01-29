;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 1998--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;;		     Jan Nieuwenhuizen <janneke@gnu.org>
;;;;
;;;; LilyPond is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; LilyPond is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.

;; TODO: should link back into user manual.

(define (mm-rest-child-list music)
  "Generate events for multimeasure rests,
to be used by the sequential-iterator"
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
     . ((description . "Create a dynamic mark.

Syntax: @var{note}@code{\\x}, where @code{\\x} is a dynamic mark like
@code{\\ppp} or @code{\\sfz}.  A complete list is in file
@file{ly/@/dynamic-scripts-init.ly}.")
	(types . (general-music event dynamic-event absolute-dynamic-event))
	))

    (AnnotateOutputEvent
     . ((description . "Print an annotation of an output element.")
	(types . (general-music event annotate-output-event))
	))

    (ApplyContext
     . ((description . "Call the argument with the current context during
interpreting phase.")
	(types . (general-music apply-context))
	(iterator-ctor . ,ly:apply-context-iterator::constructor)
	))

    (ApplyOutputEvent
     . ((description . "Call the argument with all current grobs during
interpreting phase.

Syntax: @code{\\applyOutput #'@var{context} @var{func}}

Arguments to @var{func} are 1.@tie{}the grob, 2.@tie{}the originating
context, and 3.@tie{}the context where @var{func} is called.")
	(types . (general-music event apply-output-event))
	))

    (ArpeggioEvent
     . ((description . "Make an arpeggio on this note.

Syntax: @var{note}@code{-\\arpeggio}")
	(types . (general-music arpeggio-event event))
	))

    ;; todo: use articulation-event for slur as well.
    ;; separate non articulation scripts
    (ArticulationEvent
     . ((description . "Add an articulation marking to a note.

Syntax: @var{note}@code{x}@code{y}, where @code{x} is a direction
(@code{^} for up or @code{_} for down), or LilyPond's choice
(no direction specified)), and where @code{y} is an articulation
(such as @code{-.}, @code{->}, @code{\\tenuto}, @code{\\downbow}).
See the Notation Reference for details.")
	(types . (general-music event articulation-event script-event))
	))

    (AutoChangeMusic
     . ((description . "Used for making voices that switch between
piano staves automatically.")
	(iterator-ctor . ,ly:auto-change-iterator::constructor)
	(start-callback . ,ly:music-wrapper::start-callback)
	(length-callback . ,ly:music-wrapper::length-callback)
	(types . (general-music music-wrapper-music auto-change-instruction))
	))

    (BarCheck
     . ((description . "Check whether this music coincides with
the start of the measure.")
	(types . (general-music bar-check))
	(iterator-ctor . ,ly:bar-check-iterator::constructor)
	))

    (BassFigureEvent
     . ((description . "Print a bass-figure text.")
	(types . (general-music event rhythmic-event bass-figure-event))
	))

    (BeamEvent
     . ((description . "Start or stop a beam.

Syntax for manual control: @code{c8-[ c c-] c8}")
	(types . (general-music event beam-event span-event))
	))

    (BeamForbidEvent
     . ((description . "Specify that a note may not auto-beamed.")
	(types . (general-music event beam-forbid-event))
	))

    (BendAfterEvent
     . ((description . "A drop/fall/doit jazz articulation.")
	(types . (general-music bend-after-event event))))

    (BreathingEvent
     . ((description . "Create a @q{breath mark} or @q{comma}.

Syntax: @var{note}@code{\\breathe}")

	(types . (general-music event breathing-event))
	))

    (ClusterNoteEvent
     . ((description . "A note that is part of a cluster.")
	;; not a note-event, to ensure that Note_heads_engraver doesn't eat it.
	(types . (general-music cluster-note-event melodic-event
		  rhythmic-event event))
	))

    (ContextChange
     . ((description . "Change staves in Piano staff.

Syntax: @code{\\change Staff = @var{new-id}}")
	(iterator-ctor . ,ly:change-iterator::constructor)
	(types . (general-music translator-change-instruction))
	))

    (ContextSpeccedMusic
     . ((description . "Interpret the argument music within a
specific context.")
	(iterator-ctor . ,ly:context-specced-music-iterator::constructor)
	(length-callback . ,ly:music-wrapper::length-callback)
	(start-callback . ,ly:music-wrapper::start-callback)
	(types . (context-specification general-music music-wrapper-music))
	))

    (CrescendoEvent
     . ((description . "Begin or end a crescendo.

Syntax: @var{note}@code{\\<} @dots{} @var{note}@code{\\!}

An alternative syntax is @var{note}@code{\\cr} @dots{}
@var{note}@code{\\endcr}.")
	(types . (general-music span-event span-dynamic-event crescendo-event
		  event))
	))

    (DecrescendoEvent
     . ((description . "Begin or end a decrescendo.

Syntax: @var{note}@code{\\>} @dots{} @var{note}@code{\\!}

An alternative syntax is @var{note}@code{\\decr} @dots{}
@var{note}@code{\\enddecr}.")
	(types . (general-music span-event span-dynamic-event decrescendo-event
		  event))
	))

    (Event
     . ((description . "Atomic music event.")
	(types . (general-music event))
	))

    (EventChord
     . ((description . "Internally used to group a set of events.")
	(iterator-ctor . ,ly:event-chord-iterator::constructor)
	(length-callback . ,ly:music-sequence::maximum-length-callback)
	(to-relative-callback .
	 ,ly:music-sequence::event-chord-relative-callback)
	(types . (general-music event-chord simultaneous-music))
	))

    (ExtenderEvent
     . ((description . "Extend lyrics.")
	(types . (general-music extender-event event))
	))

    (FingeringEvent
     . ((description . "Specify what finger to use for this note.")
	(types . (general-music fingering-event event))
	))

    (GlissandoEvent
     . ((description . "Start a glissando on this note.")
	(types . (general-music glissando-event event))
	))

    (GraceMusic
     . ((description . "Interpret the argument as grace notes.")
	(start-callback . ,ly:grace-music::start-callback)
	(length . ,ZERO-MOMENT)
	(iterator-ctor . ,ly:grace-iterator::constructor)
	(types . (grace-music music-wrapper-music general-music))
	))

    (HarmonicEvent
     . ((description . "Mark a note as harmonic.")
	(types . (general-music event harmonic-event))
	))

    (HyphenEvent
     . ((description . "A hyphen between lyric syllables.")
	(types . (general-music hyphen-event event))
	))

    (KeyChangeEvent
     . ((description . "Change the key signature.

Syntax: @code{\\key} @var{name} @var{scale}")
	(to-relative-callback . ,(lambda (x p) p))
	(types . (general-music key-change-event event))
	))

    (LabelEvent
     . ((description . "Place a bookmarking label.")
	(types . (general-music label-event event))
	))

    (LaissezVibrerEvent
     . ((description . "Don't damp this chord.

Syntax: @var{note}@code{\\laissezVibrer}")
	(types . (general-music event laissez-vibrer-event))
	))

    (LigatureEvent
     . ((description . "Start or end a ligature.")
	(span-type . ligature)
	(types . (general-music span-event ligature-event event))
	))

    (LineBreakEvent
     . ((description . "Allow, forbid or force a line break.")
	(types . (general-music line-break-event break-event event))
	))

    (LyricCombineMusic
     . ((description . "Align lyrics to the start of notes.

Syntax: @code{\\lyricsto} @var{voicename} @var{lyrics}")
	(length . ,ZERO-MOMENT)
	(types . (general-music lyric-combine-music))
	(iterator-ctor . ,ly:lyric-combine-music-iterator::constructor)
	))

    (LyricEvent
     . ((description . "A lyric syllable.  Must be entered in lyrics mode,
i.e., @code{\\lyrics @{ twinkle4 twinkle4 @} }.")
	(types . (general-music rhythmic-event lyric-event event))
	))

    (MarkEvent
     . ((description . "Insert a rehearsal mark.

Syntax: @code{\\mark} @var{marker}

Example: @code{\\mark \"A\"}")
	(types . (general-music mark-event event))
	))

    (MultiMeasureRestEvent
     . ((description . "Used internally by @code{MultiMeasureRestMusic}
to signal rests.")
	(types . (general-music event rhythmic-event
		  multi-measure-rest-event))
	))

    (MultiMeasureRestMusic
     . ((description . "Rests that may be compressed into Multi rests.

Syntax: @code{R2.*4} for 4 measures in 3/4 time.")
	(iterator-ctor . ,ly:sequential-iterator::constructor)
	(elements-callback . ,mm-rest-child-list)
	(types . (general-music multi-measure-rest))
	))

    (MultiMeasureTextEvent
     . ((description . "Texts on multi measure rests.

Syntax: @code{R-\\markup @{ \\roman \"bla\" @}}

Note the explicit font switch.")
	(types . (general-music event multi-measure-text-event))
	))

    (Music
     . ((description . "Generic type for music expressions.")
	(types . (general-music))
	))

    (NoteEvent
     . ((description . "A note.")
	(types . (general-music event note-event rhythmic-event
		  melodic-event))
	))

    (NoteGroupingEvent
     . ((description . "Start or stop grouping brackets.")
	(types . (general-music event note-grouping-event))
	))

    (OverrideProperty
     . ((description . "Extend the definition of a graphical object.

Syntax: @code{\\override} [ @var{context} @code{.} ]
@var{object} @var{property} @code{=} @var{value}")
	(types . (general-music layout-instruction-event
		  override-property-event))
	(iterator-ctor . ,ly:push-property-iterator::constructor)
	))

    (PageBreakEvent
     . ((description . "Allow, forbid or force a page break.")
	(types . (general-music break-event page-break-event event))
	))

    (PageTurnEvent
     . ((description . "Allow, forbid or force a page turn.")
	(types . (general-music break-event page-turn-event event))
	))

    (PartCombineMusic
     . ((description . "Combine two parts on a staff, either merged or
as separate voices.")
	(length-callback . ,ly:music-sequence::maximum-length-callback)
	(start-callback . ,ly:music-sequence::minimum-start-callback)
	(types . (general-music part-combine-music))
	(iterator-ctor . ,ly:part-combine-iterator::constructor)
	))

    (PercentEvent
     . ((description . "Used internally to signal percent repeats.")
	(types . (general-music event percent-event rhythmic-event))
	))

    (PercentRepeatedMusic
     . ((description . "Repeats encoded by percents.")
	(iterator-ctor . ,ly:percent-repeat-iterator::constructor)
	(start-callback .  ,ly:repeated-music::first-start)
	(length-callback . ,ly:repeated-music::unfolded-music-length)
	(types . (general-music repeated-music percent-repeated-music))
	))

    (PesOrFlexaEvent
     . ((description . "Within a ligature, mark the previous and the
following note to form a pes (if melody goes up) or a flexa (if melody
goes down).")
	(types . (general-music pes-or-flexa-event event))
	))

    (PhrasingSlurEvent
     . ((description . "Start or end phrasing slur.

Syntax: @var{note}@code{\\(} and @var{note}@code{\\)}")
	(types . (general-music span-event event phrasing-slur-event))
	))

    (PropertySet
     . ((description . "Set a context property.

Syntax: @code{\\set @var{context}.@var{prop} = @var{scheme-val}}")
	(types . (layout-instruction-event general-music))
	(iterator-ctor . ,ly:property-iterator::constructor)
	))

    (PropertyUnset
     . ((description . "Restore the default setting for a context
property.  See @ref{PropertySet}.

Syntax: @code{\\unset @var{context}.@var{prop}}")
	(types . (layout-instruction-event general-music))
	(iterator-ctor . ,ly:property-unset-iterator::constructor)
	))

    (QuoteMusic
     . ((description . "Quote preprocessed snippets of music.")
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

    (RelativeOctaveMusic
     . ((description . "Music that was entered in relative octave notation.")
	(to-relative-callback . ,ly:relative-octave-music::relative-callback)
	(iterator-ctor . ,ly:music-wrapper-iterator::constructor)
	(length-callback . ,ly:music-wrapper::length-callback)
	(start-callback . ,ly:music-wrapper::start-callback)
	(types . (music-wrapper-music general-music relative-octave-music))
	))

    (RepeatedChord
     . ((description . "A chord repetition")
	(to-relative-callback . ,ly:music-sequence::repeated-chord-relative-callback)
	(iterator-ctor . ,ly:music-wrapper-iterator::constructor)
	(start-callback . ,ly:music-wrapper::start-callback)
	(length-callback . ,ly:music-wrapper::length-callback)
	(types . (general-music music-wrapper-music))
	))

    (RepeatedMusic
     . ((description . "Repeat music in different ways.")
	(types . (general-music repeated-music))
	))

    (RepeatTieEvent
     . ((description . "Ties for starting a second volta bracket.")
	(types . (general-music event repeat-tie-event))
	))

    (RestEvent
     . ((description . "A Rest.

Syntax: @code{r4} for a quarter rest.")
	(types . (general-music event rhythmic-event rest-event))
	))

    (RevertProperty
     . ((description . "The opposite of @ref{OverrideProperty}: remove a
previously added property from a graphical object definition.")
	(types . (general-music layout-instruction-event))
	(iterator-ctor . ,ly:pop-property-iterator::constructor)
	))

    (ScriptEvent
     . ((description . "Add an articulation mark to a note.")
	(types . (general-music event))
	))

    (SequentialMusic
     . ((description . "Music expressions concatenated.

Syntax: @code{\\sequential @{ @dots{} @}} or simply @code{@{ @dots{} @}}")
	(length-callback . ,ly:music-sequence::cumulative-length-callback)
	(start-callback . ,ly:music-sequence::first-start-callback)
	(elements-callback . ,(lambda (m) (ly:music-property m 'elements)))
	(iterator-ctor . ,ly:sequential-iterator::constructor)
	(types . (general-music sequential-music))
	))

    (SimultaneousMusic
     . ((description . "Music playing together.

Syntax: @code{\\simultaneous @{ @dots{} @}} or @code{<< @dots{} >>}")
	(iterator-ctor . ,ly:simultaneous-music-iterator::constructor)
	(start-callback . ,ly:music-sequence::minimum-start-callback)
	(length-callback . ,ly:music-sequence::maximum-length-callback)
	(to-relative-callback .
	 ,ly:music-sequence::simultaneous-relative-callback)
	(types . (general-music simultaneous-music))
	))

    (SkipEvent
     . ((description . "Filler that takes up duration, but does not
print anything.

Syntax: @code{s4} for a skip equivalent to a quarter rest.")
	(types . (general-music event rhythmic-event skip-event))
	))

    (SkipMusic
     . ((description . "Filler that takes up duration, does not
print anything, and also does not create staves or voices implicitly.

Syntax: @code{\\skip} @var{duration}")
	(length-callback . ,ly:music-duration-length)
	(iterator-ctor . ,ly:simple-music-iterator::constructor)
	(types . (general-music event rhythmic-event skip-event))
	))

    (SlurEvent
     . ((description . "Start or end slur.

Syntax: @var{note}@code{(} and @var{note}@code{)}")
	(types . (general-music span-event event slur-event))
	))

    (SoloOneEvent
     . ((description . "Print @q{Solo@tie{}1}.")
	(part-combine-status . solo1)
	(types . (general-music event part-combine-event solo-one-event))
	))

    (SoloTwoEvent
     . ((description . "Print @q{Solo@tie{}2}.")
	(part-combine-status . solo2)
	(types . (general-music event part-combine-event solo-two-event))
	))

    (SostenutoEvent
     . ((description . "Depress or release sostenuto pedal.")
	(types . (general-music event pedal-event sostenuto-event))
	))

    (SpacingSectionEvent
     . ((description . "Start a new spacing section.")
	(types . (general-music event spacing-section-event))))

    (SpanEvent
     . ((description . "Event for anything that is started at a
different time than stopped.")
	(types . (general-music event))
	))

    (StaffSpanEvent
     . ((description . "Start or stop a staff symbol.")
	(types . (general-music event span-event staff-span-event))
     ))

    (StringNumberEvent
     . ((description . "Specify on which string to play this note.

Syntax: @code{\\@var{number}}")
	(types . (general-music string-number-event event))
	))

    (StrokeFingerEvent
     . ((description . "Specify with which finger to pluck a string.

Syntax: @code{\\rightHandFinger @var{text}}")
	(types . (general-music stroke-finger-event event))
	))

    (SustainEvent
     . ((description . "Depress or release sustain pedal.")
	(types . (general-music event pedal-event sustain-event))
	))

    (TextScriptEvent
     . ((description . "Print text.")
	(types . (general-music script-event text-script-event event))
	))

    (TextSpanEvent
     . ((description . "Start a text spanner, for example, an
octavation.")
	(types . (general-music span-event event text-span-event))
	))

    (TieEvent
     . ((description . "A tie.

Syntax: @var{note}@code{-~}")
	(types . (general-music tie-event event))
	))

    (TimeScaledMusic
     . ((description . "Multiply durations, as in tuplets.

Syntax: @code{\\times @var{fraction} @var{music}}, e.g.,
@code{\\times 2/3 @{ @dots{} @}} for triplets.")
	(length-callback . ,ly:music-wrapper::length-callback)
	(start-callback . ,ly:music-wrapper::start-callback)
	(iterator-ctor . ,ly:time-scaled-music-iterator::constructor)
	(types . (time-scaled-music music-wrapper-music general-music))
	))

    (TransposedMusic
     . ((description . "Music that has been transposed.")
	(iterator-ctor . ,ly:music-wrapper-iterator::constructor)
	(start-callback . ,ly:music-wrapper::start-callback)
	(length-callback . ,ly:music-wrapper::length-callback)
	(to-relative-callback .
	 ,ly:relative-octave-music::no-relative-callback)
	(types . (music-wrapper-music general-music transposed-music))
	))

    (TremoloEvent
     . ((description . "Unmeasured tremolo.")
	(types . (general-music event tremolo-event))
	))

    (TremoloRepeatedMusic
     . ((description . "Repeated notes denoted by tremolo beams.")
	(iterator-ctor . ,ly:chord-tremolo-iterator::constructor)
	(start-callback .  ,ly:repeated-music::first-start)
	;; the length of the repeat is handled by shifting the note logs
	(length-callback . ,ly:repeated-music::folded-music-length)
	(types . (general-music repeated-music tremolo-repeated-music))
	))

    (TremoloSpanEvent
     . ((description . "Tremolo over two stems.")
	(types . (general-music event span-event tremolo-span-event))
	))

    (TrillSpanEvent
     . ((description . "Start a trill spanner.")
	(types . (general-music span-event event trill-span-event))
	))

    (TupletSpanEvent
     . ((description . "Used internally to signal where tuplet
brackets start and stop.")
	(types . (tuplet-span-event span-event event general-music))
       ))

    (UnaCordaEvent
     . ((description . "Depress or release una-corda pedal.")
	(types . (general-music event pedal-event una-corda-event))
	))

    (UnfoldedRepeatedMusic
     . ((description . "Repeated music which is fully written
(and played) out.")
	(iterator-ctor . ,ly:unfolded-repeat-iterator::constructor)
	(start-callback .  ,ly:repeated-music::first-start)
	(types . (general-music repeated-music unfolded-repeated-music))
	(length-callback . ,ly:repeated-music::unfolded-music-length)
	))

    (UnisonoEvent
     . ((description . "Print @q{a@tie{}2}.")
	(part-combine-status . unisono)
	(types . (general-music event part-combine-event unisono-event))))

    (UnrelativableMusic
     . ((description . "Music that cannot be converted from relative
to absolute notation.  For example, transposed music.")
	(to-relative-callback . ,ly:relative-octave-music::no-relative-callback)
	(iterator-ctor . ,ly:music-wrapper-iterator::constructor)
	(length-callback . ,ly:music-wrapper::length-callback)
	(types . (music-wrapper-music general-music unrelativable-music))
	))

    (VoiceSeparator
     . ((description . "Separate polyphonic voices in simultaneous music.

Syntax: @code{\\\\}")
	(types . (separator general-music))
	))

    (VoltaRepeatedMusic
     . ((description . "Repeats with alternatives placed sequentially.")
	(iterator-ctor . ,ly:volta-repeat-iterator::constructor)
	(start-callback .  ,ly:repeated-music::first-start)
	(length-callback . ,ly:repeated-music::volta-music-length)
	(types . (general-music repeated-music volta-repeated-music))
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
according to @code{music-properties}, a list of alternating property symbols
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
  (let* ((repeated-music (assoc-get name '(("volta" . VoltaRepeatedMusic)
					   ("unfold" . UnfoldedRepeatedMusic)
					   ("percent" . PercentRepeatedMusic)
					   ("tremolo" . TremoloRepeatedMusic))))
	 (repeated-music-name (if repeated-music
				  repeated-music
        			  (begin
				    (ly:warning (_ "unknown repeat type `~S'") name)
				    (ly:warning (_ "See define-music-types.scm for supported repeats"))
				    'VoltaRepeatedMusic))))
    (make-music repeated-music-name)))
