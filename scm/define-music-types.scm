;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 1998--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;;                 Jan Nieuwenhuizen <janneke@gnu.org>
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

(define-public music-descriptions
  `(
    (AbsoluteDynamicEvent
     . ((description . "Create a dynamic mark.

Syntax: @var{note}@code{\\x}, where @code{\\x} is a dynamic mark like
@code{\\ppp} or @code{\\sfz}.  A complete list is in file
@file{ly/@/dynamic-scripts-init.ly}.")
        (types . (post-event event dynamic-event absolute-dynamic-event))
        ))

    (AdHocJumpEvent
     . ((description . "Insert a @code{JumpScript}.

Syntax: @code{\\jump} @var{markup}

Example: @code{\\jump \"Gavotte I D.C.\"}")
        (types . (ad-hoc-jump-event event))
        ))

    (AdHocMarkEvent
     . ((description . "Insert markup as a rehearsal mark without advancing
the rehearsal mark sequence.

Syntax: @code{\\mark} @var{markup}

Example: @code{\\mark \"A\"}")
        (types . (ad-hoc-mark-event mark-event event))
        ))

    (AlternativeEvent
     . ((description . "Create an alternative event.")
        (types . (event alternative-event))
        ))

    (AnnotateOutputEvent
     . ((description . "Print an annotation of an output element.")
        (types . (event annotate-output-event post-event))
        ))

    (ApplyContext
     . ((description . "Call the argument with the current context during
interpreting phase.")
        (types . (apply-context))
        (iterator-ctor . ,ly:apply-context-iterator::constructor)
        ))

    (ApplyOutputEvent
     . ((description . "Call the argument with all current grobs during
interpreting phase.

Syntax: @code{\\applyOutput #'@var{context} @var{func}}

Arguments to @var{func} are 1.@tie{}the grob, 2.@tie{}the originating
context, and 3.@tie{}the context where @var{func} is called.")
        (types . (event apply-output-event))
        ))

    (ArpeggioEvent
     . ((description . "Make an arpeggio on this note.

Syntax: @w{@var{note}@code{-\\arpeggio}}")
        (types . (post-event arpeggio-event event))
        ))

    ;; todo: use articulation-event for slur as well.
    ;; separate non articulation scripts
    (ArticulationEvent
     . ((description . "Add an articulation marking to a note.

Syntax: @var{note}@code{x}@code{y}, where @code{x} is a direction\
\n(@code{^} for up or @code{_} for down), or LilyPond's choice\
\n(no direction specified), and where @code{y} is an articulation\
\n(such as @w{@code{-.}}, @w{@code{->}}, @code{\\tenuto}, @code{\\downbow}).
See the Notation Reference for details.")
        (types . (post-event event articulation-event script-event))
        ))

    (BarEvent
     . ((description . "Force a bar line.

Syntax: @code{\\bar @var{type}}

Example: @code{\\bar \"!\"}")
        (types . (bar-event event))
        ))

    (BarCheck
     . ((description . "Check whether this music coincides with
the start of the measure.")
        (types . (bar-check))
        (iterator-ctor . ,ly:bar-check-iterator::constructor)
        ))

    (BassFigureEvent
     . ((description . "Print a bass-figure text.")
        (types . (event rhythmic-event bass-figure-event))
        ))

    (BeamEvent
     . ((description . "Start or stop a beam.

Syntax for manual control: @code{c8-[ c c-] c8}")
        (types . (post-event event beam-event span-event))
        ))

    (BeamForbidEvent
     . ((description . "Specify that a note may not auto-beamed.")
        (types . (post-event event beam-forbid-event))
        ))

    (BendSpanEvent
     . ((description . "Used to signal where a bend spanner starts and stops.")
        (types . (bend-span-event post-event span-event event))
        ))

    (BreakDynamicSpanEvent
     . ((description . "End an alignment spanner for dynamics here.")
        (types . (post-event break-span-event break-dynamic-span-event event))
        ))

    (BendAfterEvent
     . ((description . "A drop/@/fall/@/doit jazz articulation.")
        (types . (post-event bend-after-event event))))

    (BreathingEvent
     . ((description . "A short span of silence that shortens the previous note.

Syntax: @var{note}@code{\\breathe}")

        (types . (event breathing-event))
        (midi-length . ,breathe::midi-length)))

    (CaesuraEvent
     . ((description . "A short span of silence that does not shorten the
previous note.

Syntax: @var{note}@code{\\caesura}")

        (types . (caesura-event event))))

    (ClusterNoteEvent
     . ((description . "A note that is part of a cluster.")
        ;; not a note-event, to ensure that Note_heads_engraver doesn't eat it.
        (iterator-ctor . ,ly:rhythmic-music-iterator::constructor)
        (types . (cluster-note-event melodic-event rhythmic-event event))
        ))

    (CodaMarkEvent
     . ((description . "Add a coda mark.")
        (types . (coda-mark-event event))
        ))

    (CompletizeExtenderEvent
     . ((description . "Used internally to signal the end of a lyrics block to
ensure extenders are completed correctly when a @code{Lyrics} context ends
before its associated @code{Voice} context.")
        (types . (completize-extender-event event))
        ))

    (ContextChange
     . ((description . "Change staves in Piano staff.

Syntax: @code{\\change Staff = @var{new-id}}")
        (iterator-ctor . ,ly:change-iterator::constructor)
        (types . (translator-change-instruction))
        ))

    (ContextSpeccedMusic
     . ((description . "Interpret the argument music within a
specific context.")
        (iterator-ctor . ,ly:context-specced-music-iterator::constructor)
        (length-callback . ,ly:music-wrapper::length-callback)
        (start-callback . ,ly:music-wrapper::start-callback)
        (types . (context-specification music-wrapper-music))
        ))

    (CrescendoEvent
     . ((description . "Begin or end a crescendo.

Syntax: @var{note}@code{\\<} @dots{} @var{note}@code{\\!}

An alternative syntax is @var{note}@code{\\cr} @dots{}
@var{note}@code{\\endcr}.")
        (types . (post-event span-event span-dynamic-event crescendo-event
                             event))
        ))

    (DalSegnoEvent
     . ((description . "Add a @emph{D.S.} or similar instruction.")
        (types . (dal-segno-event event))
        ))

    (DecrescendoEvent
     . ((description . "Begin or end a decrescendo.

Syntax: @var{note}@code{\\>} @dots{} @var{note}@code{\\!}

An alternative syntax is @var{note}@code{\\decr} @dots{}
@var{note}@code{\\enddecr}.")
        (types . (post-event span-event span-dynamic-event decrescendo-event
                             event))
        ))

    (DoublePercentEvent
     . ((description . "Used internally to signal double percent repeats.")
        (types . (event double-percent-event rhythmic-event))
        ))

    (DurationLineEvent
     . ((description . "Initiate a duration line.

Syntax: @var{note}@code{\\-}")
        (types . (duration-line-event post-event event))
        ))

    (EpisemaEvent
     . ((description . "Begin or end an episema.")
        (types . (post-event span-event event episema-event))
        ))

    (Event
     . ((description . "Atomic music event.")
        (types . (event))
        ))

    (EventChord
     . ((description . "Explicitly entered chords.

When iterated, @code{elements} are converted to events at the current
timestep, followed by any @code{articulations}.  Per-chord postevents
attached by the parser just follow any rhythmic events in
@code{elements} instead of utilizing @code{articulations}.

An unexpanded chord repetition @samp{q} is recognizable by having its
duration stored in @code{duration}.")
        (iterator-ctor . ,ly:event-chord-iterator::constructor)
        (length-callback . ,ly:music-sequence::event-chord-length-callback)
        (to-relative-callback .
                              ,ly:music-sequence::event-chord-relative-callback)
        (types . (event-chord simultaneous-music))
        ))

    (ExtenderEvent
     . ((description . "Extend lyrics.")
        (types . (post-event extender-event event))
        ))

    (FineEvent
     . ((description . "End the performance, not necessarily at the
written end of the music.")
        (iterator-ctor . ,ly:fine-iterator::constructor)
        (types . (fine-event event))
        ))

    (FingeringEvent
     . ((description . "Specify what finger to use for this note.")
        (types . (post-event fingering-event event))
        ))

    (FingerGlideEvent
     . ((description . "Initiate a line connecting two equal fingerings.
This line represents a finger gliding on a string.

Syntax: @var{note}@code{\\glide}@code{-}@var{finger}")
        (types . (finger-glide-event post-event event))
        ))

    (FootnoteEvent
     . ((description . "Footnote a grob.")
        (types . (event footnote-event))
        ))

    (GlissandoEvent
     . ((description . "Start a glissando on this note.")
        (types . (post-event glissando-event event))
        ))

    (GraceMusic
     . ((description . "Interpret the argument as grace notes.")
        (start-callback . ,ly:grace-music::start-callback)
        (length . ,ZERO-MOMENT)
        (iterator-ctor . ,ly:grace-iterator::constructor)
        (types . (grace-music music-wrapper-music))
        ))

    (HarmonicEvent
     . ((description . "Mark a note as harmonic.")
        (types . (post-event event harmonic-event))
        ))

    (StaffHighlightEvent
     . ((description . "Start or stop a staff highlight.

Syntax: @code{\\staffHighlight}, @code{\\stopStaffHighlight}.")
        (types . (staff-highlight-event span-event event))))

    (HyphenEvent
     . ((description . "A hyphen between lyric syllables.")
        (types . (post-event hyphen-event event))
        ))

    (KeyChangeEvent
     . ((description . "Change the key signature.

Syntax: @code{\\key} @var{name} @var{scale}")
        (to-relative-callback . ,(lambda (x p) p))
        (types . (key-change-event event))
        ))

    (LabelEvent
     . ((description . "Place a bookmarking label.")
        (types . (label-event event))
        ))

    (LaissezVibrerEvent
     . ((description . "Don't damp this chord.

Syntax: @var{note}@code{\\laissezVibrer}")
        (types . (post-event event laissez-vibrer-event))
        ))

    (LigatureEvent
     . ((description . "Start or end a ligature.")
        (types . (span-event ligature-event event))
        ))

    (LineBreakEvent
     . ((description . "Allow, forbid or force a line break.")
        (types . (line-break-event break-event event))
        ))

    (LyricCombineMusic
     . ((description . "Align lyrics to the start of notes.

Syntax: @code{\\lyricsto} @var{voicename} @var{lyrics}")
        (length . ,INF-MOMENT)
        (types . (lyric-combine-music))
        (iterator-ctor . ,ly:lyric-combine-music-iterator::constructor)
        ))

    (LyricEvent
     . ((description . "A lyric syllable.  Must be entered in lyrics mode,
i.e., @code{\\lyrics @{ twinkle4 twinkle4 @} }.")
        (iterator-ctor . ,ly:rhythmic-music-iterator::constructor)
        (types . (rhythmic-event lyric-event event))
        ))

    (MeasureSpannerEvent
     . ((description . "Used to signal the start and end of a measure
spanner.")
        (types . (measure-spanner-event span-event event))
        ))

    (MeasureCounterEvent
     . ((description . "Used to signal the start and end of a measure count.")
        (types . (measure-counter-event span-event event))
        ))

    (MultiMeasureArticulationEvent
     . ((description . "Articulations on multi-measure rests.")
        (types . (post-event event multi-measure-articulation-event))
        ))

    (MultiMeasureRestEvent
     . ((description . "Used internally by @code{MultiMeasureRestMusic}
to signal rests.")
        (iterator-ctor . ,ly:rhythmic-music-iterator::constructor)
        (types . (event rhythmic-event general-rest-event multi-measure-rest-event))
        ))

    (MultiMeasureRestMusic
     . ((description . "Rests that may be compressed into
multi-measure rests.

Syntax: @code{R2.*4} for 4 measures in 3/4 time.")
        (iterator-ctor . ,ly:sequential-iterator::constructor)
        (elements-callback . ,mm-rest-child-list)
        (types . (multi-measure-rest))
        ))

    (MultiMeasureTextEvent
     . ((description . "Texts on multi-measure rests.

Syntax: @code{R-\\markup @{ \\roman \"bla\" @}}

Note the explicit font switch.")
        (types . (post-event event multi-measure-text-event))
        ))

    (Music
     . ((description . "Generic type for music expressions.")
        (types . ())
        ))

    (NoteEvent
     . ((description . "A note.

Outside of chords, any events in @code{articulations} with a listener
are broadcast like chord articulations, the others are retained.

For iteration inside of chords, @xref{EventChord}.")
        (iterator-ctor . ,ly:rhythmic-music-iterator::constructor)
        (types . (event note-event rhythmic-event melodic-event))
        ))

    (NoteGroupingEvent
     . ((description . "Start or stop grouping brackets.")
        (types . (post-event event note-grouping-event))
        ))

    (OttavaEvent
     . ((description . "Start or stop an ottava bracket.")
        (types . (ottava-event event))
        ))

    (OverrideProperty
     . ((description . "Extend the definition of a graphical object.

Syntax: @code{\\override} [ @var{context} @code{.} ]
@var{object} @var{property} @code{=} @var{value}")
        (types . (layout-instruction-event override-property-event))
        (iterator-ctor . ,ly:push-property-iterator::constructor)
        (untransposable . #t)
        ))

    (PageBreakEvent
     . ((description . "Allow, forbid or force a page break.")
        (types . (break-event page-break-event event))
        ))

    (PageTurnEvent
     . ((description . "Allow, forbid or force a page turn.")
        (types . (break-event page-turn-event event))
        ))

    (PartialSet
     . ((description . "Create an anacrusis or upbeat (partial measure).")
        (iterator-ctor . ,ly:partial-iterator::constructor)
        ;; The length-callback is kind of cheesy since 'elements is
        ;; empty.  We just use that in order to get a zero length
        ;; for the overall timing in spite of having a non-zero
        ;; duration field.
        (length-callback . ,ly:music-sequence::cumulative-length-callback)
        (types . (partial-set))
        ))

    (PartCombineMusic
     . ((description . "Combine two parts on a staff, either merged or
as separate voices.")
        (length-callback . ,ly:music-sequence::maximum-length-callback)
        (start-callback . ,ly:music-sequence::minimum-start-callback)
        (types . (part-combine-music))
        (iterator-ctor . ,ly:part-combine-iterator::constructor)
        ))

    (PercentEvent
     . ((description . "Used internally to signal percent repeats.")
        (types . (event percent-event rhythmic-event))
        ))

    (PercentRepeatedMusic
     . ((description . "Repeats encoded by percents and slashes.")
        (iterator-ctor . ,ly:percent-repeat-iterator::constructor)
        (elements-callback . ,make-percent-set)
        (start-callback .  ,ly:calculated-sequential-music::start)
        (length-callback . ,ly:calculated-sequential-music::length)
        (types . (repeated-music percent-repeated-music))
        ))

    (PesOrFlexaEvent
     . ((description . "Within a ligature, mark the previous and the
following note to form a pes (if melody goes up) or a flexa (if melody
goes down).")
        (types . (pes-or-flexa-event event))
        ))

    (PhrasingSlurEvent
     . ((description . "Start or end phrasing slur.

Syntax: @var{note}@code{\\(} and @var{note}@code{\\)}")
        (types . (post-event span-event event phrasing-slur-event))
        ))

    (PostEvents
     . ((description . "Container for several postevents.

This can be used to package several events into a single one.  Should not be seen outside of the parser.")
        (types . (post-event post-event-wrapper))))

    (PropertySet
     . ((description . "Set a context property.

Syntax: @code{\\set @var{context}.@var{prop} = @var{scheme-val}}")
        (types . (layout-instruction-event))
        (iterator-ctor . ,ly:property-iterator::constructor)
        (untransposable . #t)
        ))

    (PropertyUnset
     . ((description . "Restore the default setting for a context
property.  See @ref{PropertySet}.

Syntax: @code{\\unset @var{context}.@var{prop}}")
        (types . (layout-instruction-event))
        (iterator-ctor . ,ly:property-unset-iterator::constructor)
        ))

    (QuoteMusic
     . ((description . "Quote preprocessed snippets of music.")
        (iterator-ctor . ,ly:music-wrapper-iterator::constructor)
        (length-callback . ,ly:music-wrapper::length-callback)
        (start-callback . ,ly:music-wrapper::start-callback)
        (types . (music-wrapper-music))
        ))

    (RehearsalMarkEvent
     . ((description . "Insert a rehearsal mark.

Syntax: @code{\\mark} @var{marker}

Example: @code{\\mark 3}")
        (types . (rehearsal-mark-event mark-event event))
        ))

    (RelativeOctaveCheck
     . ((description . "Check if a pitch is in the correct octave.")
        (to-relative-callback . ,ly:relative-octave-check::relative-callback)
        (types . (relative-octave-check))
        ))

    (RelativeOctaveMusic
     . ((description . "Music in which the assignment of octaves is complete.")
        (to-relative-callback . ,ly:relative-octave-music::relative-callback)
        (iterator-ctor . ,ly:music-wrapper-iterator::constructor)
        (length-callback . ,ly:music-wrapper::length-callback)
        (start-callback . ,ly:music-wrapper::start-callback)
        (types . (music-wrapper-music relative-octave-music))
        ))

    (RepeatSlashEvent
     . ((description . "Used internally to signal beat repeats.")
        (types . (event repeat-slash-event rhythmic-event))
        ))

    (RepeatTieEvent
     . ((description . "Ties for starting a second volta bracket.")
        (types . (post-event event repeat-tie-event))
        ))

    (RestEvent
     . ((description . "A Rest.

Syntax: @code{r4} for a quarter rest.")
        (iterator-ctor . ,ly:rhythmic-music-iterator::constructor)
        (types . (event rhythmic-event general-rest-event rest-event))
        ))

    (RevertProperty
     . ((description . "The opposite of @ref{OverrideProperty}: remove a
previously added property from a graphical object definition.")
        (types . (layout-instruction-event))
        (iterator-ctor . ,ly:pop-property-iterator::constructor)
        ))

    (ScriptEvent
     . ((description . "Add an articulation mark to a note.")
        (types . (event))
        ))

    (SectionEvent
     . ((description . "Add a section division, which is typically written
as a thin double bar line.")
        (types . (section-event event))
        ))

    (SectionLabelEvent
     . ((description . "Mark the beginning of a named passage.  Does
not imply a section division.")
        (types . (section-label-event event))
        ))

    (SegnoMarkEvent
     . ((description . "Add a segno mark or bar line.")
        (types . (segno-mark-event event))
        ))

    (SegnoRepeatedMusic
     . ((description . "Repeats with alternatives placed sequentially and
marked with segno, Coda, @emph{D.C.}, etc.")
        (iterator-ctor . ,ly:volta-repeat-iterator::constructor)
        (elements-callback . ,make-volta-set)
        (start-callback .  ,ly:calculated-sequential-music::start)
        (length-callback . ,ly:calculated-sequential-music::length)
        (types . (segno-repeated-music folded-repeated-music repeated-music))
        ))

    (SequentialAlternativeMusic
     . ((description . "Repeat alternatives in sequence.

Syntax: @code{\\alternative @{ @var{alternatives} @}}")
        (elements-callback . ,(lambda (m) (ly:music-property m 'elements)))
        (iterator-ctor . ,ly:alternative-sequence-iterator::constructor)
        (length-callback . ,ly:music-sequence::cumulative-length-callback)
        (start-callback . ,ly:music-sequence::first-start-callback)
        (types . (sequential-music sequential-alternative-music))
        ))

    (SequentialMusic
     . ((description . "Music expressions concatenated.

Syntax: @code{\\sequential @{ @dots{} @}} or simply @code{@{ @dots{} @}}")
        (length-callback . ,ly:music-sequence::cumulative-length-callback)
        (start-callback . ,ly:music-sequence::first-start-callback)
        (elements-callback . ,(lambda (m) (ly:music-property m 'elements)))
        (iterator-ctor . ,ly:sequential-iterator::constructor)
        (types . (sequential-music))
        ))

    (SimultaneousMusic
     . ((description . "Music playing together.

Syntax: @code{\\simultaneous @{ @dots{} @}} or @code{<< @dots{} >>}")
        (iterator-ctor . ,ly:simultaneous-music-iterator::constructor)
        (start-callback . ,ly:music-sequence::minimum-start-callback)
        (length-callback . ,ly:music-sequence::maximum-length-callback)
        (to-relative-callback .
                              ,ly:music-sequence::simultaneous-relative-callback)
        (types . (simultaneous-music))
        ))

    (SkipEvent
     . ((description . "Filler that takes up duration, but does not
print anything.

Syntax: @code{s4} for a skip equivalent to a quarter rest.")
        (iterator-ctor . ,ly:rhythmic-music-iterator::constructor)
        (types . (event rhythmic-event skip-event))
        ))

    (SkipMusic
     . ((description . "Filler that takes up duration, does not
print anything, and also does not create staves or voices implicitly.

Syntax: @code{\\skip} @var{duration}")
        (iterator-ctor . ,ly:simple-music-iterator::constructor)
        (types . (event skip-event))
        ))

    (SkippedMusic
     . ((description . "Filler that takes up duration, does not
print anything, and also does not create staves or voices implicitly.

Syntax: @code{\\skip} @var{music}")
        (iterator-ctor . ,ly:simple-music-iterator::constructor)
        (length-callback . ,ly:music-wrapper::length-callback)
        (start-callback . ,ly:music-wrapper::start-callback)
        (types . (skipped-music music-wrapper-music))
        ))

    (SlurEvent
     . ((description . "Start or end slur.

Syntax: @var{note}@code{(} and @var{note}@code{)}")
        (types . (post-event span-event event slur-event))
        ))

    (SoloOneEvent
     . ((description . "Print @q{Solo@tie{}1}.")
        (part-combine-status . solo1)
        (types . (event part-combine-event solo-one-event))
        ))

    (SoloTwoEvent
     . ((description . "Print @q{Solo@tie{}2}.")
        (part-combine-status . solo2)
        (types . (event part-combine-event solo-two-event))
        ))

    (SostenutoEvent
     . ((description . "Depress or release sostenuto pedal.")
        (types . (post-event event pedal-event sostenuto-event))
        ))

    (SpacingSectionEvent
     . ((description . "Start a new spacing section.")
        (types . (event spacing-section-event))))

    (SpanEvent
     . ((description . "Event for anything that is started at a
different time than stopped.")
        (types . (event))
        ))

    (StaffSpanEvent
     . ((description . "Start or stop a staff symbol.")
        (types . (event span-event staff-span-event))
        ))

    (StringNumberEvent
     . ((description . "Specify on which string to play this note.

Syntax: @code{\\@var{number}}")
        (types . (post-event string-number-event event))
        ))

    (StrokeFingerEvent
     . ((description . "Specify with which finger to pluck a string.

Syntax: @code{\\rightHandFinger @var{text}}")
        (types . (post-event stroke-finger-event event))
        ))

    (SustainEvent
     . ((description . "Depress or release sustain pedal.")
        (types . (post-event event pedal-event sustain-event))
        ))

    (TempoChangeEvent
     . ((description . "A metronome mark or tempo indication.")
        (types . (event tempo-change-event))
        ))

    (TextMarkEvent
     . ((description . "A textual mark.

Syntax: @code{\\textMark @var{markup}} or @code{\\textEndMark @var{markup}}.")
        (types . (text-mark-event event))))

    (TextScriptEvent
     . ((description . "Print text.")
        (types . (post-event script-event text-script-event event))
        ))

    (TextSpanEvent
     . ((description . "Start a text spanner, for example, an
octavation.")
        (types . (post-event span-event event text-span-event))
        ))

    (TieEvent
     . ((description . "A tie.

Syntax: @w{@var{note}@code{-~}}")
        (types . (post-event tie-event event))
        ))

    (TimeScaledMusic
     . ((description . "Multiply durations, as in tuplets.

Syntax: @code{\\times @var{fraction} @var{music}}, e.g.,
@code{\\times 2/3 @{ @dots{} @}} for triplets.")
        (length-callback . ,ly:music-wrapper::length-callback)
        (start-callback . ,ly:music-wrapper::start-callback)
        (iterator-ctor . ,ly:tuplet-iterator::constructor)
        (types . (time-scaled-music))
        ))

    (TimeSignatureMusic
     . ((description . "Set a new time signature")
        (iterator-ctor . ,ly:sequential-iterator::constructor)
        (elements-callback . ,make-time-signature-set)
        (types . (time-signature-music))
        ))

    (TimeSignatureEvent
     . ((description . "An event created when setting a new time signature")
        (types . (event time-signature-event))
        ))

    (VowelTransitionEvent
     . ((description . "A vowel transition between lyric syllables.")
        (types . (post-event vowel-transition-event event))
        ))

    (TransposedMusic
     . ((description . "Music that has been transposed.")
        (iterator-ctor . ,ly:music-wrapper-iterator::constructor)
        (start-callback . ,ly:music-wrapper::start-callback)
        (length-callback . ,ly:music-wrapper::length-callback)
        (to-relative-callback .
                              ,ly:relative-octave-music::no-relative-callback)
        (types . (music-wrapper-music transposed-music))
        ))

    (TremoloEvent
     . ((description . "Unmeasured tremolo.")
        (types . (post-event event tremolo-event))
        ))

    (TremoloRepeatedMusic
     . ((description . "Repeated notes denoted by tremolo beams.")
        (iterator-ctor . ,ly:sequential-iterator::constructor)
        (elements-callback . ,make-tremolo-set)
        (start-callback .  ,ly:calculated-sequential-music::start)
        (length-callback . ,ly:calculated-sequential-music::length)
        (types . (repeated-music tremolo-repeated-music))
        ))

    (TremoloSpanEvent
     . ((description . "Tremolo over two stems.")
        (types . (event span-event tremolo-span-event))
        ))

    (TrillSpanEvent
     . ((description . "Start a trill spanner.")
        (types . (post-event span-event event trill-span-event))
        ))

    (TupletSpanEvent
     . ((description . "Used internally to signal where tuplet
brackets start and stop.")
        (types . (tuplet-span-event span-event event post-event))
        ))

    (UnaCordaEvent
     . ((description . "Depress or release una-corda pedal.")
        (types . (post-event event pedal-event una-corda-event))
        ))

    (UnfoldedRepeatedMusic
     . ((description . "Repeated music which is fully written (and
played) out.")
        (iterator-ctor . ,ly:sequential-iterator::constructor)
        (elements-callback . ,make-unfolded-set)
        (start-callback .  ,ly:calculated-sequential-music::start)
        (length-callback . ,ly:calculated-sequential-music::length)
        (types . (repeated-music unfolded-repeated-music))
        ))

    (UnfoldedSpeccedMusic
     . ((description . "Music that appears once repeated music is unfolded.")
        ;; We never want to iterate music that is thus wrapped.  If
        ;; repeats are unfolded, this wrapper is removed and the
        ;; unwrapped music is iterated according to its type.
        (iterator-ctor . ,ly:music-iterator::constructor)
        (length . ,ZERO-MOMENT)
        (types . (unfolded-specification music-wrapper-music))
        ))

    (UnisonoEvent
     . ((description . "Print @q{a@tie{}2}.")
        (part-combine-status . unisono)
        (types . (event part-combine-event unisono-event))))

    (UnrelativableMusic
     . ((description . "Music that cannot be converted from relative
to absolute notation.  For example, transposed music.")
        (to-relative-callback . ,ly:relative-octave-music::no-relative-callback)
        (iterator-ctor . ,ly:music-wrapper-iterator::constructor)
        (length-callback . ,ly:music-wrapper::length-callback)
        (start-callback . ,ly:music-wrapper::start-callback)
        (types . (music-wrapper-music unrelativable-music))
        ))

    (VoiceSeparator
     . ((description . "Separate polyphonic voices in simultaneous music.

Syntax: @code{\\\\}")
        (types . (separator))
        ))

    (VoltaRepeatEndEvent
     . ((description . "Signal the end of a volta-style repeat.  Multiple end
events per start event can be expected when there are alternative endings.")
        (types . (volta-repeat-end-event event))
        ))

    (VoltaRepeatedMusic
     . ((description . "Repeats with alternatives placed sequentially.")
        (iterator-ctor . ,ly:volta-repeat-iterator::constructor)
        (elements-callback . ,make-volta-set)
        (start-callback .  ,ly:calculated-sequential-music::start)
        (length-callback . ,ly:calculated-sequential-music::length)
        (types . (volta-repeated-music folded-repeated-music repeated-music))
        ))

    (VoltaRepeatStartEvent
     . ((description . "Signal the start of a volta-style repeat.")
        (types . (volta-repeat-start-event event))
        ))

    (VoltaSpanEvent
     . ((description . "Used internally to signal where volta
brackets start and stop.")
        (types . (volta-span-event span-event event post-event))
        ))

    (VoltaSpeccedMusic
     . ((description . "Music for a specific volta within repeated music.")
        (iterator-ctor . ,ly:volta-specced-music-iterator::constructor)
        (length-callback . ,ly:music-wrapper::length-callback)
        (start-callback . ,ly:music-wrapper::start-callback)
        (types . (volta-specification music-wrapper-music))
        ))
    ))

(set! music-descriptions
      (sort music-descriptions alist<?))

(define-public music-name-to-property-table (make-hash-table 59))

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
according to @code{music-properties}, a list of alternating property symbols
and values.  Example:

@example
(make-music 'OverrideProperty
            'symbol 'Stem
            'grob-property 'thickness
            'grob-value (* 2 1.5))
@end example

Instead of a successive symbol and value, an entry in the list may
also be an alist or a music object in which case its elements,
respectively its @emph{mutable} property list (properties not inherent
to the type of the music object), are taken.

The argument list will be interpreted left to right, so later entries
override earlier ones."
  (if (not (symbol? name))
      (ly:error (G_ "symbol expected: ~S") name))
  (let ((props (hashq-ref music-name-to-property-table name '())))
    (if (not (pair? props))
        (ly:error (G_ "cannot find music object: ~S") name))
    (let ((m (ly:make-music props)))
      (define (alist-set-props lst)
        (for-each (lambda (e)
                    (set! (ly:music-property m (car e)) (cdr e)))
                  (reverse lst)))
      (define (set-props mus-props)
        (if (pair? mus-props)
            (let ((e (car mus-props))
                  (mus-props (cdr mus-props)))
              (cond ((symbol? e)
                     (set! (ly:music-property m e) (car mus-props))
                     (set-props (cdr mus-props)))
                    ((ly:music? e)
                     (alist-set-props (ly:music-mutable-properties e))
                     (set-props mus-props))
                    ((cheap-list? e)
                     (alist-set-props e)
                     (set-props mus-props))
                    (else
                     (ly:error (G_ "bad make-music argument: ~S") e))))))
      (set-props music-properties)
      m)))
