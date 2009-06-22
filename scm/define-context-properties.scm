;;;; define-context-properties.scm -- part of backend documentation
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;;
;;;; (c) 1998--2009  Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;;                  Jan Nieuwenhuizen <janneke@gnu.org>


(define-public all-translation-properties '())

(define (translator-property-description symbol type? description)
  (if (not (and
	    (symbol? symbol)
	    (procedure? type?)
	    (string? description)))
      (throw 'init-format-error))


  (if (not (equal? #f (object-property symbol 'translation-doc)))
      (ly:error (_ "symbol ~S redefined" symbol)))

  (set-object-property! symbol 'translation-type? type?)
  (set-object-property! symbol 'translation-doc description)
  (set! all-translation-properties (cons symbol all-translation-properties))
  symbol)


(define-public all-user-translation-properties
  (map
   (lambda (x)
     (apply translator-property-description x))
   `(

     ;; TODO FIXME

     (aDueText ,markup? "Text to print at a unisono passage.")
     (alignAboveContext ,string? "Where to insert newly created context in
vertical alignment.")
     (alignBassFigureAccidentals ,boolean? "If true, then the accidentals
are aligned in bass figure context.")
     (alignBelowContext ,string? "Where to insert newly created context in
vertical alignment.")
     (associatedVoice ,string? "Name of the @code{Voice} that has the
melody for this @code{Lyrics} line.")
     (autoAccidentals ,list? "List of different ways to typeset an
accidental.

For determining when to print an accidental, several different rules
are tried.  The rule that gives the highest number of accidentals is
used.

Each entry in the list is either a symbol or a procedure.

@table @var

@item symbol
The symbol is the name of the context in which the following rules are to be
applied. For example, if @var{context} is @rinternals{Score} then all
staves share accidentals, and if @var{context} is @rinternals{Staff} then
all voices in the same staff share accidentals, but staves do not.

@item procedure
The procedure represents an accidental rule to be applied to the previously
specified context.

The procedure takes the following arguments:

@table @code

@item context
The current context to which the rule should be applied.

@item pitch
The pitch of the note to be evaluated.

@item barnum
The current bar number.

@item measurepos
The current measure position.

@end table

The procedure returns a pair of booleans. The first states whether an extra
natural should be added. The second states whether an accidental should be
printed. @code{(#t . #f)} does not make sense.

@end table")
     (autoBeamCheck ,procedure? "A procedure taking three
arguments, @var{context}, @var{dir} [start/stop (-1 or 1)], and
@var{test} [shortest note in the beam].  A non-@code{#f} return value
starts or stops the auto beam.")
     (autoBeaming ,boolean? "If set to true then beams are generated
automatically.")
     (autoCautionaries ,list? "List similar to @code{autoAccidentals},
but it controls cautionary accidentals rather than normal ones.  Both
lists are tried, and the one giving the most accidentals wins.  In
case of draw, a normal accidental is typeset.")
     (automaticBars ,boolean? "If set to false then bar lines will not
be printed automatically; they must be explicitly created with a
@code{\\bar} command.  Unlike the @code{\\cadenzaOn} keyword, measures
are still counted.  Bar line generation will resume according to that
count if this property is unset.")


     (barAlways ,boolean? "If set to true a bar line is drawn after
each note.")
     (barCheckSynchronize ,boolean? "If true then reset
@code{measurePosition} when finding a bar check.")
     (barNumberVisibility ,procedure? "A Procedure that takes an
integer and returns whether the corresponding bar number should be
printed.")
     (bassFigureFormatFunction ,procedure? "A procedure that is
called to produce the formatting for a @code{BassFigure} grob.  It
takes a list of @code{BassFigureEvent}s, a context, and the grob to
format.")
     (bassStaffProperties ,list? "An alist of property settings to
apply for the down staff of @code{PianoStaff}.  Used by
@code{\\autochange}.")
     (beamSettings ,list? "Specifies when automatically generated
beams should begin and end, as well as beam subdivision behavior.
See @ruser{Setting automatic beam
behavior} for more information.")
     (beatLength ,ly:moment? "The length of one beat in this time
signature.")


     (chordChanges ,boolean? "Only show changes in chords scheme?")
     (chordNameExceptions ,list? "An alist of chord exceptions.
Contains @code{(@var{chord} . @var{markup})} entries.")
     (chordNameExceptionsFull ,list? "An alist of full chord
exceptions.  Contains @code{(@var{chord} . @var{markup})} entries.")
     (chordNameExceptionsPartial ,list? "An alist of partial chord
exceptions.  Contains @code{(@var{chord} . (@var{prefix-markup}
@var{suffix-markup}))} entries.")
     (chordNameFunction ,procedure? "The function that converts lists
of pitches to chord names.")
     (chordNameSeparator ,markup? "The markup object used to
separate parts of a chord name.")
     (chordNoteNamer ,procedure? "A function that converts from a pitch
object to a text markup.  Used for single pitches.")
     (chordPrefixSpacer ,number? "The space added between the root
symbol and the prefix of a chord name.")
     (chordRootNamer ,procedure? "A function that converts from a pitch
object to a text markup.  Used for chords.")
     (clefGlyph ,string? "Name of the symbol within the music font.")
     (clefOctavation ,integer? "Add this much extra octavation.
Values of 7 and -7 are common.")
     (clefPosition ,number? "Where should the center of the clef
symbol go, measured in half staff spaces from the center of the
staff.")
     (completionBusy ,boolean? "Whether a completion-note head is playing.")
     (connectArpeggios ,boolean? "If set, connect arpeggios across
piano staff.")
     (countPercentRepeats ,boolean? "If set, produce counters for
percent repeats.")
     (createKeyOnClefChange ,boolean? "Print a key signature whenever
the clef is changed.")
     (createSpacing ,boolean? "Create @code{StaffSpacing} objects?
Should be set for staves.")
     (crescendoSpanner ,symbol? "The type of spanner to be used for
crescendi.  Available values are @samp{hairpin} and @samp{text}.  If unset,
a hairpin crescendo is used.")
     (crescendoText ,markup? "The text to print at start of non-hairpin
crescendo, i.e., @samp{cresc.}.")
     (currentBarNumber ,integer? "Contains the current barnumber.
This property is incremented at every bar line.")


     (decrescendoSpanner ,symbol? "The type of spanner to be used for
decrescendi.  Available values are @samp{hairpin} and @samp{text}.  If
unset, a hairpin decrescendo is used.")
     (decrescendoText ,markup? "The text to print at start of
non-hairpin decrescendo, i.e., @samp{dim.}.")
     (defaultBarType ,string? "Set the default type of bar line.  See
@code{whichBar} for information on available bar types.

This variable is read by @rinternals{Timing_translator} at
@rinternals{Score} level.")
     (doubleRepeatType ,string? "Set the default bar line for double
repeats.")
     (doubleSlurs ,boolean? "If set, two slurs are created for every
slurred note, one above and one below the chord.")
     (drumPitchTable ,hash-table? "A table mapping percussion
instruments (symbols) to pitches.")
     (drumStyleTable ,hash-table? "A hash table which maps drums to
layout settings.  Predefined values: @samp{drums-style},
@samp{timbales-style}, @samp{congas-style}, @samp{bongos-style}, and
@samp{percussion-style}.

The layout style is a hash table, containing the drum-pitches (e.g.,
the symbol @samp{hihat}) as keys, and a list
@code{(@var{notehead-style} @var{script} @var{vertical-position})} as
values.")


     (explicitClefVisibility ,vector? "@samp{break-visibility}
function for clef changes.")
     (explicitKeySignatureVisibility ,vector? "@samp{break-visibility}
function for explicit key changes.  @samp{\\override} of the
@code{break-visibility} property will set the visibility for normal
(i.e., at the start of the line) key signatures.")
     (extendersOverRests ,boolean? "Whether to continue extenders as
they cross a rest.")
     (extraNatural ,boolean? "Whether to typeset an extra natural sign
before accidentals changing from a non-natural to another
non-natural.")


     (figuredBassAlterationDirection ,ly:dir? "Where to put alterations
relative to the main figure.")
     (figuredBassCenterContinuations ,boolean? "Whether to vertically
center pairs of extender lines.  This does not work with three or more
lines.")
     (figuredBassFormatter ,procedure? "A routine generating a markup
for a bass figure.")
     (figuredBassPlusDirection ,ly:dir? "Where to put plus signs
relative to the main figure.")
     (fingeringOrientations ,list? "A list of symbols, containing
@samp{left}, @samp{right}, @samp{up} and/or @samp{down}.  This list
determines where fingerings are put relative to the chord being
fingered.")
     (firstClef ,boolean? "If true, create a new clef when starting a
staff.")
     (followVoice ,boolean? "If set, note heads are tracked across
staff switches by a thin line.")
     (fontSize ,number? "The relative size of all grobs in a context.")
     (forbidBreak ,boolean? "If set to @code{##t}, prevent a line break
at this point.")
     (forceClef ,boolean? "Show clef symbol, even if it has not
changed.  Only active for the first clef after the property is set, not
for the full staff.")


     (gridInterval ,ly:moment? "Interval for which to generate
@code{GridPoint}s.")


     (harmonicAccidentals ,boolean? "If set, harmonic notes in chords
get accidentals.")
     (harmonicDots ,boolean? "If set, harmonic notes in dotted chords get
dots.")
     (highStringOne ,boolean? "Whether the first string is the string
with highest pitch on the instrument.  This used by the automatic
string selector for tablature notation.")


     (ignoreBarChecks ,boolean? "Ignore bar checks.")
     (ignoreFiguredBassRest ,boolean? "Don't swallow rest events.")
     (ignoreMelismata ,boolean? "Ignore melismata for this
@rinternals{Lyrics} line.")
     (implicitBassFigures ,list? "A list of bass figures that are not
printed as numbers, but only as extender lines.")
     (implicitTimeSignatureVisibility ,vector? "break visibility for
the default time signature.")
     (instrumentCueName ,markup? "The name to print if another
instrument is to be taken.")
     (instrumentEqualizer ,procedure? "A function taking a string
(instrument name), and returning a @code{(@var{min} . @var{max})} pair
of numbers for the loudness range of the instrument.")
     (instrumentName ,markup? "The name to print left of a staff.  The
@code{instrument} property labels the staff in the first system, and
the @code{instr} property labels following lines.")
     ;; the definition is reversed wrt traditional transposition
     ;; otherwise \transpose { \transposition .. } won't work
     (instrumentTransposition ,ly:pitch? "Define the transposition of
the instrument.  Its value is the pitch that sounds like middle@tie{}C.
This is used to transpose the MIDI output, and @code{\\quote}s.")
     (internalBarNumber ,integer? "Contains the current barnumber.
This property is used for internal timekeeping, among others by the
@code{Accidental_engraver}.")


     (keepAliveInterfaces ,list? "A list of symbols, signifying grob
interfaces that are worth keeping a staff with @code{remove-empty} set
around for.")
     (keyAlterationOrder ,list? "An alist that defines in what order
alterations should be printed.  The format is @code{(@var{step}
. @var{alter})}, where @var{step} is a number from 0 to@tie{}6 and
@var{alter} from -2 (sharp) to 2 (flat).")
     (keySignature ,list? "The current key signature.  This is an alist
containing @code{(@var{step} . @var{alter})} or @code{((@var{octave} .
@var{step}) . @var{alter})},  where @var{step} is a number in the range
0 to@tie{}6 and @var{alter} a fraction, denoting alteration.  For
alterations, use symbols, e.g. @code{keySignature = #`((6 . ,FLAT))}.")


     (lyricMelismaAlignment ,ly:dir? "Alignment to use for a melisma syllable.")


     (majorSevenSymbol ,markup? "How should the major 7th be formatted
in a chord name?")
     (markFormatter ,procedure? "A procedure taking as arguments the
context and the rehearsal mark.  It should return the formatted mark as
a markup object.")
     (maximumFretStretch ,number? "Don't allocate frets further than
this from specified frets.")
     (measureLength ,ly:moment? "Length of one measure in the current
time signature.")
     (measurePosition ,ly:moment? "How much of the current measure have
we had.  This can be set manually to create incomplete measures.")
     (melismaBusyProperties ,list? "A list of properties (symbols) to
determine whether a melisma is playing.  Setting this property will
influence how lyrics are aligned to notes.  For example, if set to
@code{#'(melismaBusy beamMelismaBusy)}, only manual melismata and
manual beams are considered.  Possible values include
@code{melismaBusy}, @code{slurMelismaBusy}, @code{tieMelismaBusy}, and
@code{beamMelismaBusy}.")
     (metronomeMarkFormatter ,procedure? "How to produce a metronome
markup.  Called with four arguments: text, duration, count and context.")
     (middleCClefPosition ,number? "The position of the middle C,
as determined only by the clef.  This can be calculated by looking at
@code{clefPosition} and @code{clefGlyph}.")
     (middleCOffset ,number? "The offset of
middle C from the position given by @code{middleCClefPosition} This
is used for ottava brackets.")
     (middleCPosition ,number? "The place of the middle C, measured in
half staff-spaces.  Usually determined by looking at
@code{middleCClefPosition} and @code{middleCOffset}.")
     (midiInstrument ,string? "Name of the MIDI instrument to use.")
     (midiMaximumVolume ,number? "Analogous to
@code{midiMinimumVolume}.")
     (midiMinimumVolume ,number? "Set the minimum loudness for MIDI.
Ranges from 0 to@tie{}1.")
     (minimumFret ,number? "The tablature auto string-selecting
mechanism selects the highest string with a fret at least
@code{minimumFret}.")
     (minimumPageTurnLength ,ly:moment? "Minimum length of a rest for a
page turn to be allowed.")
     (minimumRepeatLengthForPageTurn ,ly:moment? "Minimum length of a
repeated section for a page turn to be allowed within that section.")


     (noChordSymbol ,markup? "Markup to be displayed for rests in a
ChordNames context.")
     (noteToFretFunction ,procedure? "How to produce a fret diagram.
Parameters: A list of note events and a list of tabstring events.")


     (ottavation ,markup? "If set, the text for an ottava spanner.
Changing this creates a new text spanner.")
     (output ,ly:music-output? "The output produced by a score-level
translator during music interpretation.")


     (pedalSostenutoStrings ,list? "See @code{pedalSustainStrings}.")
     (pedalSostenutoStyle ,symbol? "See @code{pedalSustainStyle}.")
     (pedalSustainStrings ,list? "A list of strings to print for
sustain-pedal.  Format is @code{(@var{up} @var{updown} @var{down})},
where each of the three is the string to print when this is done with
the pedal.")
     (pedalSustainStyle ,symbol? "A symbol that indicates how to print
sustain pedals: @code{text}, @code{bracket} or @code{mixed} (both).")
     (pedalUnaCordaStrings ,list? "See @code{pedalSustainStrings}.")
     (pedalUnaCordaStyle ,symbol? "See @code{pedalSustainStyle}.")
     (predefinedDiagramTable ,hash-table? "The hash table of predefined
fret diagrams to use in FretBoards.")
     (printKeyCancellation ,boolean? "Print restoration alterations
before a key signature change.")
     (printOctaveNames ,boolean? "Print octave marks for the
@code{NoteNames} context.")
     (printPartCombineTexts ,boolean? "Set @q{Solo} and @q{A due} texts
in the part combiner?")
     (proportionalNotationDuration ,ly:moment? "Global override for
shortest-playing duration.  This is used for switching on proportional
notation.")


     (recordEventSequence ,procedure? "When
@code{Recording_group_engraver} is in this context, then upon
termination of the context, this function is called with current
context and a list of music objects.  The list of contains entries with
start times, music objects and whether they are processed in this
context.")
     (rehearsalMark ,integer? "The last rehearsal mark printed.")
     (repeatCommands ,list? "This property is a list of commands
of the form @code{(list 'volta @var{x})}, where @var{x} is a string or
@code{#f}.  @code{'end-repeat} is also accepted as a command.")
     (repeatCountVisibility ,procedure? "A procedure taking as
arguments an integer and context, returning whether the corresponding
percent repeat number should be printed when @code{countPercentRepeats}
is set.")
     (restNumberThreshold ,number? "If a multimeasure rest has more
measures than this, a number is printed.")


     (shapeNoteStyles ,vector? "Vector of symbols, listing style for
each note head relative to the tonic (qv.) of the scale.")
     (shortInstrumentName ,markup? "See @code{instrument}.")
     (shortVocalName ,markup? "Name of a vocal line, short version.")
     (skipBars ,boolean? "If set to true, then skip the empty bars
that are produced by multimeasure notes and rests.  These bars will
not appear on the printed output.  If not set (the default),
multimeasure notes and rests expand into their full length, printing
the appropriate number of empty bars so that synchronization with other
voices is preserved.

@example
@{
  r1 r1*3 R1*3
  \\set Score.skipBars= ##t
  r1*3 R1*3
@}
@end example")
     (skipTypesetting ,boolean? "If true, no typesetting is done,
speeding up the interpretation phase.  Useful for debugging large
scores.")
     (soloIIText ,markup? "The text for the start of a solo for
voice @q{two} when part-combining.")
     (soloText ,markup? "The text for the start of a solo when
part-combining.")
     (squashedPosition ,integer? "Vertical position of squashing for
@rinternals{Pitch_squash_engraver}.")
     (staffLineLayoutFunction ,procedure? "Layout of staff lines,
@code{traditional}, or @code{semitone}.")
     (stanza ,markup? "Stanza @q{number} to print before the start of a
verse.  Use in @code{Lyrics} context.")
     (stemLeftBeamCount ,integer? "Specify the number of beams to draw
on the left side of the next note.  Overrides automatic beaming.  The
value is only used once, and then it is erased.")
     (stemRightBeamCount ,integer? "See @code{stemLeftBeamCount}.")
     (stringNumberOrientations ,list? "See
@code{fingeringOrientations}.")
     (stringOneTopmost ,boolean? "Whether the first string is
printed on the top line of the tablature.")
     (stringTunings ,list? "The tablature strings tuning.  It is a list
of the pitch (in semitones) of each string (starting with the lower
one).")
     (strokeFingerOrientations ,list? "See
@code{fingeringOrientations}.")
     (subdivideBeams ,boolean? "If set, multiple beams will be
subdivided at beat positions by only drawing one beam over the beat.")
     (suggestAccidentals ,boolean? "If set, accidentals are typeset as
cautionary suggestions over the note.")
     (systemStartDelimiter ,symbol? "Which grob to make for the start
of the system/staff?  Set to @code{SystemStartBrace},
@code{SystemStartBracket} or @code{SystemStartBar}.")
     (systemStartDelimiterHierarchy ,pair? "A nested list, indicating
the nesting of a start delimiters.")


     (tablatureFormat ,procedure? "A function formatting a tablature
note head.  Called with three arguments: string number, context and event.
It returns the text as a string.")
     (tempoHideNote ,boolean? "Hide the note=count in tempo marks.")
     (tempoText ,markup? "Text for tempo marks.")
     (tempoUnitCount ,number? "Count for specifying tempo.")
     (tempoUnitDuration ,ly:duration? "Unit for specifying tempo.")
     (tempoWholesPerMinute ,ly:moment? "The tempo in whole notes per
minute.")
     (tieWaitForNote ,boolean? "If true, tied notes do not have to
follow each other directly.  This can be used for writing out
arpeggios.")
     (timeSignatureFraction ,number-pair? "A pair of numbers,
signifying the time signature.  For example, @code{#'(4 . 4)} is a
4/4 time signature.")
     (timing ,boolean? "Keep administration of measure length,
position, bar number, etc.?  Switch off for cadenzas.")
     (tonic ,ly:pitch? "The tonic of the current scale.")
     (topLevelAlignment ,boolean? "If true, the @var{Vertical_align_engraver}
will create a @var{VerticalAlignment}; otherwise, it will create a
@var{StaffGrouper}")
     (trebleStaffProperties ,list? "An alist of property settings to
apply for the up staff of @code{PianoStaff}.  Used by
@code{\\autochange}.")
     (tremoloFlags ,integer? "The number of tremolo flags to add if no
number is specified.")
     (tupletFullLength ,boolean? "If set, the tuplet is printed up to
the start of the next note.")
     (tupletFullLengthNote ,boolean? "If set, end at the next note,
otherwise end on the matter (time signatures, etc.) before the note.")
     (tupletSpannerDuration ,ly:moment? "Normally, a tuplet bracket is
as wide as the @code{\\times} expression that gave rise to it.  By
setting this property, you can make brackets last shorter.

@example
@{
  \\set tupletSpannerDuration = #(ly:make-moment 1 4)
  \\times 2/3 @{ c8 c c c c c @}
@}
@end example")


     (useBassFigureExtenders ,boolean? "Whether to use extender lines
for repeated bass figures.")

     (verticallySpacedContexts ,list? "List of symbols, containing
context names whose vertical axis groups should be taken into account
for vertical spacing of systems.")
     (vocalName ,markup? "Name of a vocal line.")
     (voltaSpannerDuration ,ly:moment? "This specifies the maximum
duration to use for the brackets printed for @code{\\alternative}.
This can be used to shrink the length of brackets in the situation
where one alternative is very large.")


     (whichBar ,string? "This property is read to determine what type
of bar line to create.

Example:

@example
\\set Staff.whichBar = \"|:\"
@end example

@noindent
This will create a start-repeat bar in this staff only.  Valid values
are described in @rinternals{bar-line-interface}.")
     )))


(define-public all-internal-translation-properties
  (map
   (lambda (x)
     (set-object-property! (car x) 'internal-translation #t)
     (apply translator-property-description x))

   `(

     (associatedVoiceContext ,ly:context? "The context object of the
@code{Voice} that has the melody for this @code{Lyrics}.")


     (barCheckLastFail ,ly:moment? "Where in the measure did the last
barcheck fail?")
     (beamMelismaBusy ,boolean? "Signal if a beam is present.")
     (breakableSeparationItem ,ly:grob? "The breakable items in this
time step, for this staff.")
     (busyGrobs ,list? "A queue of @code{(@var{end-moment} .
@var{GROB})} cons cells.  This is for internal (C++) use only.  This
property contains the grobs which are still busy (e.g. note heads,
spanners, etc.).")


     (currentCommandColumn ,ly:grob? "Grob that is X-parent to all
current breakable (clef, key signature, etc.) items.")
     (currentMusicalColumn ,ly:grob? "Grob that is X-parent to all
non-breakable items (note heads, lyrics, etc.).")


     (dynamicAbsoluteVolumeFunction ,procedure? "A procedure that takes
one argument, the text value of a dynamic event, and returns the absolute
volume of that dynamic event.")


     (finalizations ,list? "A list of expressions to evaluate before
proceeding to next time step.  This is an internal variable.")


     (graceSettings ,list? "Overrides for grace notes.  This property
should be manipulated through the @code{add-grace-property} function.")


     (hasStaffSpacing ,boolean? "True if the current
@code{CommandColumn} contains items that will affect spacing.")


     (instrumentSupport ,grob-list? "A list of grobs to attach the
instrument name to.")


     (lastKeySignature ,list? "Last key signature before a key
signature change.")
     (localKeySignature ,list? "The key signature at this point in the
measure.  The format is the same as for @code{keySignature}, but can
also contain @code{((@var{octave} . @var{name}) . (@var{alter}
@var{barnumber} . @var{measureposition}))} pairs.")


     (melismaBusy ,boolean? "Signifies whether a melisma is active.
This can be used to signal melismas on top of those automatically
detected.")


     (originalMiddleCPosition ,integer? "Used for temporary overriding
middle@tie{}C in octavation brackets.")


     (quotedEventTypes ,list? "A list of symbols, representing the
event types that should be duplicated for @code{\\quote} commands.")
;    (quotes ,hash-table? "A hash table, mapping names to
;@code{music-event} vectors.")


     (rootSystem ,ly:grob? "The System object.")


     (scriptDefinitions ,list? "The description of scripts.  This is
used by the @code{Script_engraver} for typesetting note-superscripts
and subscripts.  See @file{scm/script.scm} for more information.")
     (slurMelismaBusy ,boolean? "Signal if a slur is present.")
     (stavesFound ,grob-list? "A list of all staff-symbols found.")


     (tieMelismaBusy ,boolean? "Signal whether a tie is present.")
     )))

(define-public all-translation-properties
  (append all-user-translation-properties
	  all-internal-translation-properties))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public default-melisma-properties
  '(melismaBusy slurMelismaBusy tieMelismaBusy beamMelismaBusy completionBusy))
