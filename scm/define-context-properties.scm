;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 1998--2022  Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;;                  Jan Nieuwenhuizen <janneke@gnu.org>
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


(define-public all-translation-properties '())

(define (translator-property-description symbol type? description)
  (if (not (and
            (symbol? symbol)
            (procedure? type?)
            (string? description)))
      (throw 'init-format-error))


  (if (not (equal? #f (object-property symbol 'translation-doc)))
      (ly:error (G_ "symbol ~S redefined") symbol))

  (set-object-property! symbol 'translation-type? type?)
  (set-object-property! symbol 'translation-doc description)
  (set! all-translation-properties (cons symbol all-translation-properties))
  symbol)


;;; Both the user and internal context properties are sorted alphabetically
;;; (ignoring case).

(define-public all-user-translation-properties
  (map
   (lambda (x)
     (apply translator-property-description x))
   `(

     ;; TODO FIXME

     (accidentalGrouping ,symbol? "If set to @code{'voice}, accidentals
on the same note in different octaves may be horizontally staggered
if in different voices.")
     (additionalBassStrings ,list? "The additional tablature bass-strings, which
will not get a seprate line in TabStaff.  It is a list of the pitches of each
string (starting with the lowest numbered one).")
     (additionalPitchPrefix ,string? "Text with which to prefix
additional pitches within a chord name.")
     (aDueText ,markup? "Text to print at a unisono passage.")
     (alignAboveContext ,string? "Where to insert newly created context in
vertical alignment.")
     (alignBelowContext ,string? "Where to insert newly created context in
vertical alignment.")
     (alterationGlyphs ,list? "Alist mapping alterations to accidental glyphs.
Alterations are given as exact numbers, e.g., -1/2 for flat.  This applies
to all grobs that can print accidentals.")
     (alternativeNumber ,index? "When set, the index of the current
@code{\\alternative} element, starting from one.  Not set outside of
alternatives.  Note the distinction from volta number: an alternative
may pertain to multiple volte.")
     (alternativeNumberingStyle ,symbol? "The scheme and style for
numbering bars in repeat alternatives.  If not set (the default), bar
numbers continue through alternatives.  Can be set to @code{numbers}
to reset the bar number at each alternative, or set to
@code{numbers-with-letters} to reset and also include letter
suffixes.")
     (alternativeRestores ,symbol-list? "Timing variables that are
restored to their value at the start of the first alternative in
subsequent alternatives.")
     (associatedVoice ,string? "Name of the context (see
@code{associatedVoiceType} for its type, usually @code{Voice}) that
has the melody for this @code{Lyrics} line.")
     (associatedVoiceType ,symbol? "Type of the context that has the
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
applied.  For example, if @var{context} is @rinternals{Score} then all
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

@end table

The procedure returns a pair of booleans.  The first states whether an extra
natural should be added.  The second states whether an accidental should be
printed.  @code{(#t . #f)} does not make sense.

@end table")
     (autoBeamCheck ,procedure? "A procedure taking three
arguments, @var{context}, @var{dir} [start/@/stop (-1 or 1)], and
@var{test} [shortest note in the beam].  A non-@code{#f} return value
starts or stops the auto beam.")
     (autoBeaming ,boolean? "If set to true then beams are generated
automatically.")
     (autoCautionaries ,list? "List similar to @code{autoAccidentals},
but it controls cautionary accidentals rather than normal ones.  Both
lists are tried, and the one giving the most accidentals wins.  In
case of draw, a normal accidental is typeset.")


     (barCheckSynchronize ,boolean? "If true then reset
@code{measurePosition} when finding a bar check.")
     (barExtraVelocity ,integer? "Extra MIDI velocity added by the
@samp{Beat_performer} at the start of each measure.")
     (barNumberFormatter ,procedure? "A procedure that takes a bar
number, measure position, and alternative number and returns a markup
of the bar number to print.")
     (barNumberVisibility ,procedure? "A procedure that takes a bar
number and a measure position and returns whether the corresponding
bar number should be printed.  Note that the actual print-out of
bar numbers is controlled with the @code{break-visibility} property.

The following procedures are predefined:

@table @code
@item all-bar-numbers-visible
Enable bar numbers for all bars, including the first one and broken
bars (which get bar numbers in parentheses).

@item first-bar-number-invisible
Enable bar numbers for all bars (including broken bars) except the
first one.  If the first bar is broken, it doesn't get a bar number
either.

@item first-bar-number-invisible-save-broken-bars
Enable bar numbers for all bars (including broken bars) except the
first one.  A broken first bar gets a bar number.

@item first-bar-number-invisible-and-no-parenthesized-bar-numbers
Enable bar numbers for all bars except the first bar and broken bars.
This is the default.

@item (every-nth-bar-number-visible @var{n})
Assuming @var{n} is value@tie{}2, for example, this enables bar numbers
for bars 2, 4, 6, etc.

@item (modulo-bar-number-visible @var{n} @var{m})
If bar numbers 1, 4, 7, etc., should be enabled, @var{n}@tie{}(the modulo)
must be set to@tie{}3 and @var{m}@tie{}(the division remainder) to@tie{}1.
@end table")
     (baseMoment ,ly:moment? "Smallest unit of time that will stand on its
own as a subdivided section.")
     (beamExceptions ,list? "An alist of exceptions to autobeam rules
that normally end on beats.")
     (beamHalfMeasure ,boolean? "Whether to allow a beam to begin
halfway through the measure in triple time, which could look like 6/8.")
     (beatExtraVelocity ,integer? "Extra MIDI velocity added by the
@samp{Beat_performer} at the start of each beat.")
     (beatStructure ,list? "List of @code{baseMoment}s that are combined
to make beats.")
     (breathMarkType ,symbol? "The type of @code{BreathingSign} to create at
@code{\\breathe}.")


     (caesuraType ,list? "An alist

@example
((bar-line . @var{bar-type})
 (breath . @var{breath-type})
 (scripts . @var{script-type}@dots{})
 (underlying-bar-line . @var{bar-type}))
@end example

@noindent
specifying which breath mark, bar line, and scripts to create at
@code{\\caesura}.  All entries are optional.

@code{bar-@/line} has higher priority than a measure bar line and
@code{underlying-@/bar-@/line} has lower priority than a measure bar
line.")
     (caesuraTypeTransform ,procedure? "An engraver callback taking
three arguments and returning an alist of the same kind as
@code{caesuraType}.

The first argument is the context.

The second argument is the value of @code{caesuraType} with an
additional entry @code{(articulations . @var{symbol-@/list})}
identifying the articulations attached to the caesura in the music.
If the transform function returns this second argument unmodified, it
is as if no transform function were set; the function is free to
return a different value.  The transform function can remove
articulations, but any added articulations are ignored.

The third argument is a symbol-list identifying certain things the
engraver has observed.  @code{bar-@/line} indicates that the engraver
has observed a @code{BarLine} at the current moment.")
     (centerBarNumbers ,boolean? "Whether to center bar numbers in
their measure instead of aligning them on the bar line.")
     (chordChanges ,boolean? "Only show changes in chords scheme?")
     (chordNameExceptions ,list? "An alist of chord exceptions.
Contains @code{(@var{chord} . @var{markup})} entries.")
     (chordNameFunction ,procedure? "The function that converts lists
of pitches to chord names.")
     (chordNameLowercaseMinor ,boolean? "Downcase roots of minor chords?")
     (chordNameSeparator ,markup? "The markup object used to
separate parts of a chord name.")
     (chordNoteNamer ,procedure? "A function that converts from a pitch
object to a text markup.  Used for single pitches.")
     (chordPrefixSpacer ,number? "The space added between the root
symbol and the prefix of a chord name.")
     (chordRootNamer ,procedure? "A function that converts from a pitch
object to a text markup.  Used for chords.")
     (clefGlyph ,string? "Name of the symbol within the music font.")
     (clefPosition ,number? "Where should the center of the clef
symbol go, measured in half staff spaces from the center of the
staff.")
     (clefTransposition ,integer? "Add this much extra transposition.
Values of 7 and -7 are common.")
     (clefTranspositionFormatter ,procedure? "A procedure that takes the
Transposition number as a string and the style as a symbol and returns a
markup.")
     (clefTranspositionStyle ,symbol? "Determines the way the ClefModifier
grob is displayed.  Possible values are @samp{default}, @samp{parenthesized}
and @samp{bracketed}.")
     (codaMarkFormatter ,procedure? "A procedure that creates a coda
mark (which in conventional @emph{D.S. al Coda} form indicates the
start of the alternative endings), taking as arguments the mark
sequence number and the context.  It should return a markup object.")
     (completionBusy ,boolean? "Whether a completion-note head is playing.")
     (completionFactor ,rational-or-procedure?
                       "When @code{Completion_heads_engraver} and
@code{Completion_rest_engraver} need to split a note or rest with a
scaled duration, such as @code{c2*3}, this specifies the scale factor
to use for the newly-split notes and rests created by the engraver.

If @code{#f}, the completion engraver uses the scale-factor of
each duration being split.

If set to a callback procedure, that procedure is called with the
context of the completion engraver, and the duration to be split.")
     (completionUnit ,ly:moment? "Sub-bar unit of completion.")
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
     (cueClefGlyph ,string? "Name of the symbol within the music font.")
     (cueClefPosition ,number? "Where should the center of the clef
symbol go, measured in half staff spaces from the center of the
staff.")
     (cueClefTransposition ,integer? "Add this much extra transposition.
Values of 7 and -7 are common.")
     (cueClefTranspositionFormatter ,procedure? "A procedure that
takes the Transposition number as a string and the style as a symbol
and returns a markup.")
     (cueClefTranspositionStyle ,symbol? "Determines the way the ClefModifier
grob is displayed.  Possible values are @samp{default}, @samp{parenthesized}
and @samp{bracketed}.")
     (currentBarNumber ,integer? "Contains the current barnumber.
This property is incremented at every bar line.")


     (dalSegnoTextFormatter ,procedure? "Format a jump instruction
such as @emph{D.S.}

The first argument is the context.

The second argument is the number of times the instruction is
performed.

The third argument is a list of three markups: @var{start-markup},
@var{end-markup}, and @var{next-markup}.

If @var{start-markup} is @code{#f}, the form is @emph{da capo};
otherwise the form is @emph{dal segno} and @var{start-markup} is the
sign at the start of the repeated section.

If @var{end-markup} is not @code{#f}, it is either the sign at the end
of the main body of the repeat, or it is a @emph{Fine} instruction.
When it is a Fine instruction, @var{next-markup} is @code{#f}.

If @var{next-markup} is not @code{#f}, it is the mark to be jumped to
after performing the body of the repeat, e.g., Coda.")
     (decrescendoSpanner ,symbol? "The type of spanner to be used for
decrescendi.  Available values are @samp{hairpin} and @samp{text}.  If
unset, a hairpin decrescendo is used.")
     (decrescendoText ,markup? "The text to print at start of
non-hairpin decrescendo, i.e., @samp{dim.}.")
     (defaultStrings ,list? "A list of strings to use in calculating
frets for tablatures and fretboards if no strings are provided in
the notes for the current moment.")
     (doubleRepeatBarType ,string? "Bar line to insert where the end
of one @code{\\repeat volta} coincides with the start of another.  The
default is @samp{:..:}.")
     (doubleRepeatSegnoBarType ,string? "Bar line to insert where an
in-staff segno coincides with the end of one @code{\\repeat volta} and
the beginning of another.  The default is @samp{:|.S.|:}.")
     (doubleSlurs ,boolean? "If set, two slurs are created for every
slurred note, one above and one below the chord.")
     (drumPitchTable ,hash-table? "A table mapping percussion
instruments (symbols) to pitches.")
     (drumStyleTable ,hash-table? "A hash table which maps drums to
layout settings.  Predefined values: @samp{drums-style},
@samp{agostini-drums-style}, @samp{weinberg-drums-style}, @samp{timbales-style}, @samp{congas-style},
@samp{bongos-style}, and @samp{percussion-style}.

The layout style is a hash table, containing the drum-pitches (e.g.,
the symbol @samp{hihat}) as keys, and a list
@code{(@var{notehead-style} @var{script} @var{vertical-position})} as
values.")


     (endAtSkip ,boolean? "End @code{DurationLine} grob on @code{skip-event}")
     (endRepeatBarType ,string? "Bar line to insert at the end of a
@code{\\repeat volta}.  The default is @samp{:|.}.")
     (endRepeatSegnoBarType ,string? "Bar line to insert where an in-staff
segno coincides with the end of a @code{\\repeat volta}.  The default is
@samp{:|.S}.")
     (explicitClefVisibility ,vector? "@samp{break-visibility}
function for clef changes.")
     (explicitCueClefVisibility ,vector? "@samp{break-visibility}
function for cue clef changes.")
     (explicitKeySignatureVisibility ,vector? "@samp{break-visibility}
function for explicit key changes.  @samp{\\override} of the
@code{break-visibility} property will set the visibility for
normal (i.e., at the start of the line) key signatures.")
     (extendersOverRests ,boolean? "Whether to continue extenders as
they cross a rest.")
     (extraNatural ,boolean? "Whether to typeset an extra natural sign
before accidentals that reduce the effect of a previous alteration.")


     (figuredBassAlterationDirection ,ly:dir? "Where to put alterations
relative to the main figure.")
     (figuredBassCenterContinuations ,boolean? "Whether to vertically
center pairs of extender lines.  This does not work with three or more
lines.")
     (figuredBassFormatter ,procedure? "A routine generating a markup
for a bass figure.")
     (figuredBassLargeNumberAlignment ,number? "Horizontal alignment
to use for numbers in figured bass that contain more than a single
digit.")
     (figuredBassPlusDirection ,ly:dir? "Where to put plus signs
relative to the main figure.")
     (figuredBassPlusStrokedAlist ,list? "An alist mapping figured
bass digits to glyphs.  The default is mapping numbers 2, 4, 5, 6, 7,
and@tie{}9 to the six glyphs @code{figbass.*plus} and
@code{figbass.*stroked}, respectively.")
     (finalFineTextVisibility ,boolean? "Whether @code{\\fine} at the
written end of the music should create a @emph{Fine} instruction.")
     (fineBarType ,string? "Bar line to insert at @code{\\fine}.
Where there is also a repeat bar line, the repeat bar line takes
precedence and this value is appended to it as an annotation.  The
default is @samp{|.}.")
     (fineSegnoBarType ,string? "Bar line to insert where an in-staff
segno coincides with @code{\\fine}.  The default is @samp{|.S}.")
     (fineStartRepeatSegnoBarType ,string? "Bar line to insert where
an in-staff segno coincides with @code{\\fine} and the start of a
@code{\\repeat volta}.  The default is @samp{|.S.|:}.")
     (fineText ,markup? "The text to print at @code{\\fine}.")
     (fingeringOrientations ,list? "A list of symbols, containing
@samp{left}, @samp{right}, @samp{up} and/@/or @samp{down}.  This list
determines where fingerings are put relative to the chord being
fingered.")
     (firstClef ,boolean? "If true, create a new clef when starting a
staff.")
     (followVoice ,boolean? "If set, note heads are tracked across
staff switches by a thin line.")
     (fontSize ,number? "The relative size of all grobs in a context.")
     (forbidBreak ,boolean? "If set to @code{#t}, prevent a line break
at this point, except if explicitly requested by the user.")
     (forbidBreakBetweenBarLines ,boolean? "If set to true,
@code{Bar_@/engraver} forbids line breaks where there is no bar line.")
     (forceClef ,boolean? "Show clef symbol, even if it has not
changed.  Only active for the first clef after the property is set, not
for the full staff.")
     (fretLabels ,list? "A list of strings or Scheme-formatted markups
containing, in the correct order, the labels to be used for lettered
frets in tablature.")


     (glissandoMap ,list? "A map in the form of
'((source1 . target1) (source2 . target2) (sourcen . targetn))
showing the glissandi to be drawn for note columns.
The value '() will default to
'((0 . 0) (1 . 1) (n . n)),
where n is the minimal number of note-heads in
the two note columns between which the glissandi occur.")
     (gridInterval ,ly:moment? "Interval for which to generate
@code{GridPoint}s.")


     (handleNegativeFrets ,symbol? "How the automatic fret calculator
should handle calculated negative frets.  Values include @code{'ignore},
to leave them out of the diagram completely, @code{'include}, to include
them as calculated, and @code{'recalculate}, to ignore the specified
string and find a string where they will fit with a positive fret number.")
     (harmonicAccidentals ,boolean? "If set, harmonic notes in chords
get accidentals.")
     (harmonicDots ,boolean? "If set, harmonic notes in dotted chords get
dots.")
     (highStringOne ,boolean? "Whether the first string is the string
with highest pitch on the instrument.  This used by the automatic
string selector for tablature notation.")


     (ignoreBarChecks ,boolean? "Ignore bar checks.")
     (ignoreBarNumberChecks ,boolean? "Ignore bar number checks.")
     (ignoreFiguredBassRest ,boolean? "Don't swallow rest events.")
     (ignoreMelismata ,boolean? "Ignore melismata for this
@rinternals{Lyrics} line.")
     (implicitBassFigures ,list? "A list of bass figures that are not
printed as numbers, but only as extender lines.")
     (initialTimeSignatureVisibility ,vector? "break visibility for
the initial time signature.")
     (includeGraceNotes ,boolean? "Do not ignore grace notes for
@rinternals{Lyrics}.")
     (instrumentCueName ,markup? "The name to print if another
instrument is to be taken.

This property is deprecated")
     (instrumentEqualizer ,procedure? "A function taking a
string (instrument name), and returning a @code{(@var{min} . @var{max})}
pair of numbers for the loudness range of the instrument.")
     (instrumentName ,markup? "The name to print left of a staff.  The
@code{instrumentName} property labels the staff in the first system, and
the @code{shortInstrumentName} property labels following lines.")
     (instrumentTransposition ,ly:pitch? "Define the transposition of
the instrument. Its value is the pitch that sounds when the instrument
plays written middle C.  This is used to transpose the MIDI output,
and @code{\\quote}s.")
     (internalBarNumber ,integer? "Contains the current barnumber.
This property is used for internal timekeeping, among others by the
@code{Accidental_engraver}.")


     (keepAliveInterfaces ,list? "A list of symbols, signifying grob
interfaces that are worth keeping a staff with @code{remove-empty} set
around for.")
     (keyAlterationOrder ,list? "A list of pairs that defines in what
order alterations should be printed.  The format of an entry is
@code{(@var{step} . @var{alter})},
where @var{step} is a number from 0 to@tie{}6 and
@var{alter} from -1 (double flat) to 1 (double sharp),
with exact rationals for alterations in between, e.g., 1/2
for sharp.")
     (keyAlterations ,list? "The current key signature.  This is an alist
containing @code{(@var{step} . @var{alter})} or
@code{((@var{octave} . @var{step}) . @var{alter})}, where @var{step}
is a number in the range 0 to@tie{}6 and @var{alter} a fraction,
denoting alteration.  For alterations, use symbols, e.g.,
@code{keyAlterations = #`((6 . ,FLAT))}.")


     (lyricMelismaAlignment ,number? "Alignment to use for a melisma syllable.")
     (lyricRepeatCountFormatter ,procedure? "A procedure taking as
arguments the context and the numeric repeat count.  It should return
the formatted repeat count as markup.  If it does not return markup,
no grob is created.")


     (magnifyStaffValue ,positive? "The most recent value set with
@code{\\magnifyStaff}.")
     (majorSevenSymbol ,markup? "How should the major 7th be formatted
in a chord name?")
     (maximumFretStretch ,number? "Don't allocate frets further than
this from specified frets.")
     (measureBarType ,string? "Bar line to insert at a measure boundary.")
     (measureLength ,ly:moment? "Length of one measure in the current
time signature.")
     (measurePosition ,ly:moment? "How much of the current measure have
we had.  This can be set manually to create incomplete measures.")
     (measureStartNow ,boolean? "True at the beginning of a measure.")
     (melismaBusyProperties ,list? "A list of properties (symbols) to
determine whether a melisma is playing.  Setting this property will
influence how lyrics are aligned to notes.  For example, if set to
@code{'(melismaBusy beamMelismaBusy)}, only manual melismata and
manual beams are considered.  Possible values include
@code{melismaBusy}, @code{slurMelismaBusy}, @code{tieMelismaBusy}, and
@code{beamMelismaBusy}.")
     (metronomeMarkFormatter ,procedure? "How to produce a metronome
markup.  Called with two arguments: a @code{TempoChangeEvent} and context.")
     (middleCClefPosition ,number? "The position of the middle C,
as determined only by the clef.  This can be calculated by looking at
@code{clefPosition} and @code{clefGlyph}.")
     (middleCCuePosition ,number? "The position of the middle C,
as determined only by the clef of the cue notes.  This can be calculated by
looking at @code{cueClefPosition} and @code{cueClefGlyph}.")
     (middleCOffset ,number? "The offset of
middle C from the position given by @code{middleCClefPosition} This
is used for ottava brackets.")
     (middleCPosition ,number? "The place of the middle C, measured in
half staff-spaces.  Usually determined by looking at
@code{middleCClefPosition} and @code{middleCOffset}.")
     (midiBalance ,number? "Stereo balance for the MIDI channel
associated with the current context.  Ranges from@tie{}@w{-1} to@tie{}1,
where the values@tie{}@w{-1} (@code{#LEFT}),@tie{}0 (@code{#CENTER})
and@tie{}1 (@code{#RIGHT}) correspond to leftmost emphasis, center
balance, and rightmost emphasis, respectively.")
     (midiChannelMapping ,symbol? "How to map MIDI channels: per
@code{staff} (default), @code{instrument} or @code{voice}.")
     (midiChorusLevel ,number? "Chorus effect level for the MIDI
channel associated with the current context.  Ranges from 0
to@tie{}1 (0=off,@tie{}1=full effect).")
     (midiExpression ,number? "Expression control for the MIDI
channel associated with the current context.  Ranges from 0
to@tie{}1 (0=off,@tie{}1=full effect).")
     (midiInstrument ,string? "Name of the MIDI instrument to use.")
     (midiMaximumVolume ,number? "Analogous to
@code{midiMinimumVolume}.")
     (midiMergeUnisons ,boolean? "If true, output only one MIDI note-on
event when notes with the same pitch, in the same MIDI-file track, overlap.")
     (midiMinimumVolume ,number? "Set the minimum loudness for MIDI.
Ranges from 0 to@tie{}1.")
     (midiPanPosition ,number? "Pan position for the MIDI channel
associated with the current context.  Ranges from@tie{}@w{-1} to@tie{}1,
where the values@tie{}@w{-1} (@code{#LEFT}),@tie{}0 (@code{#CENTER})
and@tie{}1 (@code{#RIGHT}) correspond to hard left, center, and hard
right, respectively.")
     (midiReverbLevel ,number? "Reverb effect level for the MIDI
channel associated with the current context.  Ranges from 0
to@tie{}1 (0=off,@tie{}1=full effect).")
     (minimumFret ,number? "The tablature auto string-selecting
mechanism selects the highest string with a fret at least
@code{minimumFret}.")
     (minimumPageTurnLength ,ly:moment? "Minimum length of a rest for a
page turn to be allowed.")
     (minimumRepeatLengthForPageTurn ,ly:moment? "Minimum length of a
repeated section for a page turn to be allowed within that section.")
     (minorChordModifier ,markup? "Markup displayed following the root
for a minor chord")


     (noChordSymbol ,markup? "Markup to be displayed for rests in a
ChordNames context.")
     (noteNameFunction ,procedure? "Function used to convert pitches
into strings and markups.")
     (noteNameSeparator ,string? "String used to separate simultaneous
NoteName objects.")
     (noteToFretFunction ,procedure? "Convert list of notes and list of
defined strings to full list of strings and fret numbers.
Parameters: The context, a list of note events, a list of
tabstring events, and the fretboard grob if a fretboard is desired.")
     (nullAccidentals ,boolean? "The @code{Accidental_engraver}
generates no accidentals for notes in contexts were this is set.
In addition to supressing the printed accidental, this option removes
any effect the note would have had on accidentals in other voices.")


     (ottavation ,markup? "If set, the text for an ottava spanner.
Changing this creates a new text spanner.")
     (ottavationMarkups ,list? "An alist defining the markups used
for ottava brackets.  It contains entries of the form
@code{(@var{number of octaves} . @var{markup})}.")
     (ottavaStartNow ,boolean? "Is an ottava starting in this
time step?")
     (output ,ly:music-output? "The output produced by a score-level
translator during music interpretation.")


     (partCombineForced ,symbol? "Override for the partCombine
decision.  Can be @code{apart}, @code{chords}, @code{unisono},
@code{solo1}, or @code{solo2}.")
     (partCombineTextsOnNote ,boolean? "Print part-combine texts only on
the next note rather than immediately on rests or skips.")
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
     (printAccidentalNames ,boolean-or-symbol? "Print accidentals in the
@code{NoteNames} context.")
     (printKeyCancellation ,boolean? "Print restoration alterations
before a key signature change.")
     (printNotesLanguage ,string? "Use a specific language in the
@code{NoteNames} context.")
     (printOctaveNames ,boolean-or-symbol? "Print octave marks in the
@code{NoteNames} context.")
     (printPartCombineTexts ,boolean? "Set @q{Solo} and @q{A due} texts
in the part combiner?")
     (proportionalNotationDuration ,ly:moment? "Global override for
shortest-playing duration.  This is used for switching on proportional
notation.")


     (rehearsalMark ,integer? "The next rehearsal mark to print.")
     (rehearsalMarkFormatter ,procedure? "A procedure taking as
arguments the context and the sequence number of the rehearsal mark.
It should return the formatted mark as a markup object.")
     (repeatCommands ,list? "A list of commands related to volta-style
repeats.  In general, each element is a list, @code{'(@var{command}
@var{args@dots{}})}, but a command with no arguments may be
abbreviated to a symbol; e.g., @code{'((start-@/repeat))} may be given
as @code{'(start-@/repeat)}.

@table @code
@item end-repeat
End a repeated section.

@item start-repeat
Start a repeated section.

@item volta @var{text}
If @var{text} is markup, start a volta bracket with that label; if
@var{text} is @code{#f}, end a volta bracket.

@end table")
     (repeatCountVisibility ,procedure? "A procedure taking as
arguments an integer and context, returning whether the corresponding
percent repeat number should be printed when @code{countPercentRepeats}
is set.")
     (restCompletionBusy ,boolean? "Signal whether a completion-rest is active.")
     (restNumberThreshold ,number? "If a multimeasure rest has more
measures than this, a number is printed.")
     (restrainOpenStrings ,boolean? "Exclude open strings from the
automatic fret calculator.")


     (searchForVoice ,boolean? "Signal whether a search should be made
of all contexts in the context hierarchy for a voice to provide rhythms
for the lyrics.")
     (sectionBarType ,string? "Bar line to insert at @code{\\section}.
Where there is also a repeat bar line, the repeat bar line takes
precedence and this value is appended to it as an annotation.  The
default is @samp{||}.")
     (segnoBarType ,string? "Bar line to insert at an in-staff segno.
The default is @samp{S}.")
     (segnoMarkFormatter ,procedure? "A procedure that creates a
segno (which conventionally indicates the start of a repeated
section), taking as arguments the mark sequence number and the
context.  It should return a markup object.")
     (segnoStyle ,symbol? "A symbol that indicates how to print a segno:
@code{bar-line} or @code{mark}.")
     (shapeNoteStyles ,vector? "Vector of symbols, listing style for
each note head relative to the tonic (q.v.@:) of the scale.")
     (shortInstrumentName ,markup? "See @code{instrumentName}.")
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
     (slashChordSeparator ,markup? "The markup object used to separate
a chord name from its root note in case of inversions or slash
chords.")
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
     (startAtNoteColumn ,boolean? "Start @code{DurationLine} grob at entire
@code{NoteColumn}.")
     (startAtSkip ,boolean? "Start @code{DurationLine} grob at
@code{skip-event}.")
     (startRepeatBarType ,string? "Bar line to insert at the start of
a @code{\\repeat volta}.  The default is @samp{.|:}.")
     (startRepeatSegnoBarType ,string? "Bar line to insert where an
in-staff segno coincides with the start of a @code{\\repeat volta}.
The default is @samp{S.|:}.")
     (stemLeftBeamCount ,integer? "Specify the number of beams to draw
on the left side of the next note.  Overrides automatic beaming.  The
value is only used once, and then it is erased.")
     (stemRightBeamCount ,integer? "See @code{stemLeftBeamCount}.")
     (strictBeatBeaming ,boolean? "Should partial beams reflect the
beat structure even if it causes flags to hang out?")
     (stringNumberOrientations ,list? "See
@code{fingeringOrientations}.")
     (stringOneTopmost ,boolean? "Whether the first string is
printed on the top line of the tablature.")
     (stringTunings ,list? "The tablature strings tuning.  It is a list
of the pitches of each string (starting with the lowest numbered
one).")
     (strokeFingerOrientations ,list? "See
@code{fingeringOrientations}.")
     (subdivideBeams ,boolean? "If set, multiple beams will be
subdivided at @code{baseMoment} positions by only drawing one beam over the beat.")
     (suggestAccidentals ,boolean-or-symbol? "If set to @code{#t},
accidentals are typeset as suggestions above the note.  Setting it to
@code{'cautionary} only applies that to cautionary accidentals.")
     (supportNonIntegerFret ,boolean? "If set in @code{Score} the
@code{TabStaff} will print micro-tones as @samp{2½}")
     (suspendMelodyDecisions ,boolean? "When using the @code{Melody_engraver},
stop changing orientation of stems based on the melody when this is set
to true.")
     (suspendRestMerging ,boolean? "When using the Merge_rest_engraver do not
merge rests when this is set to true.")
     (systemStartDelimiter ,symbol? "Which grob to make for the start
of the system/@/staff?  Set to @code{SystemStartBrace},
@code{SystemStartBracket} or @code{SystemStartBar}.")
     (systemStartDelimiterHierarchy ,pair? "A nested list, indicating
the nesting of a start delimiters.")


     (tablatureFormat ,procedure? "A function formatting a tablature
note head.  Called with three arguments: context, string number and,
fret number.  It returns the text as a markup.")
     (tabStaffLineLayoutFunction ,procedure? "A function determining the
staff position of a tablature note head.  Called with two arguments:
the context and the string.")
     (tempoHideNote ,boolean? "Hide the note = count in tempo marks.")
     (tempoWholesPerMinute ,ly:moment? "The tempo in whole notes per
minute.")
     (tieWaitForNote ,boolean? "If true, tied notes do not have to
follow each other directly.  This can be used for writing out
arpeggios.")
     (timeSignatureFraction ,fraction? "A pair of numbers,
signifying the time signature.  For example, @code{'(4 . 4)} is a
4/4 time signature.")
     (timeSignatureSettings ,cheap-list? "A nested alist of settings for
time signatures.  Contains elements for various time signatures.  The
element for each time signature contains entries for @code{baseMoment},
@code{beatStructure}, and @code{beamExceptions}.")
     (timing ,boolean? "Keep administration of measure length,
position, bar number, etc.?  Switch off for cadenzas.")
     (tonic ,ly:pitch? "The tonic of the current scale.")
     (topLevelAlignment ,boolean? "If true, the @var{Vertical_align_engraver}
will create a @var{VerticalAlignment}; otherwise, it will create a
@var{StaffGrouper}")
     (tupletFullLength ,boolean? "If set, the tuplet is printed up to
the start of the next note.")
     (tupletFullLengthNote ,boolean? "If set, end at the next note,
otherwise end on the matter (time signatures, etc.@:) before the note.")
     (tupletSpannerDuration ,ly:moment? "Normally, a tuplet bracket is
as wide as the @code{\\times} expression that gave rise to it.  By
setting this property, you can make brackets last shorter.

@example
@{
  \\set tupletSpannerDuration = #(ly:make-moment 1 4)
  \\times 2/3 @{ c8 c c c c c @}
@}
@end example")


     (underlyingRepeatBarType ,string? "Bar line to insert at points
of repetition or departure where no bar line would normally appear,
for example at the end of a system broken in mid measure where the
next system begins with a segno.  Where there is also a repeat bar
line, the repeat bar line takes precedence and this value is appended
to it as an annotation.  The default is @samp{||}.")
     (useBassFigureExtenders ,boolean? "Whether to use extender lines
for repeated bass figures.")


     (vocalName ,markup? "Name of a vocal line.")
     (voltaSpannerDuration ,ly:moment? "This specifies the maximum
duration to use for the brackets printed for @code{\\alternative}.
This can be used to shrink the length of brackets in the situation
where one alternative is very large.")


     (whichBar ,string? "The current bar line type, or @code{'()} if
there is no bar line.  Setting this explicitly in user code is
deprecated.  Use @code{\\bar} or related commands to set it."))))


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
     (breathMarkDefinitions ,list? "The description of breath marks.
This is used by the @code{Breathing_@/sign_@/engraver}.  See
@file{scm/@/breath.scm} for more information.")
     (busyGrobs ,list? "A queue of @code{(@var{end-moment} .
@var{grob})} cons cells.  This is for internal (C++) use only.  This
property contains the grobs which are still busy (e.g., note heads,
spanners, etc.).")


     (codaMarkCount ,index? "Updated at the end of each timestep in
which a coda mark appears: not set during the first timestep,
0@tie{}up to the first coda mark, 1@tie{}from the first to the second,
2@tie{}from the second to the third, etc.")
     (currentBarLine ,ly:grob? "Set to the @code{BarLine} that
@code{Bar_@/engraver} has created in the current timestep.")
     (currentChordCause ,ly:stream-event? "Event cause of the chord
that should be created in this time step (if any).")
     (currentChordText ,markup? "In contexts printing chord names,
this is at any point of time the markup that will be put in the
chord name.")
     (currentCommandColumn ,ly:grob? "Grob that is X-parent to all
current breakable items (clef, key signature, etc.).")
     (currentMusicalColumn ,ly:grob? "Grob that is X-parent to all
non-breakable items (note heads, lyrics, etc.).")
     (currentPerformanceMarkEvent ,ly:stream-event? "The coda,
section, or segno mark event selected by
@code{Mark_@/tracking_@/translator} for engraving by
@code{Mark_@/engraver}.")
     (currentRehearsalMarkEvent ,ly:stream-event? "The ad-hoc or
rehearsal mark event selected by @code{Mark_@/tracking_@/translator}
for engraving by @code{Mark_@/engraver}.")


     (dynamicAbsoluteVolumeFunction ,procedure? "A procedure that takes
one argument, the text value of a dynamic event, and returns the absolute
volume of that dynamic event.")


     (finalizations ,list? "A list of expressions to evaluate before
proceeding to next time step.  This is an internal variable.")
     (forceBreak ,boolean? "Set to @code{#t} when an event forcing a
line break was heard.")

     (graceSettings ,list? "Overrides for grace notes.  This property
should be manipulated through the @code{add-grace-property} function.")


     (hasAxisGroup ,boolean? "True if the current context is contained
in an axis group.")
     (hasStaffSpacing ,boolean? "True if @code{currentCommandColumn}
contains items that will affect spacing.")


     (lastChord ,markup? "Last chord, used for detecting chord changes.")
     (lastKeyAlterations ,list? "Last key signature before a key
signature change.")
     (localAlterations ,list? "The key signature at this point in the
measure.  The format is the same as for @code{keyAlterations}, but can
also contain @code{((@var{octave} . @var{name}) . (@var{alter}
@var{barnumber} . @var{measureposition}))} pairs.")


     (melismaBusy ,boolean? "Signifies whether a melisma is active.
This can be used to signal melismas on top of those automatically
detected.")
     (midiSkipOffset ,ly:moment? "This is the accrued MIDI offset to
account for time skipped via @code{skipTypesetting}.")


     (partialBusy ,boolean? "Signal that \\partial acts at the current timestep.")


     (quotedCueEventTypes ,list? "A list of symbols, representing the
event types that should be duplicated for @code{\\cueDuring} commands.")
     (quotedEventTypes ,list? "A list of symbols, representing the
event types that should be duplicated for @code{\\quoteDuring} commands.
This is also a fallback for @code{\\cueDuring} if @code{quotedCueEventTypes}
is not set")


     (rootSystem ,ly:grob? "The System object.")


     (scriptDefinitions ,list? "The description of scripts.  This is
used by the @code{Script_engraver} for typesetting note-superscripts
and subscripts.  See @file{scm/@/script.scm} for more information.")
     (segnoMarkCount ,index? "Updated at the end of each timestep in
which a segno appears: not set during the first timestep, 0@tie{}up to
the first segno, 1@tie{}from the first to the second segno,
2@tie{}from the second to the third segno, etc.")
     (slurMelismaBusy ,boolean? "Signal if a slur is present.")
     (stavesFound ,grob-list? "A list of all staff-symbols found.")
     (stringFretFingerList ,list? "A list containg three entries.
In @code{TabVoice} and @code{FretBoards} they determine the string, fret and
finger to use")


     (tieMelismaBusy ,boolean? "Signal whether a tie is present.")
     )))


(define-public all-translation-properties
  (append all-user-translation-properties
          all-internal-translation-properties))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public default-melisma-properties
  '(melismaBusy slurMelismaBusy tieMelismaBusy beamMelismaBusy completionBusy))
