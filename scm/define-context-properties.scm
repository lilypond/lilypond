;;;; define-context-properties.scm -- part of backend documentation
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 1998--2006  Han-Wen Nienhuys <hanwen@xs4all.nl>
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
   
     (aDueText ,string? "Text to print at a unisono passage.")
     (alignBelowContext ,string? "Where to insert newly created context in vertiical alignment.")
     (alignAboveContext ,string? "Where to insert newly created context in vertiical alignment.")
     (alignBassFigureAccidentals ,boolean?
				 "If true, then the accidentals are aligned in bass figure context.")

     (associatedVoice ,string? "Name of the
@code{Voice} that has the melody for this @code{Lyrics} line.")
     (autoBeamSettings ,list? "Specifies
when automatically generated beams should begin and end.
See @usermanref{Setting automatic beam behavior} for more information.
")
     (autoAccidentals ,list? "List of
different ways to typeset an accidental.

For determining when to print an accidental, several different rules
are tried.  The rule that gives the highest number of accidentals is
used.  Each rule consists of

@table @var

@item context:
      In which context is the rule applied. For example, if
@var{context} is @internalsref{Score} then all staves share
accidentals, and if @var{context} is @internalsref{Staff} then all
voices in the same staff share accidentals, but staves do not.

@item octavation:
      Whether the accidental changes all octaves or only the current
      octave. Valid choices are 

     @table @samp
      @item same-octave:
      This is the default algorithm. Accidentals are typeset if the note changes
      the accidental of that note in that octave. Accidentals lasts to the end of the measure 
      and then as many measures as specified in the value. I.e. 1 means to the end
      of next measure, -1 means to the end of previous measure (that is: no duration at all), etc. #t means forever.
      @item any-octave:
      Accidentals are typeset if the note is different from 
      the previous note on the same pitch in any octave. The value has same meaning as in
      same-octave.
      @end table

@item laziness

Over how many bar lines the accidental lasts.
If @var{laziness} is @code{-1} then the accidental is forgotten
immediately, and if @var{laziness} is @code{#t} then the accidental
lasts forever.
@end table
")
     (autoBeamCheck ,procedure? "Procedure taking three
arguments, CONTEXT, DIR start/stop (-1 or 1) and TEST shortest
note in the beam.  A non-#f return value starts or stops the auto beam.")
     (autoBeaming ,boolean? "If set to true then beams are generated
automatically.")

     (autoCautionaries ,list? "List similar to
@code{autoAccidentals}, but it controls cautionary accidentals rather than
normal ones. Both lists are tried, and the one giving the most accidentals
wins. In case of draw, a normal accidental is typeset.
")
     (automaticBars ,boolean? " If set to true then bar lines will not be
printed automatically; they must be explicitly created with
@code{\\bar} command.  Unlike the @code{\\cadenza} keyword, measures
are still counted.  Bar generation will resume according to that count
if this property is unset.
")
     
     (barAlways ,boolean? "If set to true a bar line is drawn after each
note.")

     (barCheckSynchronize ,boolean? "If true then reset @code{measurePosition}
when finding a barcheck.")

     (barNumberVisibility ,procedure? "Procedure that takes an int and
returns whether the corresponding bar number should be printed")
     (bassStaffProperties ,list? "Alist of property settings to apply
for the down staff of PianoStaff. Used by @code{\\autochange}")
     (trebleStaffProperties ,list? "Alist of property settings to apply
for the up staff of PianoStaff. Used by @code{\\autochange}")

     (figuredBassFormatter ,procedure? "Routine generating a markup
for a bass figure.")
     (bassFigureFormatFunction ,procedure? "Procedure that is called
to produce the formatting for a @code{BassFigure} grob. It takes a
list of @code{BassFigureEvent}s, a context, and the grob to format.")
     
     (beatLength ,ly:moment? "The length of one beat in this time signature.")
     (beatGrouping ,list?
		   "List of beatgroups, e.g., in 5/8 time @code{'(2
3)}.")



     (middleCPosition ,number? "Place of the middle C, measured in half
staff-spaces.  Usually determined by looking at @code{clefPosition} and
@code{clefGlyph}.")

     (chordNameFunction ,procedure?
			"The function that converts lists of pitches to chord names.")
     (chordNoteNamer ,procedure?
		     "Function that converts from a pitch object to a text markup. Used for single pitches.")
     (chordRootNamer ,procedure?
		     "Function that converts from a pitch object to a text markup. Used for chords.")
     (chordNameExceptions ,list?
			  "An alist of chord exceptions.
Contains (@var{chord} . @var{markup}) entries.")
     (chordNameExceptionsFull ,list?
			      "An alist of chord exceptions.
Contains (@var{chord} . @var{markup}) entries.")
     (chordNameExceptionsPartial
      ,list?
      "An alist of partial chord exceptions. Contains (@var{chord} . (@var{prefix-markup} @var{suffix-markup})) entries.")
     
     (chordNameSeparator ,markup?
			 "The markup object used to separate
 parts of a chord name.")
     (chordPrefixSpacer ,number?
			"The space added between the root symbol and the prefix
 of a chord name")
     (chordChanges ,boolean? "Only show changes in chords scheme?")
     (clefGlyph ,string? "Name of the symbol within the music font.")
     (clefOctavation ,integer? "Add
this much extra octavation. Values of 7 and -7 are common.")

     (clefPosition ,number? "Where should the center of the clef
symbol go, measured in half staff spaces from the center of the staff.")

     (connectArpeggios ,boolean? " If set, connect arpeggios across
piano staff.")
     (countPercentRepeats ,boolean? "If set, produce counters for
percent repeats. ")
     (createKeyOnClefChange ,boolean? "Print a key signature whenever the clef is changed.")
     (createSpacing ,boolean? "Create @code{StaffSpacing} objects?
Should be set for staves.")
     (crescendoText ,markup? "Text to print at start of non-hairpin crescendo, i.e.: @samp{cresc.}")
     (crescendoSpanner ,symbol? "Type of spanner to be used for crescendi.
One of: @samp{hairpin}, @samp{line}, @samp{dashed-line},
@samp{dotted-line}.  If unset, hairpin type is used.")
     (decrescendoText ,markup? "Text to print at start of non-hairpin decrescendo, i.e.: @samp{dim.}")

     (drumPitchTable ,hash-table? "A table mapping percussion
instruments (symbols) to pitches.")

     (drumStyleTable ,hash-table? "A hash table containing mapping
drums to layout settings.  Predefined values: @samp{drums-style},
@samp{timbales-style}, @samp{congas-style}, @samp{bongos-style} and
@samp{percussion-style}.

The layout style is a hash table, containing the drum-pitches (e.g. the
symbol @samp{hihat}) as key, and a list (@var{notehead-style}
@var{script} @var{vertical-position}) as values.
 ")
     (currentBarNumber ,integer? "Contains the current barnumber. This property is incremented at every bar line. ")
     (defaultBarType ,string? "Sets the default type of bar line.
See @code{whichBar} for information on available bar types.

This variable is  read by @internalsref{Timing_translator} at
@internalsref{Score} level.
")

     (decrescendoSpanner ,symbol? "See @code{crescendoSpanner}.")
     (doubleSlurs ,boolean?
		  "When set, two slurs are created for every slurred
note, one above and one below the chord.")
     (explicitClefVisibility ,vector? "@samp{break-visibility} function for clef changes.")

     (explicitKeySignatureVisibility ,vector?
"@samp{break-visibility} function for explicit key
changes. @samp{\\override} of the @code{break-visibility} property will set the
visibility for normal (i.e. at the start of the line) key signatures.")

     (extendersOverRests ,boolean? "Whether to continue extenders as
they cross a rest.")
     (extraNatural ,boolean? "Whether to typeset an
extra natural sign before accidentals changing from a non-natural to 
another non-natural.")

     (figuredBassCenterContinuations ,boolean? "Whether to vertically center pairs of extender lines.  This does not work with three or more lines")
     (figuredBassPlusDirection ,ly:dir? "Where to put plus signs relative to the the main figure.")
     
     (figuredBassAlterationDirection ,ly:dir? "Where to put
alterations relative to the main figure.")
     (followVoice ,boolean? "If set, note heads are tracked across staff
switches by a thin line")

     (fontSize ,number?
	       "The relative size of all grobs in a context. ")

     (forbidBreak ,boolean? "If set to ##t, prevent a line break at this point.")

     (fingeringOrientations ,list?
			    "List of symbols, containing
@samp{left}, @samp{right}, @samp{up} and/or @samp{down}. This list
determines where fingerings are put relative to the chord being
fingered.")

     (firstClef ,boolean? "If true, create a new clef when starting a
staff.")
     (forceClef ,boolean? "Show clef symbol, even if it has not
changed. Only active for the first clef after the property is set, not
for the full staff.")
     (gridInterval ,ly:moment?
		   "Interval for which to generate @ref{GridPoint}s")

     (hairpinToBarline ,boolean? "If set, end a hairpin at the barline before the ending note.")
     
     (harmonicAccidentals ,boolean? "If set, harmonic notes in chords
get accidentals.")
     (highStringOne ,boolean? "Whether the 1st string is the string with
highest pitch on the instrument. This used by the automatic string
selector for tab notation.")

     (ignoreFiguredBassRest ,boolean? "Don't swallow rest events.")
     (ignoreBarChecks ,boolean? "Ignore bar checks")
     (ignoreMelismata ,boolean? "Ignore melismata for this @internalsref{Lyrics} line.")

     (implicitTimeSignatureVisibility ,vector? "break visibility for the default timesignature.")

     (implicitBassFigures ,list? "List of bass figures that are not
printed as numbers, but only as extender lines.")
     

     (instrumentCueName ,markup? "Name to print if another instrument is to be taken.")
     (instrumentName ,markup? "The name to print left of a staff.  The
@code{instrument} property labels the staff in the first system, and
the @code{instr} property labels following lines.")
     (instrumentEqualizer ,procedure? "
Function taking a string (instrument name), and returning a (@var{min} . @var{max}) pair of numbers for the loudness range of the instrument.
")

     ;; the definition is reversed wrt  traditional transposition
     ;; this because \transpose { \transposition .. } won't work
     ;; otherwise.
     (instrumentTransposition ,ly:pitch? "Defines the transposition of
the instrument. Its value is the pitch that sounds like middle C. This
is used to transpose the MIDI output, and @code{\\quote}s.")

     (internalBarNumber ,integer? "Contains the current barnumber. This property is used for internal timekeeping, among others by the @code{Accidental_engraver}.")
     
     (keepAliveInterfaces ,list? "List of symbols, signifying grob interfaces that
are worth keeping an staff with @code{remove-empty} set around for.")   
     (keyAlterationOrder ,list? " Alist that defines in what order
alterations should be printed.  The format is (@var{step}
. @var{alter}), where @var{step} is from 0 .. 6 and @var{alter} from
-2 (sharp) and 2 (flat).
")

     (keySignature ,list? "The current key signature. This is an alist
containing (@var{step} . @var{alter}) or ((@var{octave} . @var{step})
. @var{alter}).  where @var{step} is from 0.. 6 and @var{alter} a fraction, denoting
alteration.  For alterations, use symbols, eg.
@code{keySignature = #`((6 . ,FLAT))}
")
     (majorSevenSymbol ,markup? "How should
the major 7th be formatted in a chord name?")
     (markFormatter ,procedure? "Procedure
taking as arguments context and rehearsal mark. It should return the
formatted mark as a markup object.")

     (measureLength ,ly:moment? "Length of one
measure in the current time signature.")

     (measurePosition ,ly:moment? "How much of the current measure
have we had.  This can be set manually to create incomplete
measures.")

     (melismaBusyProperties ,list? "List of properties (symbols) to
determine whether a melisma is playing.  Setting this property will
influence how lyrics are aligned to notes.  For example, if set to
@code{#'(melismaBusy beamMelismaBusy)}, only manual melismata and
manual beams are considered. Possible values include
@code{melismaBusy}, @code{slurMelismaBusy}, @code{tieMelismaBusy}, and
@code{beamMelismaBusy}")


     (metronomeMarkFormatter ,procedure? "How to produce a metronome
markup.  Called with 2 arguments, event and context.")
     (midiInstrument ,string? "Name of the MIDI instrument to use ")
     (midiMinimumVolume ,number? "Sets the minimum loudness for MIDI. Ranges from 0 to 1.")
     (midiMaximumVolume ,number? "Analogous to @code{midiMinimumVolume}.")
     (minimumFret ,number? "The tablature auto string-selecting mechanism
selects the highest string with a fret at least @code{minimumFret}")
     (maximumFretStretch ,number? "Don't allocate frets further than this from specified frets.")
     (minimumPageTurnLength ,ly:moment? "Minimum length of a rest for a page turn to be allowed")
     (minimumRepeatLengthForPageTurn ,ly:moment? "Minimum length of a repeated section for a page
turn to be allowed within that section")
     (output ,ly:music-output? "The output produced by a score-level translator during music interpretation")
     (ottavation ,string? "If set, the text for an ottava spanner. Changing
this creates a new text spanner. ")
     (noteToFretFunction ,procedure? "How to produce a fret diagram.  Parameters: list of note events and list of tabstring events.")
     (pedalSustainStrings ,list? "List of string to print for
sustain-pedal. Format is (@var{up} @var{updown} @var{down}), where
each of the three is the string to print when this is done with the
pedal.")
     (pedalUnaCordaStrings ,list? "See @code{pedalSustainStrings}.")
     (pedalSostenutoStrings ,list? "See @code{pedalSustainStrings}.")
     (pedalSustainStyle ,symbol? "A symbol that indicates how to print
sustain pedals: @code{text}, @code{bracket} or @code{mixed} (both).")
     (pedalUnaCordaStyle ,symbol? "see @code{pedalSustainStyle}.")
     (pedalSostenutoStyle ,symbol? "see @code{pedalSustainStyle}.")
     (printKeyCancellation ,boolean? "Print restoration alterations before a key signature change. ")
     (printPartCombineTexts ,boolean? "set Solo/A due texts in the part combiner?")
     (printOctaveNames ,boolean? "Print octave marks for the NoteNames context.")

     (proportionalNotationDuration ,ly:moment? "Global override for
shortest-playing duration. This is used for switching on proportional
notation.")
     (recordEventSequence ,procedure? "When Recording_group_engraver
is in this context, then upon termination of the context, this
function is called with current context and a list of music objects.
The list of contains entries with start times, music objects and
whether they are processed in this context.")

     (rehearsalMark ,integer? "The last rehearsal mark printed.")
     (repeatCommands ,list? "This property is read to find any command of the form @code{(volta . @var{x})}, where @var{x} is a string or @code{#f}")
     (restNumberThreshold ,number?
			  "If a multimeasure rest has more measures
than this, a number is printed. ")
     (shapeNoteStyles ,vector? "Vector of symbols, listing style for each note
head relative to the tonic (qv.) of the scale.")
     (shortInstrumentName ,markup? "See @code{instrument}")
     (shortVocalName ,markup? "Name of a vocal line, short version.")
     (skipBars ,boolean? "If set to true, then
skip the empty bars that are produced by multimeasure notes and rests.
These bars will not appear on the printed output.  If not set (the
default) multimeasure notes and rests expand into their full length,
printing the appropriate number of empty bars so that synchronization
with other voices is preserved.


@example
@{
r1 r1*3 R1*3
\\set Score.skipBars= ##t
r1*3 R1*3
@}
@end example
")
     (skipTypesetting ,boolean?
		      "When true, all no typesetting is done, speeding
up the interpretation phase. This speeds up debugging large scores.")
     (soloIIText ,string? "text for begin of solo for voice ``two'' when part-combining.")
     (soloText ,string? "text for begin of solo when part-combining.")
     (squashedPosition ,integer? " Vertical position of
squashing for @internalsref{Pitch_squash_engraver}.")

     (staffLineLayoutFunction ,procedure? "Layout of staff lines, 'traditional, or 'semitone.")
     (stringNumberOrientations ,list? "See @code{fingeringOrientations}")
     (strokeFingerOrientations ,list? "See @code{fingeringOrientations}")
     (stringOneTopmost ,boolean? "Whether the 1st string is printed on the
top line of the tablature.")

     (stanza ,markup? "Stanza `number' to print before the start of a
verse. Use in Lyrics context.")

     (stemLeftBeamCount ,integer? " Specify the number of beams to draw on
the left side of the next note.  Overrides automatic beaming.  The
value is only used once, and then it is erased.")

     (stemRightBeamCount ,integer? "See @code{stemLeftBeamCount}.")

     (stringTunings ,list? "The tablature strings tuning. It is a list
of the pitch (in semitones) of each string (starting with the lower
one).")

     (subdivideBeams ,boolean? "If set, multiple beams will be subdivided
at beat positions by only drawing one beam over the beat.")
     (suggestAccidentals ,boolean? "If set, accidentals are typeset as cautionary suggestions over the note.")

     (systemStartDelimiterHierarchy ,pair? "A nested list, indicating the nesting of a start delimiters.") 

     (systemStartDelimiter ,symbol? "Which grob to make for the start of
the system/staff? Set to @code{SystemStartBrace},
@code{SystemStartBracket} or @code{SystemStartBar}.")

     (tablatureFormat ,procedure? "Function formatting a tab note head; it
takes a string number, a list of string tunings and Pitch object. It
returns the text as a string.")

     (tieWaitForNote ,boolean? "If true, tied notes do not have to follow each other directly.
This can be used for writing out arpeggios")
     (timeSignatureFraction ,number-pair?
			    "pair of numbers, signifying the time
signature. For example @code{#'(4 . 4)} is a 4/4 time signature.")

     (timing ,boolean? " Keep administration of measure length, position, bar number, etc?
Switch off for cadenzas.")
     (tonic ,ly:pitch?
	    "The tonic of the current scale")

     (tremoloFlags ,integer? "Number of tremolo flags to add if no
number is specified.")

     (tupletFullLength ,boolean? "If set, the tuplet is printed up to
the start of the next note.")
     (tupletFullLengthNote ,boolean? "If set, end at the next note, otherwise end on the matter (time sigs, etc.) before the note.")
     (tupletSpannerDuration ,ly:moment? "
Normally a tuplet bracket is as wide as the
@code{\\times} expression that gave rise to it.  By setting this
property, you can make brackets last shorter.  Example

@example
@{
\\set tupletSpannerDuration = #(ly:make-moment 1 4)
\\times 2/3 @{ c8 c c c c c @}
@}
@end example
.")

     (useBassFigureExtenders ,boolean? "Whether to use extender lines
for repeated bass figures")

     (verticallySpacedContexts ,list? "List of symbols, containing
context names whose vertical axis groups should be taken into account for
vertical spacing of systems.")
     
     (vocalName ,markup? "Name of a vocal line.")

     (voltaOnThisStaff ,boolean?
		       "Normally, volta brackets are put only on the
topmost staff.  This variable overrides this behavior, when set to
@code{#t} or @code{#f}.")

     (voltaSpannerDuration ,ly:moment? "This specifies the maximum duration
to use for the brackets printed for @code{\\alternative}.  This can be
used to shrink the length of brackets in the situation where one
alternative is very large.")

     (whichBar
      ,string?
      "This property is read to determine what type of bar line to create.

Example:
@example
\\set Staff.whichBar = \"|:\"
@end example

This will create a start-repeat bar in this staff only.
Valid values are described in @internalsref{bar-line-interface}.
")
     (tempoWholesPerMinute ,ly:moment? "The tempo in whole notes per minute.")
     (tempoUnitDuration ,ly:duration? "Unit for specifying tempo.")
     (tempoUnitCount ,number? "Count for specifying tempo.")
     
     )))

(define-public all-internal-translation-properties
  (map
   (lambda (x)
     (set-object-property! (car x) 'internal-translation #t)
     (apply translator-property-description x)

     )

   `((slurMelismaBusy ,boolean? "Signal if a slur is present.")
     (originalMiddleCPosition
      ,integer?
      "Used for temporary overriding middle C in octavation brackets. ")
     (melismaBusy ,boolean? "Signifies
whether a melisma is active. This can be used to signal melismas on
top of those automatically detected. ")
     (graceSettings ,list?
		    "Overrides for grace notes. This property should
be manipulated through the @code{add-grace-property} function.")
     (currentCommandColumn ,ly:grob? "Grob that is X-parent to all
current breakable (clef, key signature, etc.) items.")
     (currentMusicalColumn ,ly:grob? "Grob that is X-parent to all
non-breakable items (note heads, lyrics, etc.).")
     (breakableSeparationItem ,ly:grob?
			      "The breakable items in this time step,
for this staff.")

     (localKeySignature ,list? "the key signature at this point in the
measure.  The format is the same as for keySignature, but can also
contain ((@var{octave} . @var{name}) . (@var{alter} . @var{barnumber}))
pairs. It is reset at every bar line.")

     (finalizations ,list? "List of expressions to evaluate before proceeding to next time step. Internal variable.")
     (busyGrobs ,list? "a queue of (@var{end-moment} . @var{GROB})
conses. This is for internal (C++) use only.  This property contains
the grobs which are still busy (e.g. note heads, spanners, etc.)
")
     (barCheckLastFail ,ly:moment? "Where in the measure did the last barcheck fail?") 
     (associatedVoiceContext ,ly:context? "The context object of the Voice that has the melody for this Lyrics.")
     (beamMelismaBusy ,boolean? "Signal if a beam is present.")
     (dynamicAbsoluteVolumeFunction ,procedure? "[DOCUMENT-ME]")

     (lastKeySignature ,list? "Last key signature before a key
signature change.")
     (rootSystem ,ly:grob? "The System object")
     (scriptDefinitions ,list? "Description of scripts. This is used by
Script_engraver for typesetting note-super/subscripts. See
@file{scm/script.scm} for more information
")
     (quotedEventTypes ,list? "List of symbols, representing the
event types that should be duplicated for @code{\\quote} commands.")

;     (quotes ,hash-table? "Hash table, mapping names to music-event vectors.")
     (stavesFound ,grob-list? "list of all staff-symbols found.")
     (instrumentSupport ,grob-list? "list of grobs to attach instrument name
to.")
     (tieMelismaBusy ,boolean? "Signal whether a tie is present.")
     (hasStaffSpacing ,boolean? "True if the currentCommandColumn contains items that will
affect spacing")
     )))

(define-public all-translation-properties
  (append all-user-translation-properties
	  all-internal-translation-properties))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public default-melisma-properties '(melismaBusy slurMelismaBusy tieMelismaBusy beamMelismaBusy))
