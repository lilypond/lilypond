;;;; translator-property-description.scm -- part of backend documentation
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c)  1998--2004  Han-Wen Nienhuys <hanwen@cs.uu.nl>
;;;;                 Jan Nieuwenhuizen <janneke@gnu.org>


(define-public all-translation-properties '())

(define (translator-property-description symbol type? description)
 (if (not (equal? #f (object-property symbol 'translation-doc)))
      (begin
	(ly:warn (string-append "Redefining " (symbol->string symbol) "\n"))
	(exit 2)
      ))
  
  (set-object-property! symbol 'translation-type? type?)
  (set-object-property! symbol 'translation-doc description)
  (set! all-translation-properties (cons symbol all-translation-properties))
  symbol
  )

(define-public all-user-translation-properties
  (map
   (lambda (x)
     (apply translator-property-description x))
   `(
     (aDueText ,string? "text for begin of a due")
     (alignBassFigureAccidentals ,boolean?
				 "If true, then the accidentals are aligned in bass figure context.")

     (allowBeamBreak ,boolean? "If true allow line breaks during beams.")
     (associatedVoice ,string? "Name of the
Voice that has the melody for this Lyrics.")
     (autoBeamSettings ,list? "Specifies
when automatically generated beams should begin and end.
See the notation manual for more information. ")
     (autoAccidentals ,list? "List of
different ways to typeset an accidental. See the notation manual for more information on setting this.")
     (autoBeaming ,boolean? "If set to true
then beams are generated automatically.")

     (autoCautionaries ,list? "List similar to
autoAccidentals, but it controls cautionary accidentals rather than
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

     (barCheckSynchronize ,boolean? "If true then reset measurePosition
when finding a barcheck.")

     (barNumberVisibility ,procedure? "Procedure that takes an int and
returns whether the corresponding bar number should be printed")
     (bassFigureFormatFunction ,procedure? "DOCME")
     (beatLength ,ly:moment? "The length of one beat in this time signature.")
     (beatGrouping ,list?
		   "List of beatgroups. Eg. in 5/8 time #(list 2 3).")


     (breakAlignOrder ,list? "Defines the order in which
prefatory matter (clefs, key signatures) appears, eg. this puts the
key signatures after the bar lines:

@example
	\\property Score.breakAlignOrder = #'(
	  span-bar
	  breathing-sign
	  clef
	  staff-bar
	  key
	  time-signature
	)
@end example
")

     (centralCPosition ,number? "Place of the central C, measured in half
staffspaces.  Usually determined by looking at clefPosition and
clefGlyph.")

     (chordNameFunction ,procedure?
			"The function that converts lists of pitches to chord names.")
     (chordNoteNamer ,procedure?
		     "Function that converts from a pitch object to a text markup. Used for single pitches.")
     (chordRootNamer ,procedure?
		     "Function that converts from a pitch object to a text markup. Used for chords.")
     (chordNameExceptions ,list?
			  "Alist of chord exceptions. Contains (CHORD . MARKUP) entries.")
     (chordNameExceptionsFull ,list?
			      "Alist of chord exceptions. Contains (CHORD . (MARKUP)) entries.")
     (chordNameExceptionsPartial ,list?
				 "Alist of partial chord exceptions. Contains (CHORD . (PREFIX-MARKUP SUFFIX-MARKUP)) entries.")
     (chordNameSeparator ,markup?
			 "The markup object used to separate parts of a chord name.")

     (chordChanges ,boolean? "Only show changes in chords scheme?")
     (clefGlyph ,string? "Name of the symbol within the music font")
     (clefOctavation ,integer? "Add
this much extra octavation. Values of 7 and -7 are common.")

     (clefPosition ,number? "Where should the center of the clef symbol go?
The unit of this distance is the half staff space, and 0 represents the vertical center.")

     (connectArpeggios ,boolean? " If set, connect all arpeggios that are
found.  In this way, you can make arpeggios that cross staves. ")
     (createKeyOnClefChange ,boolean? "Print a key signature whenever the clef is changed.")
     (crescendoText ,markup? "Text to print at start of non-hairpin crecscendo, ie: @samp{cresc.}")
     (crescendoSpanner ,symbol? "Type of spanner to be used for crescendi.
One of: @samp{hairpin}, @samp{line}, @samp{dashed-line},
@samp{dotted-line}.  If unset, hairpin type is used.")
     (decrescendoText ,markup? "Text to print at start of non-hairpin decrecscendo, ie: @samp{dim.}")

     (drumPitchTable ,hash-table? "A table mapping percussion
instruments (symbols) to pitches.")

     (drumStyleTable ,hash-table? "A hash table containing mapping
drums to layout settings.  Predefined values: @code{drums-style},
@code{timbales-style}, @code{congas-style}, @code{bongos-style} and
@code{percussion-style}.

The layout style is a hash table, containing the drum-pitches (eg. the
symbol @code{hihat}) as key, and a list (@var{notehead-style}
@var{script} @var{vertical-position}) as values.
 ")
     (currentBarNumber ,integer? "Contains the current barnumber. This property is incremented at
every barline.
")
     (defaultBarType ,string? "Sets the default type of bar line.
Available bar types: [FIXME];

This variable is typically read by Timing_engraver at Score level, so
overriding Staff.defaultBarType will have no effect.
")

     (decrescendoSpanner ,symbol? "Type of spanner to be used for
decrescendi.  One of: @samp{hairpin}, @samp{line}, @samp{dashed-line},
@samp{dotted-line}.  If unset, hairpin type is used.")

     (explicitClefVisibility ,procedure? "visibility-lambda function for clef changes.")

     (explicitKeySignatureVisibility ,procedure? "visibility-lambda
function for explicit Key changes; \\override of #'break-visibility
will set the visibility for normal (ie. at the start of the line) key
signatures.")

     (extraNatural ,boolean? "Whether to typeset an
extra natural sign before accidentals changing from a non-natural to 
another non-natural.")

     (extraVerticalExtent ,number-pair?
			  "extra vertical extent, same format as MinimumVerticalExtent")


     (followVoice ,boolean? "if set, note heads are tracked across staff
switches by a thin line")

     (fontSize ,number?
	       "The relative size of all grobs in a context. This is
done using the @code{Font_size_engraver}.")

     (fingeringOrientations ,list?
			    "List of symbols, containing left, right, up and/or down. This list
determines where fingerings are put relative to the chord being
fingered.")

     (forceClef ,boolean? "Show clef symbol, even if it hasn't
changed. Only active for the first clef after the property is set, not
for the full staff.")

     (highStringOne ,boolean? "Whether the 1st string is the string with
highest pitch on the instrument. This used by the automatic string
selector for tab notation.")

     (ignoreMelismata ,boolean? "Ignore melismata for this @internalsref{Lyrics}.")
     (instr ,markup? "see @code{instrument}")

     (instrument ,markup? "The name to print left of a staff.  The
@code{instrument} property labels the staff in the first system, and
the @code{instr} property labels following lines.")
     (instrumentEqualizer ,procedure? "[DOCUMENT-ME]")

     (instrumentSupport ,list? "list of grobs to attach instrument name
to.")
     (keyAccidentalOrder ,list? "
Alist that defines in what order  alterations should be printed.
The format is (NAME . ALTER), where NAME is from 0 .. 6 and ALTER from  -1, 1.
")

     (keySignature ,list? "The current key signature. This is an alist
containing (NAME . ALTER) or ((OCTAVE . NAME) . ALTER).
 where NAME is from 0.. 6 and
ALTER from -4 (double flat) to 4 (double sharp).
")

     (majorSevenSymbol ,markup? "How should
the major7 be formatted in a chord name?")
     (markFormatter ,procedure? "Procedure
taking as arguments context and rehearsal mark. It should return the
formatted mark as a markup object.")

     (measureLength ,ly:moment? "Length of one
measure in the current time signature last?")

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
     (midiMinimumVolume ,number? "[DOCUMENT-ME]")
     (midiMaximumVolume ,number? "[DOCUMENT-ME]")
     (minimumFret ,number? "The tablature auto string-selecting mechanism
selects the highest string with a fret not less than minimumFret")
     (minimumVerticalExtent ,number-pair? "minimum vertical extent, same
format as VerticalExtent")
     (recordEventSequence ,procedure? "Upon termination of this context,
this function is called with current context and a list of music
objects.  The list of contains entries with start times, music objects
and whether they're processed in this context.")
     (ottavation ,string? "If set, the text for an 8va spanner. Changing
this implies a new text spanner. ")
     (pedalSustainStrings ,list? "List of string to print for
sustain-pedal. Format is (UP UPDOWN DOWN), where each of the three is
the string to print when this is done with the pedal.")
     (pedalUnaCordaStrings ,list? "see pedalSustainStrings.")
     (pedalSostenutoStrings ,list? "see pedalSustainStrings.")
     (pedalSustainStyle ,symbol? "A symbol that indicates how to print
sustain pedals: text, bracket or mixed (both).")
     (pedalUnaCordaStyle ,symbol? "see pedalSustainStyle.")
     (pedalSostenutoStyle ,symbol? "see pedalSustainStyle.")
     (rehearsalMark ,integer? "The last rehearsal mark printed.")
     (repeatCommands ,list? "This property is read to find any command of the form (volta . X), where X is a string or #f")

     (restNumberThreshold ,number?
			  "If a multimeasure rest takes less
than this number of measures, no number is printed. ")

     (skipBars ,boolean? "If set to true, then
skip the empty bars that are produced by multimeasure notes and rests.
These bars will not appear on the printed output.  If not set (the
default) multimeasure notes and rests expand into their full length,
printing the appropriate number of empty bars so that synchronization
with other voices is preserved.


@example
@@lilypond[fragment,verbatim,center]
r1 r1*3 R1*3  \\\\property Score.skipBars= ##t r1*3 R1*3
@@end lilypond
@end example
")
     (skipTypesetting ,boolean?
		      "When true, all no typesetting is done at
this moment, causing  the interpretation phase to go a lot faster. This can
help with debugging large scores.")
     (soloADue ,boolean? "set Solo/A due texts in the part combiner?")
     (soloIIText ,string? "text for begin of solo for voice ``two'' when part-combining.")
     (soloText ,string? "text for begin of solo when part-combining.")
     (sparseTies ,boolean? "only create one tie per chord.")
     (squashedPosition ,integer? " Vertical position of
squashing for Pitch_squash_engraver.")

     (stringOneTopmost ,boolean? "Whether the 1st string is printed on the
top line of the tablature.")

     (stanza ,markup? "Stanza `number' to print before the start of a
verse. Use in Lyrics context.")

     (stemLeftBeamCount ,integer? " Specify the number of beams to draw on
the left side of the next note.  Overrides automatic beaming.  The
value is only used once, and then it is erased. [JUNKME.]")

     (stemRightBeamCount ,integer? "idem, for the right side. [JUNKME]")

     (stringTunings ,list? "The tablature strings tuning. Must be a list of
the different semitons pitch of each string (starting by the lower
one).")

     (subdivideBeams ,boolean? "If set, multiple beams will be subdivided
at beat positions - by only drawing one beam over the beat.")

     (systemStartDelimiter ,symbol? "Which grob to make for the start of
the system/staff? Set to @code{SystemStartBrace},
@code{SystemStartBracket} or @code{SystemStartBar}.")

     (tablatureFormat ,procedure? "Function formatting a tab notehead; it
takes a string number, a list of string tunings and Pitch object. It
returns the text as a string.")

     (timeSignatureFraction ,number-pair?
			    "pair of numbers, signifying the time signature. For example #'(4 . 4)
is a 4/4 time signature.")

     (timing ,boolean? " Keep administration of measure length, position, bar number, etc?
Switch off for cadenzas.")
     (tonic ,ly:pitch?
	    "The tonic of the current scale")
     (transposing ,integer? "Transpose the MIDI output.  Set this property to the number of half-steps to transpose by.")

     (tremoloFlags ,integer? "Number of tremolo flags to add if none is specified.")

     (tupletNumberFormatFunction
      ,procedure?
      "Function taking a music as input, producing a string. This function
is called to determine the text to print on a tuplet bracket.")

     (tupletSpannerDuration ,ly:moment? "
Normally a tuplet bracket is as wide as the
@code{\\times} expression that gave rise to it. By setting this
property, you can make brackets last shorter. Example

@example
@@lilypond[verbatim,fragment]
context Voice \\times 2/3 @{
  property Voice.tupletSpannerDuration = #(ly:make-moment 1 4)
  c-[8 c c-] c-[ c c-]
@}
@@end lilypond
@end example
.")
     (verticalAlignmentChildCallback ,procedure? "what callback to add to children of a vertical alignment.
It determines what alignment procedure is used on the alignment
itself.  .")
     (verticalExtent ,number-pair? "hard coded vertical extent.  The format
is a pair of dimensions, for example, this sets the sizes of a staff
to 10 (5+5) staffspaces high.

@example
property Staff.verticalExtent = #'(-5.0 . 5.0)
@end example


This does not work for Voice or any other context  that doesn't form a
vertical group.")

     (vocalName ,markup? "Name of a vocal line.")
     (vocNam ,markup? "Name of a vocal line, short version.")

     (voltaOnThisStaff ,boolean?
		       "Normally, volta brackets are put only on the topmost staff. Setting
this variable to true, will force a bracket to be on this staff as
well.")

     (voltaSpannerDuration ,ly:moment? "This specifies the maximum duration
to use for the brackets printed for @code{\\alternative}.  This can be
used to shrink the length of brackets in the situation where one
alternative is very large.")

     (whichBar
      ,string?
      "This property is read to determine what type of barline to create.

Example:
@example
\\property Staff.whichBar = \"|:\"
@end example

This will create a start-repeat bar in this staff only.
Valid values are described in @internalsref{bar-line-interface}.
")
     )))

(define-public all-internal-translation-properties
  (map
   (lambda (x)
     (set-object-property! (car x) 'internal-translation #t)
     (apply translator-property-description x)

     )

   `(

     (slurMelismaBusy ,boolean? "Signal if a slur is present.")
     (originalCentralCPosition
      ,integer?
      "Used for temporary overriding central C in octavation brackets. ")
     (melismaBusy ,boolean? "Signifies
whether a melisma is active. This can be used to signal melismas on
top of those automatically detected. ")
     (graceSettings ,vector?
		    "Overrides for grace notes. This
property should be manipulated through the @code{add-grace-property}
function.")
     (currentCommandColumn ,ly:grob? "Grob that is X-parent to all current breakable (clef, key signature, etc.) items.")
     (currentMusicalColumn ,ly:grob? "Grob that is X-parent to all non-breakable items (note heads, lyrics, etc.).")
     (breakableSeparationItem ,ly:grob?
			      "The breakable items in this time step, for this staff.")

     (localKeySignature ,list? "the key signature at this point in the
measure.  The format is the same as for keySignature, but can also contain
((OCTAVE . NAME) . (ALTER . BARNUMBER)) pairs. It is reset at every
bar line.
"  )

     
     (localKeySignatureChanges ,list? "Experimental.
 [DOCME]")

     (finalizations ,list? "List of expressions to evaluate before proceeding to next time step. Internal variable.")
     (busyGrobs ,list? "
a queue of (END-MOMENT . GROB) conses. This is for internal (C++) use only.
This property contains the grobs which are still busy (eg. noteheads, spanners, etc.)
")
     (barCheckLastFail ,ly:moment? "Where in  the measurze did the last barcheck fail?") 
     (associatedVoiceContext ,ly:context? "The context object of the Voice that has the melody for this Lyrics.")
     (acceptHashTable ,vector? "Internal
variable: store table with MusicName to Engraver entries.")
     (acknowledgeHashTable ,vector?
			   "Internal variable: store interface to engraver smob table for current
context.")

     (beamMelismaBusy ,boolean? "Signal if a beam is present.")
(dynamicAbsoluteVolumeFunction ,procedure? "[DOCUMENT-ME]
")

(lastKeySignature ,list? "Last key
signature before a key signature change.")

(scriptDefinitions ,list? "
Description of scripts. This is used by Script_engraver for typesetting note-super/subscripts. See @file{scm/script.scm} for more information
")
(stavesFound ,grob-list? "list of all staff-symbols found.")
(tieMelismaBusy ,boolean? "Signal whether a tie is present.")
     )
   ))

(define-public all-translation-properties
  (append all-user-translation-properties
	  all-internal-translation-properties))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public default-melisma-properties '(melismaBusy slurMelismaBusy tieMelismaBusy beamMelismaBusy))
