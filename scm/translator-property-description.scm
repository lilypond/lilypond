
(define-public all-translation-properties '())

(define (translator-property-description symbol type? description)
 (if (not (equal? #f (object-property symbol 'translation-doc)))
      (begin
	(ly-warn (string-append "Redefining " (symbol->string symbol) "\n"))
	(exit 2)
      ))
  
  (set-object-property! symbol 'translation-type? type?)
  (set-object-property! symbol 'translation-doc description)
  (set! all-translation-properties (cons symbol all-translation-properties))
  )



(translator-property-description 'extraVerticalExtent
				 number-pair? "extra vertical extent, same format as  MinimumVerticalExtent")
(translator-property-description 'minimumVerticalExtent number-pair?
				 "minimum vertical extent, same format as VerticalExtent")
(translator-property-description 'verticalExtent number-pair?
				 "hard coded vertical extent.
The format is a pair of dimensions, for example, this sets the sizes
of a staff to 10 (5+5) staffspaces high.

@example
property Staff.verticalExtent = #(-5.0 . 5.0)
@end example

VerticalExtent, MinimumVerticalExtent and ExtraVerticalExtent are
predefined in all relevant contexts to @code{#f}, so they will not
inherit values.

Note that these VerticalExtents can only operate on vertical groups,
and therefore only work in contexts which contain an
@code{Axis_group_engraver}.
")

(translator-property-description
 'acknowledgeHashTable vector?
 "Internal variable: store interface to engraver smob table for current context. Don't mess with this."
 )

(translator-property-description 'acceptHashTable vector? "Internal
variable: store table with MusicName to Engraver entries.")
(translator-property-description 'aDueText string? "text for begin of a due")
(translator-property-description 'associatedVoice string? "Name of the Voice that has the melody for this LyricsVoice.")
(translator-property-description 'autoBeamSettings list? "
Specifies when automatically generated beams should begin and end.  The elements have the format:

@example

   function shortest-duration-in-beam time-signature

where

    function = begin or end
    shortest-duration-in-beam = numerator denominator; eg: 1 16
    time-signature = numerator denominator, eg: 4 4

unspecified or wildcard entries for duration or time-signature
are given by * *

The user can override beam begin or end time by pushing a wildcard entries
'(begin * * * *) or '(end * * * *) resp., eg:

    property Voice.autoBeamSettings push #'(end * * * *) = #(make-moment 1 4)

The head of the list:
    '(
((end * * 3 2) . ,(make-moment 1 2))
((end 1 16 3 2) . ,(make-moment 1 4))
((end 1 32 3 2) . ,(make-moment 1 8))
     ...
    )

@end example")

(translator-property-description 'autoAccidentals list? "List of
different ways to typeset an accidental. All algorithms in the list
are tried, and the one returning the most accidentals is used.
Each entry is either a symbol containg a context name or a name-value
pair containing an algorithm-description.
The list must begin with a symbol (context name).
The symbols denote in which context THE FOLLOWING algorithms (until next symbol) 
will be interpreted. All notes in the interpretation context will share accidentals.
The contexts must be stated in order, innermost first.
The algorithms are:
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
")

(translator-property-description 'autoCautionaries list? "List similar to
autoAccidentals, but it controls cautionary accidentals rather than
normal ones. Both lists are tried, and the one giving the most accidentals
wins. In case of draw, a normal accidental is typeset.
")

(translator-property-description 'automaticPhrasing boolean? " If set,
the @ref{Lyric_phrasing_engraver} will match note heads of context
called Voice X to syllables from LyricsVoice called
X-<something>. This feature is turned on by default. See the example
file @file{lyrics-multi-stanza.ly}.
")

(translator-property-description 'automaticMelismata boolean? " If
set, \addlyrics will assume that beams, slurs and ties signal
melismata, and align lyrics accordingly.
")

(translator-property-description 'barAlways boolean? " If set to true a bar line is drawn after each note.
")
(translator-property-description 'barCheckSynchronize boolean? "If
true then reset measurePosition when finding a barcheck. Turn off when
using barchecks in polyphonic music.")
(translator-property-description 'barNonAuto boolean? " If set to true then bar lines will not be printed
    automatically; they must be explicitly created with @code{bar}
    keywords.  Unlike with the @code{cadenza} keyword, measures are
    still counted.  Bar generation will resume according to that
    count if this property is set to zero.
")
(translator-property-description 'barNumberVisibility procedure? "Procedure that takes an int and returns whether the corresponding bar number should be printed")
(translator-property-description 'beamMelismaBusy boolean? "Signal if a beam is set when automaticMelismata is set")
(translator-property-description 'beatLength moment? "The length of one beat in this time signature.")
(translator-property-description 'breakAlignOrder list? "Defines the order in which
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
(translator-property-description 'busyGrobs list? "
a queue of (END-MOMENT . GROB) conses. This is for internal (C++) use only.
Use at your own risk.  This property contains the grobs for which  END-MOMENT >= NOW.
")
(translator-property-description 'centralCPosition number? "Place of the central C. Usually determined by looking at clefPosition and clefGlyph.")
(translator-property-description
 'changeMoment moment-pair?
 "duration that voices are examined for differences, when
part-combining.  Usually unset or zero when combining threads into one
voice, and 1 (or the duration of one measure) when combining voices
into one staff.")

(translator-property-description 'chordChanges boolean? "Only show changes in chords scheme?")
(translator-property-description 'clefGlyph string? "Name of the symbol within the music font")
(translator-property-description 'clefOctavation integer? "Add
this much extra octavation. Values of 7 and -7 are common.")
(translator-property-description 'clefPosition number? "Where should the center of the symbol go?")
(translator-property-description 'combineParts boolean? "try to combine parts?")
(translator-property-description 'connectArpeggios boolean? " If
set, connect all arpeggios that are found.  In this way, you can make
arpeggios that cross staves.
")
(translator-property-description 'createKeyOnClefChange boolean? "Print a key signature whenever the clef is changed.")
(translator-property-description 'crescendoText markup? "Text to print at start of non-hairpin crecscendo, ie: @samp{cresc.}")
(translator-property-description 'crescendoSpanner symbol? "Type of spanner to be used for crescendi.  One of: @samp{hairpin}, @samp{line}, @samp{dashed-line}, @samp{dotted-line}.  If unset, hairpin type is used.")
(translator-property-description 'decrescendoText markup? "Text to print at start of non-hairpin decrecscendo, ie: @samp{dim.}")
(translator-property-description 'currentBarNumber integer? "Contains the current barnumber. This property is incremented at
every barline.
")
(translator-property-description 'currentCommandColumn ly-grob? "Grob that is X-parent to all current breakable (clef, key signature, etc.) items.")
(translator-property-description 'currentMusicalColumn ly-grob? "Grob that is X-parent to all non-breakable items (note heads, lyrics, etc.).")
(translator-property-description 'defaultBarType string? "Sets the default type of bar line.  Available bar types: [FIXME];

This variable is typically read at Score level, so overriding
Staff.defaultBarType will have no effect.

")
(translator-property-description 'devNullThread symbol? "User control of Thread_devnull_engraver: one of
@table @samp
@item (), or unset
Behave in normal way: remove one set of grobs when in unisolo.
@item allways:
Remove any grob that comes along.
@item never:
Do nothing.
@end table
")
(translator-property-description 'devNullVoice symbol? "User control of Voice_devnull_engraver: one of
@table @samp
@item (), or unset
Behave in normal way: remove spanners when in unisolo.
@item allways:
Remove any spanners that come along.
@item never:
Do nothing.
@end table
")
(translator-property-description 'decrescendoSpanner symbol? "Type of spanner to be used for decrescendi.  One of: @samp{hairpin}, @samp{line}, @samp{dashed-line}, @samp{dotted-line}.  If unset, hairpin type is used.")

(translator-property-description 'dynamicAbsoluteVolumeFunction procedure? "
[DOCUMENT-ME]
")
(translator-property-description 'explicitClefVisibility procedure? "visibility-lambda function for clef changes.")



(translator-property-description 'explicitKeySignatureVisibility
procedure? "visibility-lambda function for explicit Key changes;
\override of #'break-visibility will set the visibility for normal
(ie. at the start of the line) key signatures.")

(translator-property-description 'extraNatural boolean? "Whether to typeset an
extra natural sign before accidentals changing from a non-natural to 
another non-natural.
")

(translator-property-description 'finalizations list? "List of expressions to evaluate before proceeding to next time step. Internal variable.")
(translator-property-description 'followVoice boolean?
				 "if set, note heads are tracked  across staff switches by a thin line")
(translator-property-description 'fontSize integer?
				 "Used to set the relative size of all grobs
in a context. This is done using the @code{Font_size_engraver}.")

(translator-property-description 'forceClef boolean? "Show clef symbol, even if it hasn't changed. Only active for the first clef after the property is set, not for the full staff.")
(translator-property-description 'graceAccidentalSpace number? "amount space to alot for an accidental")
(translator-property-description 'graceAlignPosition dir? "put the grace note before or after the main note?")
(translator-property-description 'highStringOne boolean? "Whether the 1st string is the string with
highest pitch on the instrument (used by the automatic string selector).")
(translator-property-description 'instr markup? "see @code{instrument}")
(translator-property-description 'instrument markup? " If @code{Instrument_name_engraver}
@cindex Instrument_name_engraver
 is
    added to the Staff translator, then the @code{instrument} property
    is used to label the first line of the staff and the @code{instr}
    property is used to label subsequent lines.  If the
    @code{midiInstrument} property is not set, then @code{instrument}
    is used to determine the instrument for MIDI output.")

(translator-property-description 'instrumentEqualizer procedure? "[DOCUMENT-ME]")
(translator-property-description 'instrumentSupport list? "
list of grobs to attach instrument name to. 
")				 
(translator-property-description 'keyAccidentalOrder list? "
Alist that defines in what order  alterations should be printed.
The format is (NAME . ALTER), where NAME is from 0 .. 6 and ALTER from  -1, 1.
")
(translator-property-description 'keySignature list? "The current key signature. This is an alist containing (NAME . ALTER) or ((OCTAVE . NAME) . ALTER) or ((OCTAVE . NAME) . (ALTER . BARNUMBER)) pairs, where NAME is from 0.. 6 and ALTER from -2,-1,0,1,2. The optional barnumber contains the number of the measure of the accidental. FIXME: describe broken tie entries.")

(translator-property-description 'lastKeySignature list? "Last key
signature before a key signature change.")

(translator-property-description 'localKeySignature list? "the key
signature at this point in the measure.  The format is the same as for keySignature. Is reset at every bar line."
) 
(translator-property-description 'localKeySignatureChanges list? "Experimental.
 [DOCME]")
(translator-property-description 'measureLength moment? "Length of one
measure in the current time signature last?")
(translator-property-description 'measurePosition moment? " How much
of the current measure (measured in whole notes) have we had.  This
can be set manually to create incomplete measures (anacrusis, upbeat),
the start of the music.
")
(translator-property-description 'melismaBusy boolean? "Signifies
whether a melisma is active. This can be used to signal melismas on
top of those automatically detected. ")
(translator-property-description 'melismaEngraverBusy boolean? "See @ref{(lilypond)melismaBusy}. This is set automatically.")
(translator-property-description 'midiInstrument string? "Name of the
MIDI instrument to use ")
(translator-property-description 'midiMinimumVolume number? "[DOCUMENT-ME]")
(translator-property-description 'midiMaximumVolume number? "[DOCUMENT-ME]")
(translator-property-description 'minimumFret number? "The tablature
auto string-selecting mechanism selects the highest string with a fret
not less than minimumFret")
(translator-property-description 'autoBeaming boolean? "If set to true
then beams are generated automatically.")
(translator-property-description 'noDirection boolean? "Don't set directions by a2-engraver when part-combining.")
(translator-property-description 'oneBeat moment? "  How long does one beat in the current time signature last?")
(translator-property-description 'othersolo boolean? "FIXME")
(translator-property-description 'pedalSustainStrings list? "List of   string to print for sustain-pedal. Format is
 (UP UPDOWN DOWN), where each of the three is the string to print when
this is done with the pedal.")
(translator-property-description 'pedalUnaCordaStrings list? "see pedalSustainStrings.")
(translator-property-description 'pedalSostenutoStrings list? "see pedalSustainStrings.")

(translator-property-description 'phrasingPunctuation string? "")
(translator-property-description 'rehearsalMark number-or-string? "")
(translator-property-description 'regularSpacingDelta moment? "TODO")
(translator-property-description 'repeatCommands list? "This property is read to find any command of the form (volta . X), where X is a string or #f")
(translator-property-description 'scriptDefinitions list? "
Description of scripts. This is used by Script_engraver for typesetting note-super/subscripts. See @file{scm/script.scm} for more information
")

(translator-property-description 'scriptHorizontal boolean? "  Put
scripts left or right of note heads.  Support for this is limited.
Accidentals will collide with scripts.
")

(translator-property-description 'skipBars boolean? " Set to true to
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
(translator-property-description 'skipTypesetting boolean?
				 "When true, all no typesetting is done at
this moment, causing  the interpretation phase to go a lot faster. This can
help with debugging large scores.")
(translator-property-description 'slurMelismaBusy boolean? "Signal a slur if automaticMelismata is set.")
(translator-property-description 'solo boolean? "set if solo is detected by the part combiner.")
(translator-property-description 'soloADue boolean? "set Solo/A due texts in the part combiner?.")
(translator-property-description 'soloIIText string? "text for begin of solo for voice ``two'' when part-combining.")
(translator-property-description 'soloText string? "text for begin of solo when part-combining.")
(translator-property-description 'sparseTies boolean? "only create one tie per chord.")
(translator-property-description 'splitInterval number-pair? "part-combiner will separate its two voices (or threads) when interval between the two voices is contained in this range.")
(translator-property-description 'split-interval boolean? "set if part-combiner separated voices based on splitInterval.")
(translator-property-description 'squashedPosition integer? " Vertical position of
squashing for Pitch_squash_engraver.")
(translator-property-description 'stringOneTopmost boolean? "Whether the 1st string is printed on the
top line of the tablature.")
(translator-property-description 'stavesFound list? "list of all staff-symbols found.")
(translator-property-description 'stanza markup? "Stanza `number' to print at start of a verse. Use in LyricsVoice context.")


(translator-property-description 'stemLeftBeamCount integer? "
Specify the number of beams to draw on the left side of the next note.
Overrides automatic beaming.  The value is only used once, and then it
is erased.
.")
(translator-property-description 'stemRightBeamCount integer? "idem, for the right side.")

(translator-property-description 'stz markup? "Abbreviated form for a stanza, see also Stanza property.")
(translator-property-description 'subdivideBeams boolean? "If set, multiple beams will be subdivided at beat
positions - by only drawing one beam over the beat.")
(translator-property-description 'systemStartDelimiter symbol? "Which grob to make for the start of the system/staff?")
(translator-property-description 'tablatureFormat procedure?
				 "Function formatting a tab notehead; it takes
a string number, a list of string tunings and Pitch object. It returns the text as a string.")

(translator-property-description 'textNonEmpty boolean? " If set
to true then text placed above or below the staff is not assumed to
have zero width.  @code{\fatText} and @code{\emptyText} are predefined
settings.
.")
(translator-property-description 'tieMelismaBusy boolean? "Signal ties when automaticMelismata is set.")
(translator-property-description 'timeSignatureFraction number-pair? "
pair of numbers,  signifying the time signature. For example #'(4 . 4) is a 4/4time signature.")
(translator-property-description 'timing boolean? " Keep administration of measure length, position, bar number, etc?
Switch off for cadenzas.")
(translator-property-description 'transposing integer? "Transpose the MIDI output.  Set this property to the number of half-steps to transpose by.")
(translator-property-description 'tremoloFlags integer? "Number of tremolo flags to add if none is specified.")
(translator-property-description 'tupletNumberFormatFunction procedure?
				 "Function taking a music as input, producing a string. This function is called to determine the text to print on a tuplet bracket.")

(translator-property-description 'tupletInvisible boolean? "
    If set to true, tuplet bracket creation is switched off
entirely. This has the same effect as setting both
@code{tupletNumberVisibility} and @code{tupletBracketVisibility} to
@code{#f}, but as this does not even create any grobs, this setting
uses less memory and time.")
(translator-property-description 'tupletSpannerDuration moment? "
Normally a tuplet bracket is as wide as the
@code{	imes} expression that gave rise to it. By setting this
property, you can make brackets last shorter. Example

@example
@@lilypond[verbatim,fragment]
context Voice 	imes 2/3 @{
  property Voice.tupletSpannerDuration = #(make-moment 1 4)
  [c8 c c] [c c c]
@}
@@end lilypond
@end example
.")
(translator-property-description 'unirhythm boolean? "set if unirhythm is detected by the part combiner.")
(translator-property-description 'unisilence boolean? "set if unisilence is detected by the part combiner.")
(translator-property-description 'unison boolean? "set if unisono is detected  by the part combiner. .")
(translator-property-description 'verticalAlignmentChildCallback
procedure? "what callback to add to children of a vertical alignment.
It determines what alignment procedure is used on the alignment
itself.  .")
(translator-property-description
 'voltaOnThisStaff boolean?
 "Normally, volta brackets are put only on the topmost staff. Setting this variable to true, will force a bracket to be on this staff as well.")
(translator-property-description 'voltaSpannerDuration moment? "maximum duration of the volta bracket.

    Set to a duration to control the size of the brackets printed by
@code{\\alternative}.  It specifies the number of whole notes duration
to use for the brackets.  This can be used to shrink the length of
brackets in the situation where one alternative is very large.  It may
have odd effects if the specified duration is longer than the music
given in an @code{\\alternative}.
.")
(translator-property-description 'weAreGraceContext boolean? ".")
(translator-property-description 'whichBar string?
				 "This property is read to determine what type of barline to create.

Example:
@example
\\property Staff.whichBar = \"|:\"
@end example

This will create a start-repeat bar in this staff only.

If not set explicitly (by property or @code{\bar}), this is set
according to values of @code{defaultBarType}, @code{barAlways},
@code{barNonAuto} and @code{measurePosition}.

Valid values are described in @ref{(lilypond-internals)bar-line-interface}.

.")
(translator-property-description 'stringTunings list? "The tablature strings tuning. Must be a list of the different semitons pitch of each string (starting by the lower one).")

