
(define all-translation-properties '())

(define (translator-property-description symbol type? description)
  (set-object-property! symbol 'translation-type? type?)
  (set-object-property! symbol 'translation-doc description)
  (set! all-translation-properties (cons symbol all-translation-properties))
  )



(translator-property-description 'CONTEXTNAMEMinimumVerticalExtent number-pair? "minimum vertical extent, same format as CONTEXTNAMEVerticalExtent [fixme, naming]")
(translator-property-description 'CONTEXTNAMEVerticalExtent number-pair? "hard coded vertical extent.
The format is a pair of dimensions, for example, this sets the sizes
of a staff to 10 (5+5) staffspaces high.

@example
property Staff.StaffVerticalExtent = #(-5.0 . 5.0)
@end example

 [fixme, naming]")
(translator-property-description 'CONTEXTNAMExtraVerticalExtent number-pair? "extra vertical extent, same format
CONTEXTNAMEMinimumVerticalExtent [fixme, naming]")
(translator-property-description 'Generic_property_list list? "description of the conversion.

Defines names and types for generic properties. These are properties
than can be plugged into the backend directly. See the init file
@file{generic-property.scm} for details.  For internal use only,
deprecated.
")
(translator-property-description 'aDueText string? "text for begin of a due")
(translator-property-description 'associatedVoice string? "")
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
(translator-property-description 'automaticPhrasing boolean? "")
(translator-property-description 'barAlways boolean? " If set to true a bar line is drawn after each note.
")
(translator-property-description 'barCheckNoSynchronize boolean? "If set, don't reset measurePosition when finding a bbarcheck. This
makes bar-checks for polyphonic music easier.")
(translator-property-description 'barNonAuto boolean? " If set to true then bar lines will not be printed
    automatically; they must be explicitly created with @code{bar}
    keywords.  Unlike with the @code{cadenza} keyword, measures are
    still counted.  Bar generation will resume according to that
    count if this property is set to zero.
")
(translator-property-description 'beamMelismaBusy boolean? "Signal if a beam is set when automaticMelismata is set")
(translator-property-description 'beamMelismaBusy boolean? "")
(translator-property-description 'breakAlignOrder list? "Defines the order in which
prefatory matter (clefs, key signatures) appears, eg. this puts the
key signatures after the bar lines:

@example
	\\property Score.breakAlignOrder = #'(
	  Span_bar
	  Breathing_sign
	  Clef_item
	  Staff_bar
	  Key_item
	  Time_signature
	)
@end example
")
(translator-property-description 'centralCPosition number? "Place of the central C. Usually determined by looking at clefPosition and clefGlyph.")
(translator-property-description 'chordInversion boolean? " Determines whether LilyPond should look for chord inversions when
    translating from notes to chord names.  Set to 1 to find
    inversions.  The default is 0 which does not look for
    inversions.")
(translator-property-description 'clefGlyph string? "Name of the symbol within the music font")
(translator-property-description 'clefOctavation integer? "Add
this much extra octavation. Values of 7 and -7 are common.")
(translator-property-description 'clefPitches list? "an alist mapping GLYPHNAME to the position of the central C for that symbol")
(translator-property-description 'clefPosition number? "Where should the center of the symbol go?")
(translator-property-description 'combineParts boolean? "try to combine parts?")
(translator-property-description 'connectArpeggios boolean? " If
set, connect all arpeggios that are found.  In this way, you can make
arpeggios that cross staffs.
")
(translator-property-description 'createKeyOnClefChange boolean? "")
(translator-property-description 'currentBarNumber integer? "this is read to determine
 the number to put on the bar ")
(translator-property-description 'currentBarNumber integer? "Contains the current barnumber. This property is incremented at
every barline.
")
(translator-property-description 'currentCommandColumn ly-grob? "")
(translator-property-description 'currentMusicalColumn ly-grob? "")
(translator-property-description 'defaultBarType string? "Sets the default type of bar line.  Available bar types: [FIXME]
")
(translator-property-description 'drarnChords boolean? "")
(translator-property-description 'explicitClefVisibility procedure? "visibility-lambda function for clef changes.")
(translator-property-description 'explicitKeySignatureVisibility procedure? "")
(translator-property-description 'forgetAccidentals boolean? "do
not set localKeySignature when a note alterated differently from
localKeySignature is found.

Causes accidentals to be printed at every note instead of
remembered for the duration of a measure.
")
(translator-property-description 'graceAccidentalSpace number? "amount space to alot for an accidental")
(translator-property-description 'graceAlignPosition dir? "put the grace note before or after the main note?")
(translator-property-description 'instr list-or-string? "see @code{instrument}")
(translator-property-description 'instrument list-or-string? " If @code{Instrument_name_engraver}
@cindex Instrument_name_engraver
 is
    added to the Staff translator, then the @code{instrument} property
    is used to label the first line of the staff and the @code{instr}
    property is used to label subsequent lines.  If the
    @code{midiInstrument} property is not set, then @code{instrument}
    is used to determine the instrument for MIDI output.")
(translator-property-description 'keyAccidentalOrder list? "")
(translator-property-description 'keyOctaviation boolean? "")
(translator-property-description 'keySignature list? "")
(translator-property-description 'keySignature list? "")
(translator-property-description 'localKeySignature list? "the key signature at this point  in the measure")
(translator-property-description 'measureLength moment? "  How long does one measure in the current time signature last?")
(translator-property-description 'measurePosition moment? "
  How much of the current measure (measured in whole notes) have we had?

Set this  manually to  create  incomplete measures (anacrusis, upbeat), eg. at the start of 
the music.
")
(translator-property-description 'melismaBusy boolean? "")
(translator-property-description 'melismaEngraverBusy boolean? "")
(translator-property-description 'midiInstrument string? "")
(translator-property-description 'noAutoBeaming boolean? "  If set to true then beams are not generated automatically.
")
(translator-property-description 'noResetKey boolean? "Do not
reset local key to the value of keySignature at the start of a measure,
as determined by measurePosition.

Do not reset the key at the start of a measure.  Accidentals will be
printed only once and are in effect until overridden, possibly many
measures later.
")
(translator-property-description 'oneBeat moment? "  How long does one beat in the current time signature last?")
(translator-property-description 'phrasingPunctuation string? "")
(translator-property-description 'rehearsalMark number-or-string? "")
(translator-property-description 'repeatCommands list? "This property is read to find any command of the form (volta . X), where X is a string or #f")
(translator-property-description 'repeatCommands list? "")
(translator-property-description 'scriptDefinitions list? "
Description of scripts to use.  (fixme) 
")
(translator-property-description 'scriptHorizontal boolean? "    Put scripts left or right of note heads.  Support for this is
    limited.  Accidentals will collide with scripts.
    
")
(translator-property-description 'scriptHorizontal boolean? "    Put scripts left or right of note heads.  Support for this is
    limited.  Accidentals will collide with scripts.
    
")
(translator-property-description 'skipBars boolean? " Set to true to skip the empty bars that are produced by
    multimeasure notes and rests.  These bars will not appear on the
    printed output.  If not set (the default)  multimeasure
    notes and rests expand into their full length, printing the appropriate
    number of empty bars so that synchronization with other voices is
    preserved.

@c my @vebatim patch would help...
@example
@@lilypond[fragment,verbatim,center]
r1 r1*3 R1*3property Score.skipBars=1 r1*3 R1*3

@@end lilypond
@end example

")
(translator-property-description 'slurBeginAttachment symbol? "translates to the car of Slur.element-property 'attachment.")
(translator-property-description 'slurEndAttachment symbol? "translates to the cdr of Slur.element-property 'attachment.")
(translator-property-description 'slurMelismaBusy boolean? "")
(translator-property-description 'slurMelismaBusy boolean? "Signal a slur if automaticMelismata is set")
(translator-property-description 'solo boolean? "set if solo is detected")
(translator-property-description 'soloADue boolean? "set Solo/A due texts?")
(translator-property-description 'soloIIText string? "text for begin of solo for voice ``two''")
(translator-property-description 'soloText string? "text for begin of solo")
(translator-property-description 'sparseTies boolean? "only create one tie per chord.")
(translator-property-description 'split-interval number-pair? "always split into two voices for contained intervals")
(translator-property-description 'squashedPosition integer? " Vertical position of
squashing.")
(translator-property-description 'staffsFound list? "list of all staff-symbols found.")
(translator-property-description 'staffsFound list? "")
(translator-property-description 'stanza string? "Stanza `number' to print at start of a verse")
(translator-property-description 'startSustain string? "")
(translator-property-description 'startUnaChorda string? "")
(translator-property-description 'stemLeftBeamCount integer? "
Specify the number of beams to draw on the left side of the next note.
Overrides automatic beaming.  The value is only used once, and then it
is erased.
")
(translator-property-description 'stemRightBeamCount integer? "idem, for the right side")
(translator-property-description 'stopStartSustain string? "")
(translator-property-description 'stopSustain string? "")
(translator-property-description 'stopUnaChorda string? "")
(translator-property-description 'stz string? "")
(translator-property-description 'textNonEmpty boolean? " If set
to true then text placed above or below the staff is not assumed to
have zero width.  @code{fatText} and @code{emptyText} are predefined
settings.
")
(translator-property-description 'tieMelismaBusy boolean? "")
(translator-property-description 'tieMelismaBusy boolean? "Signal ties when automaticMelismata is set")
(translator-property-description 'timeSignatureFraction number-pair? "
pair of numbers,  signifying the time signature. For example #'(4 . 4) is a 4/4time signature.")
(translator-property-description 'timing boolean? " Keep administration of measure length, position, bar number, etc?
Switch off for cadenzas.")
(translator-property-description 'tremoloFlags integer? "")
(translator-property-description 'tupletInvisible boolean? "
    If set to true, tuplet bracket creation is switched off
entirely. This has the same effect as setting both
@code{tupletNumberVisibility} and @code{tupletBracketVisibility} to
@code{#f}, but as this does not even create elements, this setting
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
")
(translator-property-description 'unirhythm boolean? "set if unirhythm is detected")
(translator-property-description 'unisilence boolean? "set if unisilence is detected")
(translator-property-description 'unison boolean? "set if unisono is detected  ")
(translator-property-description 'verticalAlignmentChildCallback
procedure? "what callback to add to children of a vertical alignment.
It determines what alignment procedure is used on the alignment
itself.  ")
(translator-property-description 'voltaSpannerDuration moment? "maximum duration of the volta bracket.

    Set to a duration to control the size of the brackets printed by
@code{lternative}.  It specifies the number of whole notes duration
to use for the brackets.  This can be used to shrink the length of
brackets in the situation where one alternative is very large.  It may
have odd effects if the specified duration is longer than the music
given in an @code{lternative}.
")
(translator-property-description 'weAreGraceContext boolean? "")
(translator-property-description 'whichBar string? "This property is read to determine what type of barline to create.
Example:
@example
\\property Staff.whichBar = \"|:\"
@end example
will create a start-repeat bar in this staff only 
")
(translator-property-description 'whichBar string? "")
(translator-property-description 'whichBar string? "if not set
explicitly (by property or bar), this is set according to values of
defaultBarType, barAlways, barNonAuto and measurePosition.
 ")
