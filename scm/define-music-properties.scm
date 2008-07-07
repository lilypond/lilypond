;;;; music-property-description.scm -- part of generated backend documentation
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;;
;;;; (c) 1998--2007  Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;;                 Jan Nieuwenhuizen <janneke@gnu.org>

(define (music-property-description symbol type? description)
  (if (not (equal? #f (object-property symbol 'music-doc)))
      (ly:error (_ "symbol ~S redefined") symbol))
  (set-object-property! symbol 'music-type? type?)
  (set-object-property! symbol 'music-doc description)
  symbol)

(define-public all-music-properties
  (map
   (lambda (x) (apply music-property-description x))
   `(
     (X-offset ,number?
	       "Offset of resulting grob; only used for balloon texts.")
     (Y-offset ,number?
	       "Offset of resulting grob; only used for balloon texts.")

     (alteration ,number? "Alteration for figured bass.")
     (absolute-octave ,integer?
		      "The absolute octave for a octave check note.")
     (articulations ,ly:music-list?
		    "Articulation events specifically for this note.")
     (articulation-type ,string? "Key for script definitions alist.

TODO: Consider making type into symbol.")
     (augmented ,boolean? "This figure is for an augmented figured bass
(with @code{+} sign).")
     (augmented-slash ,boolean? "This figure is for an augmented figured bass
(back-slashed number).")
     (associated-context ,string? "Name of the Voice context associated with
this @code{\\newaddlyrics} section.")

     (bass ,boolean? "Set if this note is a bass note in a chord.")
     (bracket-start ,boolean? "Start a bracket here.

TODO: Use SpanEvents?")
     (bracket-stop ,boolean? "Stop a bracket here.")
     (break-penalty ,number? "Penalty for line break hint.")
     (break-permission ,symbol?
		       "Whether to allow, forbid or force a line break.")

     (cautionary ,boolean? "If set, this alteration needs a
cautionary accidental.")
     (change-to-id ,string? "Name of the context to change to.")
     (change-to-type ,symbol? "Type of the context to change to.")
     (compress-procedure ,procedure? "Compress this music expression.
Arg@tie{}1: the music, arg@tie{}2: factor.")
     (context-id ,string? "Name of context.")
     (context-type ,symbol?  "Type of context.")
     (create-new ,boolean? "Create a fresh context.")

     (delta-step ,number? "How much should a fall change pitch?")
     (descend-only ,boolean? "If set, this @code{\\context} only descends
in the context tree.")
     (denominator ,integer? "Denominator in a time signature.")
     (digit ,integer? "Digit for fingering.")
     (diminished ,boolean? "This bass figure should be slashed.")
     (direction ,ly:dir? "Print this up or down?")
     (drum-type ,symbol? "Which percussion instrument to play this note on.")
     (duration ,ly:duration? "Duration of this note or lyric.")

     (error-found ,boolean?
		  "If true, a parsing error was found in this expression.")
     (element ,ly:music? "The single child of a Music_wrapper music object,
or the body of a repeat.")
     (elements ,ly:music-list? "A list of elements for sequential of
simultaneous music, or the alternatives of repeated music.")
     (elements-callback ,procedure? "Return a list of children, for use by
a sequential iterator.  Takes a single music parameter.")
     (expected-beam-count ,integer? "Expected number of non-tremolo beams
in a tremolo repeat.")

     (figure ,integer? "A bass figure.")
     (force-accidental ,boolean? "If set, a cautionary accidental should
always be printed on this note.")

     (grob-property ,symbol? "The symbol of the grob property to set.")
     (grob-property-path ,list? "A list of symbols, locating a nested grob
property, e.g., @code{(beamed-lengths details)}.")
     (grob-value ,scheme? "The value of the grob property to set.")

     (input-tag ,scheme? "Arbitrary marker to relate input and output.")
     (inversion ,boolean? "If set, this chord note is inverted.")
     (iterator-ctor ,procedure? "Function to construct a
@code{music-event-iterator} object for this music.")

     (label ,markup? "Label of a mark.")
     (last-pitch ,ly:pitch? "The last pitch after relativization.")
     (length ,ly:moment? "The duration of this music.")
     (length-callback ,procedure? "How to compute the duration of this music.
This property can only be defined as initializer in
@file{scm/@/define-music-types.scm}.")
     (line-break-permission ,symbol? "When the music is at top-level,
whether to allow, forbid or force a line break.")

     (metronome-count ,number? "How many beats in a minute?")

     (name ,symbol? "Name of this music object.")
     (no-continuation ,boolean? "If set, disallow continuation lines.")
     (numerator ,integer? "Numerator of a time signature.")

     (once ,boolean? "Apply this operation only during one time step?")
     (octavation ,integer? "This pitch was octavated by how many octaves?
For chord inversions, this is negative.")
     (origin ,ly:input-location? "Where was this piece of music defined?")

     (page-break-permission ,symbol? "When the music is at top-level,
whether to allow, forbid or force a page break.")
     (page-label ,symbol? "The label of a page marker.")
     (page-marker ,boolean? "If true, and the music expression is found at
top-level, a page marker object is instanciated instead of a score.")
     (page-turn-permission ,symbol? "When the music is at top-level,
whether to allow, forbid or force a page turn.")
     (part-combine-status ,symbol? "Change to what kind of state?
Options are @code{solo1}, @code{solo2} and @code{unisono}.")
     (parenthesize ,boolean? "Enclose resulting objects in parentheses?")
     (pitch ,ly:pitch? "The pitch of this note.")
     (pitch-alist ,list? "A list of pitches jointly forming the scale
of a key signature.")
     (pop-first ,boolean? "Do a revert before we try to do a override
on some grob property.")
     (prob-property ,symbol? "The symbol of the prob property to set.")
     (procedure ,procedure? "The function to run with @code{\\applycontext}.
It must take a single argument, being the context.")
     (property-operations ,list? "Do these operations for instantiating
the context.")

     (quoted-events ,vector? "A vector of with @code{moment} and
@code{event-list} entries.")
     (quoted-music-name ,string? "The name of the voice to quote.")
     (quoted-voice-direction ,ly:dir? "Should the quoted voice be up-stem
or down-stem?")
     (quoted-context-type ,symbol? "The name of the context to
direct quotes to, e.g., @code{Voice}.")
     (quoted-context-id ,string? "The ID of the context to direct quotes to,
e.g., @code{cue}.")
     (quoted-transposition ,ly:pitch? "The pitch used for the quote,
overriding @code{\\transposition}.")

     (repeat-count ,integer? "Do a @code{\\repeat} how often?")

     (span-direction ,ly:dir? "Does this start or stop a spanner?")
     (span-type ,string? "What kind of spanner should be created?

TODO: Consider making type into symbol.")
     (split-list ,list? "Splitting moments for part combiner.")
     (start-callback ,procedure? "Function to compute the negative length
of starting grace notes.  This property can only be defined as initializer
in @file{scm/@/define-music-types.scm}.")
     (string-number ,integer? "The number of the string in
a @code{StringNumberEvent.}")
     (symbol ,symbol? "Grob name to perform an override or revert on.")

     (tags ,list? "List of symbols that for denoting extra details, e.g.,
@code{\\tag #'part @dots{}} could tag a piece of music as only being active
in a part.")
     (text-type ,symbol?
		"Particular type of text script (e.g., finger, dynamic).")
     (text ,markup? "Markup expression to be printed.")
     (tempo-unit ,ly:duration? "The unit for the metronome count.")
     (to-relative-callback ,procedure? "How to transform a piece of music
to relative pitches.")
     (tonic ,ly:pitch? "Base of the scale.")
     (tremolo-type ,integer? "Speed of tremolo, e.g., 16 for @code{c4:16}.")
     (trill-pitch ,ly:pitch? "Pitch of other note of the trill.")
     (type ,symbol? "The type of this music object.
Determines iteration in some cases.")
     (types ,list? "The types of this music object; determines by what
engraver this music expression is processed.")
     (tweaks ,list? "An alist of properties to override in the backend
for the grob made of this event.")

     (untransposable ,boolean? "If set, this music is not transposed.")

     (value ,scheme? "Assignment value for a translation property.")
     (void ,boolean? "If this property is @code{#t}, then the
music expression is to be discarded by the toplevel music handler.")

     (what ,symbol? "What to change for auto-change.

FIXME: Naming.")
    )))
