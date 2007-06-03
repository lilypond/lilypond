;;;; music-property-description.scm -- part of generated backend documentation
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 1998--2006  Han-Wen Nienhuys <hanwen@xs4all.nl>
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
     (X-offset ,number? "Offset of resulting grob; only used for balloon texts.")
     (Y-offset ,number? "Offset of resulting grob; only used for balloon texts. ")
     
     (alteration ,number? "alteration for figured bass")

     (absolute-octave ,integer?
		      "The absolute octave for a octave check note.")
     (articulations ,ly:music-list?
		    "Articulation events specifically for this note.")
     (articulation-type ,string? "key for script definitions alist.

TODO: consider making type into symbol ")
     (augmented ,boolean? "This figure is for an augmented figured bass (with +) sign.")
     (associated-context ,string? "Name of the Voice context associated with this \\newaddlyrics section")
     (bass ,boolean? "Set if this note is a bass note in a chord")
     (bracket-start ,boolean? "start a bracket
here. TODO: use SpanEvents?")
     (bracket-stop ,boolean? "stop a bracket here.")
     (break-penalty ,number? "Penalty for line break hint.")
     (break-permission ,symbol? "Whether to allow, forbid or force a line break.")
     (cautionary ,boolean? "If set, this alteration needs cautionary accidental")
     (change-to-id ,string? "name of the context to change to ")
     (change-to-type ,symbol? "type of the context to change to.")
     (compress-procedure ,procedure? "compress this music expression. Argument 1: the music, arg 2: factor")
     (context-id ,string? "name of context")
     (context-type ,symbol?  "type of context")
     (create-new ,boolean? "Create a fresh context.")
     (delta-step ,number? "How much should a fall change pitch?") 
     (descend-only ,boolean? "If set, this @code{\\context} will only descend in the context tree.")
     (denominator ,integer? "denominator in a time signature")
     (digit ,integer? "digit for fingering")
     (diminished ,boolean? "This bass figure should be slashed.")
     (direction ,ly:dir? "Print this up or down?")
     (drum-type ,symbol? "Which percussion instrument to play this note on.")
     (duration ,ly:duration? "Duration of this note/lyric.")
     (error-found ,boolean? "If true, a parsing error was found in this expression")
     (element ,ly:music? "The single child of a Music_wrapper music object, or the body of a repeat.")
     (elements ,ly:music-list? "A list of elements for sequential of simultaneous music, or the alternatives of repeated music. ")
     (elements-callback ,procedure? "Return a list of children, for use by a sequential iterator. Takes a single Music parameter")
     (expected-beam-count ,integer? "Expected number of non-tremolo beams in a tremolo repeat")
     (figure ,integer? "a bass figure")
     (force-accidental ,boolean? "If set, a cautionary accidental should always be printed on this note")
     (grob-property ,symbol? "The symbol of the grob property to set. ")
     (grob-property-path ,list? "A list of symbols, locating a nested grob property, e.g. (beamed-lengths details). ")
     (grob-value ,scheme? "The value of the grob property to set")
     (input-tag ,scheme? "Arbitrary marker to relate input and output")
     (inversion ,boolean? "If set, this chord note is inverted.")
     (iterator-ctor ,procedure? "Function to construct music-event-iterator object for this Music")
     
     (label ,markup? "label of a mark.")
     (last-pitch ,ly:pitch? "The last pitch after relativization.")
     (length ,ly:moment? "The duration of this music")
     (length-callback ,procedure? "How to compute the duration of this music. This property can only be defined as initializer in @file{define-music-types.scm}.")
     (line-break-permission ,symbol? "When the music is at top-level, whether to allow, forbid or force a line break.")
     (metronome-count ,number? "How many beats in a minute?")
     (name ,symbol? "Name of this music object")
     (no-continuation ,boolean? "If set, disallow continuation lines")
     (numerator ,integer? "numerator of a time signature")
     (once ,boolean? "Apply this operation only during one time step?")
     (octavation ,integer? "This pitch was octavated by how many octaves? For chord inversions, this is negative.")
     (origin ,ly:input-location? "where was this piece of music defined?")
     (page-break-permission ,symbol? "When the music is at top-level, whether to allow, forbid or force a page break.")
     (page-label ,symbol? "The label of a page marker")
     (page-marker ,boolean? "If true, and the music expression is found at top-level, a page marker object is instanciated instead of a score.")
     (page-turn-permission ,symbol? "When the music is at top-level, whether to allow, forbid or force a page turn.")
     (part-combine-status ,symbol?
			  "Change to what kind of state? Options are
solo1, solo2 and unisono")
     (parenthesize ,boolean? "Enclose resulting objects in parentheses?")
     (pitch ,ly:pitch? "the pitch of this note")
     (pitch-alist ,list? "list of pitches jointly forming the scale of a key signature")
     (pop-first ,boolean? "Do a revert before we try to do a override on some grob property.")
     (prob-property ,symbol? "The symbol of the prob property to set. ")
     (procedure ,procedure?
		"The function to run with \\applycontext.
It must take a single argument, being the context.")
     (property-operations ,list?
			  "Do these operations for instantiating the context.")
     (quoted-events ,vector? "A vector of with moment/event-list entries.")
     (quoted-music-name ,string? "The name of the voice to quote.")
     (quoted-voice-direction ,ly:dir? "Should the quoted voice be up-stem or down-stem?")
     (quoted-context-type ,symbol? "The name of the context to direct quotes to, eg., @code{Voice}.")
     (quoted-context-id ,string? "The id of the context to direct quotes to, eg., @code{cue}.")
     (quoted-transposition ,ly:pitch? "The pitch used for the quote, overriding \\transposition")
     (to-relative-callback ,procedure? "How to transform a piece of music to relative pitches")
     (repeat-count  ,integer? "do a @code{\repeat} how ofen?")
     (span-direction ,ly:dir? "Does this start or stop a spanner?")
     (span-type ,string? "What kind of spanner should be created?

TODO: consider making type into symbol") 
     (split-list ,list? "splitting moments for part combiner.")
     (start-callback ,procedure? "Function to compute the negative
length of starting grace notes.  This property can only be defined as
initializer in @file{define-music-types.scm}.")
     (string-number ,integer? "The number of the string in a StringNumberEvent")
     (symbol ,symbol? "Grob name to perform an override/revert on.")
     (tags ,list? "List of symbols that for denoting extra details,
e.g. @code{\\tag #'part ...} could tag a piece of music as only being active in a part.")
     (text-type ,symbol? "Particular type of text script (e.g. finger, dynamic).")
     
     (text ,markup? "markup expression to be printed")
     (tempo-unit ,ly:duration? "The unit for the metronome count.")
     (tonic ,ly:pitch? "Base of the scale")
     (tremolo-type ,integer? "speed of tremolo, e.g. 16 for c4:16")
     (trill-pitch ,ly:pitch? "Pitch of other note of the trill.")
     (type ,symbol? "The type of this music object. Determines iteration in some cases.")
     (types ,list? "The types of this music object; determines by what
engraver this music expression is processed.")
     (tweaks ,list? "An alist of properties to override in the backend
-for the grob made of this event.")
     (value ,scheme? "Assignment value for a
translation property")
     (void ,boolean? "If this property is #t, then the music expression is to be
discarded by the toplevel music handler.")
     (what ,symbol? "What to change for auto-change. FIXME, naming")
     (untransposable ,boolean? "If set, this music is not transposed.")
     )))
