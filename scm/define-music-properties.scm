;;;; music-property-description.scm -- part of generated backend documentation
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c)  1998--2004  Han-Wen Nienhuys <hanwen@cs.uu.nl>
;;;;                 Jan Nieuwenhuizen <janneke@gnu.org>



(define (music-property-description symbol type? description)
  (if (not (equal? #f (object-property symbol 'music-doc)))
      (begin
	(ly:warn "Redefining ~S" symbol)
	(exit 2)
	))
  (set-object-property! symbol 'music-type? type?)
  (set-object-property! symbol 'music-doc description)
  symbol)

(define-public all-music-properties
  (map
   (lambda (x) (apply music-property-description x))
   `(
     (iterator-ctor ,procedure? "Function to construct music-event-iterator object for this Music")
     (duration ,ly:duration? "Duration of this note/lyric.")
     (metronome-count ,number? "How many beats in a minute?")
     (span-type ,string? "What kind of spanner should be created?

TODO: consider making type into symbol") 
     (absolute-octave ,integer?
		      "The absolute octave for a octave check note.")
     (articulations ,ly:music-list?
		    "Articulation events specifically for this note.")
     (articulation-type ,string? "key for script definitions alist.

TODO: consider making type into symbol ")
     (associated-context ,string? "Name of the Voice context associated with this \\newaddlyrics section")
     (bass ,boolean? "Set if this note is a bass note in a chord")
     (cautionary ,boolean? "If set, this alteration needs cautionary accidental")
     (change-to-id ,string? "name of the context to change to ")
     (change-to-type ,symbol? "type of the context to change to.")
     (compress-procedure ,procedure? "compress this music expression. Argument 1: the music, arg 2: factor")
     (context-id ,string? "name of context")
     (context-type ,symbol?  "type of context")
     (descend-only ,boolean? "If set, this @code{\\context} will only
descend in the context tree.")
     (denominator ,integer? "denominator in a time signature")
     (digit ,integer? "digit for fingering")
     (direction ,ly:dir? "Print this up or down?")
     (drum-type ,symbol? "Which percussion instrument to play this note on.")
     (tags ,list? "List of symbols that for denoting extra details,
e.g. @code{\\tag #'part ...} could tag a piece of music as only being active in a part.")
     (text-type ,symbol? "Particular type of text script (e.g. finger, dynamic).")
     (tempo-unit ,ly:duration? "The unit for the metronome count.")
     (tonic ,ly:pitch? "Base of the scale")
     (error-found ,boolean? "If true, a parsing error was found in this expression")
     (element ,ly:music? "The single child of a Music_wrapper music object, or the body of a repeat.")
     (elements ,ly:music-list? "A list of elements for sequential of simultaneous music, or the alternatives of repeated music. ")
     (force-accidental ,boolean? "If set, a cautionary accidental should always be printed on this note")
     (grob-property ,symbol? "The symbol of the grob property to set. ")
     (grob-value ,scheme? "The value of the grob property to set")
     (inversion ,boolean? "If set, this chord note is inverted.")
     (label ,markup? "label of a mark.")
     (last-pitch ,ly:pitch? "The last pitch after relativization.")
     (length ,ly:moment? "The duration of this music")
     (length-callback ,procedure? "How to compute the duration of this music")
     (internal-class-name ,string? "C++ class to use for this Music object") 
     (name ,symbol? "Name of this music object")
     (numerator ,integer? "numerator of a time signature")
     (once ,boolean? "Apply this operation only during one time step?")
     (octavation ,integer?
		  "This pitch was octavated by how many octaves?
For chord inversions, this is negative.")
     (origin ,ly:input-location? "where was this piece of music defined?")
     (page-penalty ,number? "Penalty for page break hint.")
     (penalty ,number? "Penalty for line break hint.")
     (pitch ,ly:pitch? "the pitch of this note")
     (pitch-alist ,list? "list of pitches jointly forming the scale of a key signature")
     (pop-first ,boolean? "Do a revert before we try to do a override on some grob property.")
     (procedure ,procedure?
		"The function to run with \\applycontext. It must take a single argument, being the context.")
     (property-operations ,list?
			  "Do these operations for instantiating the context.")
     (predicate ,procedure? "the predicate of a \\outputproperty.")
     (quoted-events ,vector? "A vector of with moment/event-list entries.")
     (quoted-music-name ,string? "The name of the voice to quote.")
     (quoted-voice-direction ,ly:dir? "Should the quoted voice be up-stem or down-stem?")
     (quoted-context-type ,symbol? "The name of the context to direct quotes to, eg., @code{Voice}.")
     (quoted-context-id ,string? "The id of the context to direct quotes to, eg., @code{cue}.")

     (to-relative-callback ,procedure? "How to transform a piece of music to relative pitches")
     (repeat-count  ,integer? "do a @code{\repeat} how ofen?")
     (span-direction ,ly:dir? "Does this start or stop a spanner?")
     (split-list ,list? "splitting moments for part combiner.")
     (start-callback ,procedure? "Function to compute the negative length of
starting grace notes.")
     (string-number ,integer? "The number of the string in a String_number_req")
     (symbol ,symbol? "Grob name to perform an override/revert on.")
     (text ,markup? "markup expression to be printed")
     (tremolo-type ,integer? "")
     (type ,symbol? "The type of this music object. Determines iteration in some cases.")
     (types ,list? "The types of this music object; determines by what
engraver this music expression is processed.")
     (value ,scheme? "Assignment value for a
translation property")
     (what ,symbol? "What to change for auto-change. FIXME, naming")
     (part-combine-status ,symbol?
			  "Change to what kind of state? Options are
solo1, solo2 and unisono")

     (figure ,markup? "a `figure' (which may be
a string) for figured bass")
     (alteration ,number? "alteration for figured bass")
     (bracket-start ,boolean? "start a bracket
here. TODO: use span requests?")
     (bracket-stop ,boolean? "stop a bracket here.")
     (untransposable ,boolean? "If set, this music is not transposed.")
     )))
