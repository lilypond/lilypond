;;;; music-property-description.scm -- part of generated backend documentation
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 1998--2001  Han-Wen Nienhuys <hanwen@cs.uu.nl>
;;;;                 Jan Nieuwenhuizen <janneke@gnu.org>


(define all-music-properties '())

(define (music-property-description symbol type? description)
 (if (not (equal? #f (object-property symbol 'music-doc)))
      (begin
	(ly-warn (string-append "Redefining " (symbol->string symbol) "\n"))
	(exit 2)
      ))
 (set-object-property! symbol 'music-type? type?)
 (set-object-property! symbol 'music-doc description)
 (set! all-music-properties (cons symbol all-music-properties))
 )



(music-property-description 'iterator-ctor procedure? "Function to construct music-event-iterator object for this Music")
(music-property-description 'duration duration? "Duration of this note/lyric.")
(music-property-description 'metronome-count number? "How many beats in a minute?")
(music-property-description 'span-type string? "What kind of spanner should be created?

TODO: consider making type into symbol") 
(music-property-description 'articulation-type string? "key for script definitions alist.

TODO: consider making type into symbol ")
(music-property-description 'bass boolean? "Set if this note is a bass note in a chord")
(music-property-description 'cautionary boolean? "If set, this alteration needs cautionary accidental")
(music-property-description 'change-to-id string? "name of the context to change to ")
(music-property-description 'change-to-type string? "type of the context to change to.")
(music-property-description 'context-id string? "name of context")
(music-property-description 'context-type string?  "type of context")
(music-property-description 'denominator integer? "denominator in a time signature")
(music-property-description 'direction dir? "Print this up or down?")
(music-property-description 'text-type symbol? "Particular type of text script (eg. finger, dynamic).")
(music-property-description 'element music? "The single child of a Music_wrapper music object, or the body of a repeat.")
(music-property-description 'elements music-list? "A list of elements for sequential of simultaneous music, or the alternatives of repeated music. ")
(music-property-description 'force-accidental boolean? "If set, a cautionary accidental should always be printed on this note")
(music-property-description 'grob-property symbol? "The symbol of the grob property to set. ")
(music-property-description 'grob-value scheme? "The value of the grob property to set")
(music-property-description 'inversion boolean? "If set, this chord note is inverted.")
(music-property-description 'label markup? "label of a mark.")
(music-property-description 'last-pitch pitch? "The last pitch after relativization.")
(music-property-description 'length procedure? "How to compute the duration of this music")
(music-property-description 'numerator integer? "numerator of a time signature")
(music-property-description 'origin ly-input-location? "where was this piece of music defined?")
(music-property-description 'penalty number? "Penalty for break hint.")
(music-property-description 'pitch pitch? "the pitch of this note")
(music-property-description 'pitch-alist list? "list of pitches jointly forming the scale of a key signature")
(music-property-description 'pop-first boolean? "Do a revert before we try to do a override on some grob property.")

(music-property-description 'predicate procedure? "the predicate of a \outputproperty")
(music-property-description 'type symbol? "The type of this music object. Determines iteration in some cases.")
(music-property-description 'repeat-count  integer? "do a @code{\repeat} how ofen?")
(music-property-description 'span-direction dir? "Does this start or stop a spanner?")

(music-property-description 'start-moment-function procedure? "Function to compute the negative length of
starting grace notes.")
(music-property-description 'string-number integer? "The number of the string in a String_number_req")
(music-property-description 'symbol symbol? "Grob name to perform an override/revert on.")
(music-property-description 'text markup? "markup expression to be printed")
;; markup?
(music-property-description 'tremolo-type integer? "")
(music-property-description 'value scheme? "Assignment value for a
translation property")
(music-property-description 'what string? "What to change for auto-change. FIXME, naming")

(music-property-description 'figure number? "number for figured bass")
(music-property-description 'alteration number? "alteration for figured bass")
(music-property-description 'bracket-start boolean? "start a bracket
here. TODO: use span requests?")
(music-property-description 'bracket-stop boolean? "stop a bracket here.")

