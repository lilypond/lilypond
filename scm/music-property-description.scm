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

(define (true? x) #t)

(music-property-description 'iterator-ctor c++-function? "Function to construct music-event-iterator object for this Music")
(music-property-description 'duration duration? "")
(music-property-description 'metronome-count number? "How many beats in a minute?")
(music-property-description 'span-type symbol? "What kind of spanner should be created?")
(music-property-description 'alternatives music? "Music_sequence of alternatives for repeated music.")
(music-property-description 'articulation-type symbol? "key for scriptDefinitions alist")
(music-property-description 'bass boolean? "")
(music-property-description 'body music? "")
(music-property-description 'cautionary boolean? "")
(music-property-description 'change-to-id string? "")
(music-property-description 'change-to-type string? "")
(music-property-description 'context-id string? "")
(music-property-description 'context-type string?  "")
(music-property-description 'denominator integer? "")
(music-property-description 'direction dir? "")
(music-property-description 'text-type symbol? "")
(music-property-description 'element music )
(music-property-description 'grob-property symbol? "")
(music-property-description 'grob-value true? "")
(music-property-description 'elements list? "")
(music-property-description 'force-accidental boolean? "")
(music-property-description 'inversion boolean? "")
(music-property-description 'label string? "")
(music-property-description 'last-pitch pitch? "")
(music-property-description 'length procedure? "")
(music-property-description 'lyrics music? "")
(music-property-description 'mark-label string? "")
(music-property-description 'numerator integer? "")
(music-property-description 'one music? ""); part-combine, fixme, naming.
(music-property-description 'origin input? "")
(music-property-description 'penalty number? "")
(music-property-description 'pitch pitch? "")
(music-property-description 'pitch-alist list? "")
(music-property-description 'predicate procedure? "")
(music-property-description 'type symbol? "")
(music-property-description 'repeat-count  integer? "")
(music-property-description 'span-direction dir? "")
(music-property-description 'symbol symbol? "")
(music-property-description 'symbols list? "")
(music-property-description 'tempo duration? "")
(music-property-description 'text string? "");; markup?
(music-property-description 'tremolo-type integer? "")
(music-property-description 'two music? ""); part-combine, fixme, naming.
(music-property-description 'value true? "")
(music-property-description 'what string? "")
