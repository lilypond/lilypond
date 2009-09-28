;;;; music-functions.scm --
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;;
;;;; (c) 1998--2009 Jan Nieuwenhuizen <janneke@gnu.org>
;;;;                 Han-Wen Nienhuys <hanwen@xs4all.nl>

;; (use-modules (ice-9 optargs))

;;; ly:music-property with setter
;;; (ly:music-property my-music 'elements)
;;;   ==> the 'elements property
;;; (set! (ly:music-property my-music 'elements) value)
;;;   ==> set the 'elements property and return it
(define-public ly:music-property
  (make-procedure-with-setter ly:music-property
			      ly:music-set-property!))

(define-safe-public (music-is-of-type? mus type)
  "Does @code{mus} belong to the music class @code{type}?"
  (memq type (ly:music-property mus 'types)))

;; TODO move this
(define-public ly:grob-property
  (make-procedure-with-setter ly:grob-property
			      ly:grob-set-property!))

(define-public ly:prob-property
  (make-procedure-with-setter ly:prob-property
			      ly:prob-set-property!))

(define-public (music-map function music)
  "Apply @var{function} to @var{music} and all of the music it contains.

First it recurses over the children, then the function is applied to MUSIC.
"
  (let ((es (ly:music-property music 'elements))
	(e (ly:music-property music 'element)))
    (set! (ly:music-property music 'elements)
	  (map (lambda (y) (music-map function y)) es))
    (if (ly:music? e)
	(set! (ly:music-property music 'element)
	      (music-map function  e)))
    (function music)))

(define-public (music-filter pred? music)
  "Filter out music expressions that do not satisfy PRED."

  (define (inner-music-filter pred? music)
    "Recursive function."
    (let* ((es (ly:music-property music 'elements))
	   (e (ly:music-property music 'element))
	   (as (ly:music-property music 'articulations))
	   (filtered-as (filter ly:music? (map (lambda (y) (inner-music-filter pred? y)) as)))
	   (filtered-e (if (ly:music? e)
			   (inner-music-filter pred? e)
			   e))
	   (filtered-es (filter ly:music? (map (lambda (y) (inner-music-filter pred? y)) es))))
      (set! (ly:music-property music 'element) filtered-e)
      (set! (ly:music-property music 'elements) filtered-es)
      (set! (ly:music-property music 'articulations) filtered-as)
      ;; if filtering emptied the expression, we remove it completely.
      (if (or (not (pred? music))
	      (and (eq? filtered-es '()) (not (ly:music? e))
		   (or (not (eq? es '()))
		       (ly:music? e))))
	  (set! music '()))
      music))

  (set! music (inner-music-filter pred? music))
  (if (ly:music? music)
      music
      (make-music 'Music)))	  ;must return music.

(define-public (display-music music)
  "Display music, not done with music-map for clarity of presentation."

  (display music)
  (display ": { ")
  (let ((es (ly:music-property music 'elements))
	(e (ly:music-property music 'element)))
    (display (ly:music-mutable-properties music))
    (if (pair? es)
	(begin (display "\nElements: {\n")
	       (map display-music es)
	       (display "}\n")))
    (if (ly:music? e)
	(begin
	  (display "\nChild:")
	  (display-music e))))
  (display " }\n")
  music)

;;;
;;; A scheme music pretty printer
;;;
(define (markup-expression->make-markup markup-expression)
  "Transform `markup-expression' into an equivalent, hopefuly readable, scheme expression.
For instance,
  \\markup \\bold \\italic hello
==>
  (markup #:line (#:bold (#:italic (#:simple \"hello\"))))"
  (define (proc->command-keyword proc)
    "Return a keyword, eg. `#:bold', from the `proc' function, eg. #<procedure bold-markup (layout props arg)>"
    (let ((cmd-markup (symbol->string (procedure-name proc))))
      (symbol->keyword (string->symbol (substring cmd-markup 0 (- (string-length cmd-markup)
								  (string-length "-markup")))))))
  (define (transform-arg arg)
    (cond ((and (pair? arg) (markup? (car arg))) ;; a markup list
	   (apply append (map inner-markup->make-markup arg)))
	  ((and (not (string? arg)) (markup? arg)) ;; a markup
	   (inner-markup->make-markup arg))
	  (else					 ;; scheme arg
	   arg)))
  (define (inner-markup->make-markup mrkup)
    (if (string? mrkup)
	`(#:simple ,mrkup)
	(let ((cmd (proc->command-keyword (car mrkup)))
	      (args (map transform-arg (cdr mrkup))))
	  `(,cmd ,@args))))
  ;; body:
  (if (string? markup-expression)
      markup-expression
      `(markup ,@(inner-markup->make-markup markup-expression))))

(define-public (music->make-music obj)
  "Generate a expression that, once evaluated, may return an object equivalent to `obj',
that is, for a music expression, a (make-music ...) form."
  (cond (;; markup expression
	 (markup? obj)
	 (markup-expression->make-markup obj))
	(;; music expression
	 (ly:music? obj)
	 `(make-music
	   ',(ly:music-property obj 'name)
	   ,@(apply append (map (lambda (prop)
                                  `(',(car prop)
				    ,(music->make-music (cdr prop))))
                                (remove (lambda (prop)
                                          (eqv? (car prop) 'origin))
                                        (ly:music-mutable-properties obj))))))
	(;; moment
	 (ly:moment? obj)
	 `(ly:make-moment ,(ly:moment-main-numerator obj)
			  ,(ly:moment-main-denominator obj)
			  ,(ly:moment-grace-numerator obj)
			  ,(ly:moment-grace-denominator obj)))
	(;; note duration
	 (ly:duration? obj)
	 `(ly:make-duration ,(ly:duration-log obj)
			    ,(ly:duration-dot-count obj)
			    ,(car (ly:duration-factor obj))
			    ,(cdr (ly:duration-factor obj))))
	(;; note pitch
	 (ly:pitch? obj)
	 `(ly:make-pitch ,(ly:pitch-octave obj)
			 ,(ly:pitch-notename obj)
			 ,(ly:pitch-alteration obj)))
	(;; scheme procedure
	 (procedure? obj)
	 (or (procedure-name obj) obj))
	(;; a symbol (avoid having an unquoted symbol)
	 (symbol? obj)
	 `',obj)
	(;; an empty list (avoid having an unquoted empty list)
	 (null? obj)
	 `'())
	(;; a proper list
	 (list? obj)
	 `(list ,@(map music->make-music obj)))
	(;; a pair
	 (pair? obj)
	 `(cons ,(music->make-music (car obj))
		,(music->make-music (cdr obj))))
	(else
	 obj)))

(use-modules (ice-9 pretty-print))
(define*-public (display-scheme-music obj #:optional (port (current-output-port)))
  "Displays `obj', typically a music expression, in a friendly fashion,
which often can be read back in order to generate an equivalent expression.

Returns `obj'.
"
  (pretty-print (music->make-music obj) port)
  (newline)
  obj)

;;;
;;; Scheme music expression --> Lily-syntax-using string translator
;;;
(use-modules (srfi srfi-39)
             (scm display-lily))

(define*-public (display-lily-music expr parser #:key force-duration)
  "Display the music expression using LilyPond syntax"
  (memoize-clef-names supported-clefs)
  (parameterize ((*indent* 0)
		 (*previous-duration* (ly:make-duration 2))
		 (*force-duration* force-duration))
    (display (music->lily-string expr parser))
    (newline)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (shift-one-duration-log music shift dot)
  "  add SHIFT to duration-log of 'duration in music and optionally
  a dot to any note encountered. This scales the music up by a factor
  2^shift * (2 - (1/2)^dot)"
  (let ((d (ly:music-property music 'duration)))
    (if (ly:duration? d)
	(let* ((cp (ly:duration-factor d))
	       (nd (ly:make-duration (+ shift (ly:duration-log d))
				     (+ dot (ly:duration-dot-count d))
				     (car cp)
				     (cdr cp))))
	  (set! (ly:music-property music 'duration) nd)))
    music))

(define-public (shift-duration-log music shift dot)
  (music-map (lambda (x) (shift-one-duration-log x shift dot))
	     music))

(define-public (make-repeat name times main alts)
  "create a repeat music expression, with all properties initialized properly"
  (define (first-note-duration music)
    "Finds the duration of the first NoteEvent by searching depth-first
through MUSIC."
    (if (memq 'note-event (ly:music-property music 'types))
	(ly:music-property music 'duration)
	(let loop ((elts (if (ly:music? (ly:music-property music 'element))
			     (list (ly:music-property music 'element))
			     (ly:music-property music 'elements))))
	  (and (pair? elts)
	       (let ((dur (first-note-duration (car elts))))
		 (if (ly:duration? dur)
		     dur
		     (loop (cdr elts))))))))

  (let ((talts (if (< times (length alts))
		   (begin
		     (ly:warning (_ "More alternatives than repeats.  Junking excess alternatives"))
		     (take alts times))
		   alts))
	(r (make-repeated-music name)))
    (set! (ly:music-property r 'element) main)
    (set! (ly:music-property r 'repeat-count) (max times 1))
    (set! (ly:music-property r 'elements) talts)
    (if (equal? name "tremolo")
	(let* ((dots (1- (logcount times)))
	       (mult (/ (* times (ash 1 dots)) (1- (ash 2 dots))))
	       (shift (- (ly:intlog2 (floor mult))))
	       (note-duration (first-note-duration r))
	       (duration-log (if (ly:duration? note-duration)
				 (ly:duration-log note-duration)
				 1))
	       (tremolo-type (ash 1 duration-log)))
	  (set! (ly:music-property r 'tremolo-type) tremolo-type)
	  (if (not (integer?  mult))
              (ly:warning (_ "invalid tremolo repeat count: ~a") times))
	  (if (memq 'sequential-music (ly:music-property main 'types))
	      ;; \repeat "tremolo" { c4 d4 }
	      (let ((children (length (ly:music-property main 'elements))))

		;; fixme: should be more generic.
		(if (and (not (= children 2))
			 (not (= children 1)))
		    (ly:warning (_ "expecting 2 elements for chord tremolo, found ~a") children))
		(ly:music-compress r (ly:make-moment 1 children))
		(shift-duration-log r
				    (if (= children 2)  (1- shift) shift)
				    dots))
	      ;; \repeat "tremolo" c4
	      (shift-duration-log r shift dots)))
	r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clusters.

(define-public (note-to-cluster music)
  "Replace NoteEvents by ClusterNoteEvents."
  (if (eq? (ly:music-property music 'name) 'NoteEvent)
      (make-music 'ClusterNoteEvent
		  'pitch (ly:music-property music 'pitch)
		  'duration (ly:music-property music 'duration))
      music))

(define-public (notes-to-clusters music)
  (music-map note-to-cluster music))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; repeats.

(define-public (unfold-repeats music)
  "
This function replaces all repeats  with unfold repeats. "

  (let ((es (ly:music-property music 'elements))
	(e  (ly:music-property music 'element))
	)
    (if (memq 'repeated-music (ly:music-property music 'types))
	(let*
	    ((props (ly:music-mutable-properties music))
	     (old-name (ly:music-property music 'name))
	     (flattened  (flatten-alist props)))

	  (set! music (apply make-music (cons 'UnfoldedRepeatedMusic
					      flattened)))

	  (if (equal? old-name 'TremoloRepeatedMusic)
	      (let* ((seq-arg? (memq 'sequential-music
				     (ly:music-property e 'types)))
		     (count  (ly:music-property music 'repeat-count))
		     (dot-shift (if (= 0 (remainder count 3))
				    -1 0)))

		(if (= 0 -1)
		    (set! count (* 2 (quotient count 3))))

		(shift-duration-log music (+ (if seq-arg? 1 0)
					     (ly:intlog2 count)) dot-shift)

		(if seq-arg?
		    (ly:music-compress e (ly:make-moment (length (ly:music-property
								  e 'elements)) 1)))))))


    (if (pair? es)
	(set! (ly:music-property music 'elements)
	      (map unfold-repeats es)))
    (if (ly:music? e)
	(set! (ly:music-property music 'element)
	      (unfold-repeats e)))
    music))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; property setting music objs.

(define-public (make-grob-property-set grob gprop val)
  "Make a Music expression that sets GPROP to VAL in GROB. Does a pop first,
i.e.  this is not an override"
  (make-music 'OverrideProperty
	      'symbol grob
	      'grob-property gprop
	      'grob-value val
	      'pop-first #t))

(define-public (make-grob-property-override grob gprop val)
  "Make a Music expression that overrides GPROP to VAL in GROB."
  (make-music 'OverrideProperty
	      'symbol grob
	      'grob-property gprop
	      'grob-value val))

(define-public (make-grob-property-revert grob gprop)
  "Revert the grob property GPROP for GROB."
  (make-music 'RevertProperty
	      'symbol grob
	      'grob-property gprop))

(define direction-polyphonic-grobs
  '(DotColumn
    Dots
    Fingering
    LaissezVibrerTie
    PhrasingSlur
    RepeatTie
    Rest
    Script
    Slur
    Stem
    TextScript
    Tie))

(define-safe-public (make-voice-props-set n)
  (make-sequential-music
   (append
    (map (lambda (x) (make-grob-property-set x 'direction
					     (if (odd? n) -1 1)))
	 direction-polyphonic-grobs)
    (list
     (make-property-set 'graceSettings
			;; TODO: take this from voicedGraceSettings or similar.
			'((Voice Stem font-size -3)
			  (Voice NoteHead font-size -3)
			  (Voice TabNoteHead font-size -4)
			  (Voice Dots font-size -3)
			  (Voice Stem length-fraction 0.8)
			  (Voice Stem no-stem-extend #t)
			  (Voice Beam beam-thickness 0.384)
			  (Voice Beam length-fraction 0.8)
			  (Voice Accidental font-size -4)
			  (Voice AccidentalCautionary font-size -4)
			  (Voice Script font-size -3)
			  (Voice Fingering font-size -8)
			  (Voice StringNumber font-size -8)))

     (make-grob-property-set 'NoteColumn 'horizontal-shift (quotient n 2))
     (make-grob-property-set 'MultiMeasureRest 'staff-position (if (odd? n) -4 4))))))

(define-safe-public (make-voice-props-revert)
  (make-sequential-music
   (append
    (map (lambda (x) (make-grob-property-revert x 'direction))
	 direction-polyphonic-grobs)
    (list (make-property-unset 'graceSettings)
	  (make-grob-property-revert 'NoteColumn 'horizontal-shift)
	  (make-grob-property-revert 'MultiMeasureRest 'staff-position)))))


(define-safe-public (context-spec-music m context #:optional id)
  "Add \\context CONTEXT = ID to M. "
  (let ((cm (make-music 'ContextSpeccedMusic
			'element m
			'context-type context)))
    (if (string? id)
	(set! (ly:music-property cm 'context-id) id))
    cm))

(define-public (descend-to-context m context)
  "Like context-spec-music, but only descending. "
  (let ((cm (context-spec-music m context)))
    (ly:music-set-property! cm 'descend-only #t)
    cm))

(define-public (make-non-relative-music mus)
  (make-music 'UnrelativableMusic
	      'element mus))

(define-public (make-apply-context func)
  (make-music 'ApplyContext
	      'procedure func))

(define-public (make-sequential-music elts)
  (make-music 'SequentialMusic
	      'elements elts))

(define-public (make-simultaneous-music elts)
  (make-music 'SimultaneousMusic
	      'elements elts))

(define-safe-public (make-event-chord elts)
  (make-music 'EventChord
	      'elements elts))

(define-public (make-skip-music dur)
  (make-music 'SkipMusic
	      'duration dur))

(define-public (make-grace-music music)
  (make-music 'GraceMusic
	      'element music))

;;;;;;;;;;;;;;;;

;; mmrest
(define-public (make-multi-measure-rest duration location)
  (make-music 'MultiMeasureRestMusic
	      'origin location
	      'duration duration))

(define-public (make-property-set sym val)
  (make-music 'PropertySet
	      'symbol sym
	      'value val))

(define-public (make-property-unset sym)
  (make-music 'PropertyUnset
	      'symbol sym))

(define-public (make-ottava-set octavation)
  (let ((m (make-music 'ApplyContext)))
    (define (ottava-modify context)
      "Either reset middleCPosition to the stored original, or remember
old middleCPosition, add OCTAVATION to middleCPosition, and set
OTTAVATION to `8va', or whatever appropriate."
      (if (number? (ly:context-property	 context 'middleCOffset))
	  (let ((where (ly:context-property-where-defined context 'middleCOffset)))
	    (ly:context-unset-property where 'middleCOffset)
	    (ly:context-unset-property where 'ottavation)))

      (let* ((offset (* -7 octavation))
	     (string (assoc-get octavation '((2 . "15ma")
					     (1 . "8va")
					     (0 . #f)
					     (-1 . "8vb")
					     (-2 . "15mb")))))
	(ly:context-set-property! context 'middleCOffset offset)
	(ly:context-set-property! context 'ottavation string)
	(ly:set-middle-C! context)))
    (set! (ly:music-property m 'procedure) ottava-modify)
    (context-spec-music m 'Staff)))

(define-public (set-octavation ottavation)
  (ly:export (make-ottava-set ottavation)))

;;; Need to keep this definition for \time calls from parser
(define-public (make-time-signature-set num den)
  "Set properties for time signature NUM/DEN."
  (make-beam-rule-time-signature-set num den '()))

;;; Used for calls that include beat-grouping setting
(define-public (set-time-signature num den . rest)
  "Set properties for time signature @var{num/den}.
If @var{rest} is present, it is used to make a default
@code{beamSetting} rule."
 (ly:export (apply make-beam-rule-time-signature-set
                    (list num den rest))))

(define-public (make-beam-rule-time-signature-set num den rest)
  "Implement settings for new time signature.  Can be
called from either make-time-signature-set (used by \time
in parser) or set-time-signature (called from scheme code
included in .ly file."

  (define (make-default-beaming-rule context)
   (override-property-setting
    context
    'beamSettings
    (list (cons num den) 'end)
    (list (cons '* (car rest)))))

  (let* ((set1 (make-property-set 'timeSignatureFraction (cons num den)))
	 (beat (ly:make-moment 1 den))
	 (len  (ly:make-moment num den))
	 (set2 (make-property-set 'beatLength beat))
	 (set3 (make-property-set 'measureLength len))
         (beaming-rule
          (if (null? rest)
              '()
              (list (make-apply-context make-default-beaming-rule))))
         (output (cons* set1 set2 set3 beaming-rule)))
    (descend-to-context
     (context-spec-music
      (make-sequential-music output)
       'Timing)
     'Score)))

(define-public (make-mark-set label)
  "Make the music for the \\mark command."
  (let* ((set (if (integer? label)
		  (context-spec-music (make-property-set 'rehearsalMark label)
				      'Score)
		  #f))
	 (ev (make-music 'MarkEvent))
	 (ch (make-event-chord (list ev))))
    (if set
	(make-sequential-music (list set ch))
	(begin
	  (set! (ly:music-property ev 'label) label)
	  ch))))

(define-safe-public (make-articulation name)
  (make-music 'ArticulationEvent
	      'articulation-type name))

(define-public (make-lyric-event string duration)
  (make-music 'LyricEvent
	      'duration duration
	      'text string))

(define-safe-public (make-span-event type span-dir)
  (make-music type
	      'span-direction span-dir))

(define-public (override-head-style heads style)
  "Override style for @var{heads} to @var{style}."
  (make-sequential-music
    (if (pair? heads)
        (map (lambda (h)
              (make-grob-property-override h 'style style))
         heads)
        (list (make-grob-property-override heads 'style style)))))

(define-public (revert-head-style heads)
  "Revert style for @var{heads}."
  (make-sequential-music
    (if (pair? heads)
        (map (lambda (h)
              (make-grob-property-revert h 'style))
         heads)
        (list (make-grob-property-revert heads 'style)))))

(define-public (style-note-heads heads style music)
 "Set @var{style} for all @var{heads} in @var{music}.  Works both
inside of and outside of chord construct."
  ;; are we inside a <...>?
  (if (eq? (ly:music-property music 'name) 'NoteEvent)
      ;; yes -> use a tweak
      (begin
        (set! (ly:music-property music 'tweaks)
              (acons 'style style (ly:music-property music 'tweaks)))
        music)
      ;; not in <...>, so use overrides
      (make-sequential-music
        (list
          (override-head-style heads style)
          music
          (revert-head-style heads)))))

 (define-public (set-mus-properties! m alist)
  "Set all of ALIST as properties of M."
  (if (pair? alist)
      (begin
	(set! (ly:music-property m (caar alist)) (cdar alist))
	(set-mus-properties! m (cdr alist)))))

(define-public (music-separator? m)
  "Is M a separator?"
  (let ((ts (ly:music-property m 'types)))
    (memq 'separator ts)))

;;; splitting chords into voices.
(define (voicify-list lst number)
  "Make a list of Musics.

   voicify-list :: [ [Music ] ] -> number -> [Music]
   LST is a list music-lists.

   NUMBER is 0-base, i.e. Voice=1 (upstems) has number 0.
"
  (if (null? lst)
      '()
      (cons (context-spec-music
	     (make-sequential-music
	      (list (make-voice-props-set number)
		    (make-simultaneous-music (car lst))))
	     'Bottom  (number->string (1+ number)))
	    (voicify-list (cdr lst) (1+ number)))))

(define (voicify-chord ch)
  "Split the parts of a chord into different Voices using separator"
  (let ((es (ly:music-property ch 'elements)))
    (set! (ly:music-property  ch 'elements)
	  (voicify-list (split-list-by-separator es music-separator?) 0))
    ch))

(define-public (voicify-music m)
  "Recursively split chords that are separated with \\ "
  (if (not (ly:music? m))
      (ly:error (_ "music expected: ~S") m))
  (let ((es (ly:music-property m 'elements))
	(e (ly:music-property m 'element)))

    (if (pair? es)
	(set! (ly:music-property m 'elements) (map voicify-music es)))
    (if (ly:music? e)
	(set! (ly:music-property m 'element)  (voicify-music e)))
    (if (and (equal? (ly:music-property m 'name) 'SimultaneousMusic)
	     (reduce (lambda (x y ) (or x y)) #f (map music-separator? es)))
	(set! m (context-spec-music (voicify-chord m) 'Staff)))
    m))

(define-public (empty-music)
  (ly:export (make-music 'Music)))

;; Make a function that checks score element for being of a specific type.
(define-public (make-type-checker symbol)
  (lambda (elt)
    (not (eq? #f (memq symbol (ly:grob-property elt 'interfaces))))))

(define-public ((outputproperty-compatibility func sym val) grob g-context ao-context)
  (if (func grob)
      (set! (ly:grob-property grob sym) val)))


(define-public ((set-output-property grob-name symbol val)  grob grob-c context)
  "Usage:

\\applyoutput #(set-output-property 'Clef 'extra-offset '(0 . 1))

"
  (let ((meta (ly:grob-property grob 'meta)))
    (if (equal? (assoc-get 'name meta) grob-name)
	(set! (ly:grob-property grob symbol) val))))


;;
(define-public (smart-bar-check n)
  "Make	 a bar check that checks for a specific bar number.
"
  (let ((m (make-music 'ApplyContext)))
    (define (checker tr)
      (let* ((bn (ly:context-property tr 'currentBarNumber)))
	(if (= bn n)
	    #t
	    (ly:error
	     ;; FIXME: uncomprehensable message
	     (_ "Bar check failed.  Expect to be at ~a, instead at ~a")
	     n bn))))
    (set! (ly:music-property m 'procedure) checker)
    m))


(define-public (skip->rest mus)

  "Replace MUS by RestEvent of the same duration if it is a
SkipEvent. Useful for extracting parts from crowded scores"

  (if  (memq (ly:music-property mus 'name) '(SkipEvent SkipMusic))
   (make-music 'RestEvent 'duration (ly:music-property mus 'duration))
   mus))


(define-public (music-has-type music type)
  (memq type (ly:music-property music 'types)))

(define-public (music-clone music)
  (define (alist->args alist acc)
    (if (null? alist)
	acc
	(alist->args (cdr alist)
		     (cons (caar alist) (cons (cdar alist) acc)))))

  (apply
   make-music
   (ly:music-property music 'name)
   (alist->args (ly:music-mutable-properties music) '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; warn for bare chords at start.


(define-public (ly:music-message music msg)
  (let ((ip (ly:music-property music 'origin)))
    (if (ly:input-location? ip)
	(ly:input-message ip msg)
	(ly:warning msg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; setting stuff for grace context.
;;

(define (vector-extend v x)
  "Make a new vector consisting of V, with X added to the end."
  (let* ((n (vector-length v))
	 (nv (make-vector (+ n 1) '())))
    (vector-move-left! v 0 n nv 0)
    (vector-set! nv n x)
    nv))

(define (vector-map f v)
  "Map	F over V. This function returns nothing."
  (do ((n (vector-length v))
       (i 0 (+ i 1)))
      ((>= i n))
    (f (vector-ref v i))))

(define (vector-reverse-map f v)
  "Map	F over V, N to 0 order. This function returns nothing."
  (do ((i (- (vector-length v) 1) (- i 1)))
      ((< i 0))
    (f (vector-ref v i))))

(define-public (add-grace-property context-name grob sym val)
  "Set SYM=VAL for GROB in CONTEXT-NAME. "
  (define (set-prop context)
    (let* ((where (ly:context-property-where-defined context 'graceSettings))
	   (current (ly:context-property where 'graceSettings))
	   (new-settings (append current
				 (list (list context-name grob sym val)))))
      (ly:context-set-property! where 'graceSettings new-settings)))
  (ly:export (context-spec-music (make-apply-context set-prop) 'Voice)))

(define-public (remove-grace-property context-name grob sym)
  "Remove all SYM for GROB in CONTEXT-NAME. "
  (define (sym-grob-context? property sym grob context-name)
    (and (eq? (car property) context-name)
         (eq? (cadr property) grob)
         (eq? (caddr property) sym)))
  (define (delete-prop context)
    (let* ((where (ly:context-property-where-defined context 'graceSettings))
	   (current (ly:context-property where 'graceSettings))
           (prop-settings (filter
                            (lambda(x) (sym-grob-context? x sym grob context-name))
                            current))
	   (new-settings current))
      (for-each (lambda(x)
                 (set! new-settings (delete x new-settings)))
               prop-settings)
      (ly:context-set-property! where 'graceSettings new-settings)))
  (ly:export (context-spec-music (make-apply-context delete-prop) 'Voice)))



(defmacro-public def-grace-function (start stop . docstring)
  "Helper macro for defining grace music"
  `(define-music-function (parser location music) (ly:music?)
     ,@docstring
     (make-music 'GraceMusic
		 'origin location
		 'element (make-music 'SequentialMusic
				      'elements (list (ly:music-deep-copy ,start)
						      music
						      (ly:music-deep-copy ,stop))))))

(defmacro-public define-music-function (args signature . body)
  "Helper macro for `ly:make-music-function'.
Syntax:
  (define-music-function (parser location arg1 arg2 ...) (arg1-type? arg2-type? ...)
    ...function body...)
"
(if (and (pair? body) (pair? (car body)) (eqv? '_i (caar body)))
      ;; When the music function definition contains a i10n doc string,
      ;; (_i "doc string"), keep the literal string only
      (let ((docstring (cadar body))
	    (body (cdr body)))
	`(ly:make-music-function (list ,@signature)
				 (lambda (,@args)
				   ,docstring
				   ,@body)))
      `(ly:make-music-function (list ,@signature)
			       (lambda (,@args)
				 ,@body))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (cue-substitute quote-music)
  "Must happen after quote-substitute."

  (if (vector? (ly:music-property quote-music 'quoted-events))
      (let* ((dir (ly:music-property quote-music 'quoted-voice-direction))
	     (main-voice (if (eq? 1 dir) 1 0))
	     (cue-voice (if (eq? 1 dir) 0 1))
	     (main-music (ly:music-property quote-music 'element))
	     (return-value quote-music))

	(if (or (eq? 1 dir) (eq? -1 dir))

	    ;; if we have stem dirs, change both quoted and main music
	    ;; to have opposite stems.
	    (begin
	      (set! return-value

		    ;; cannot context-spec Quote-music, since context
		    ;; for the quotes is determined in the iterator.
		    (make-sequential-music
		     (list
		      (context-spec-music (make-voice-props-set cue-voice) 'CueVoice "cue")
		      quote-music
		      (context-spec-music (make-voice-props-revert)  'CueVoice "cue"))))
	      (set! main-music
		    (make-sequential-music
		     (list
		      (make-voice-props-set main-voice)
		      main-music
		      (make-voice-props-revert))))
	      (set! (ly:music-property quote-music 'element) main-music)))

	return-value)
      quote-music))

(define-public ((quote-substitute quote-tab) music)
  (let* ((quoted-name (ly:music-property music 'quoted-music-name))
	 (quoted-vector (if (string? quoted-name)
			    (hash-ref quote-tab quoted-name #f)
			    #f)))


    (if (string? quoted-name)
	(if (vector? quoted-vector)
	    (begin
	      (set! (ly:music-property music 'quoted-events) quoted-vector)
	      (set! (ly:music-property music 'iterator-ctor)
		    ly:quote-iterator::constructor))
	    (ly:warning (_ "cannot find quoted music: `~S'") quoted-name)))
    music))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; switch it on here, so parsing and init isn't checked (too slow!)
;;
;; automatic music transformations.

(define (switch-on-debugging m)
  (if (defined? 'set-debug-cell-accesses!)
      (set-debug-cell-accesses! 15000))
  m)

(define (music-check-error music)
  (define found #f)
  (define (signal m)
    (if (and (ly:music? m)
	     (eq? (ly:music-property m 'error-found) #t))
	(set! found #t)))

  (for-each signal (ly:music-property music 'elements))
  (signal (ly:music-property music 'element))

  (if found
      (set! (ly:music-property music 'error-found) #t))
  music)

(define (precompute-music-length music)
  (set! (ly:music-property music 'length)
	(ly:music-length music))
  music)

(define-public (make-duration-of-length moment)
 "Make duration of the given MOMENT length."
 (ly:make-duration 0 0
  (ly:moment-main-numerator moment)
  (ly:moment-main-denominator moment)))

(define (skip-this moment)
 "set skipTypesetting, make SkipMusic of the given MOMENT length,
 and then unset skipTypesetting."
 (make-sequential-music
  (list
   (context-spec-music (make-property-set 'skipTypesetting #t)
    'Score)
   (make-music 'SkipMusic 'duration
    (make-duration-of-length moment))
   (context-spec-music (make-property-set 'skipTypesetting #f)
    'Score))))

(define (unskip-this moment)
 "unset skipTypesetting, make SkipMusic of the given MOMENT length,
 and then set skipTypesetting."
 (make-sequential-music
  (list
   (context-spec-music (make-property-set 'skipTypesetting #f)
    'Score)
   (make-music 'SkipMusic 'duration
    (make-duration-of-length moment))
   (context-spec-music (make-property-set 'skipTypesetting #t)
    'Score))))

(define (skip-as-needed music parser)
 "Replace MUSIC by
 << {  \\set skipTypesetting = ##f
 LENGTHOF(\\showFirstLength)
 \\set skipTypesetting = ##t
 LENGTHOF(\\showLastLength) }
 MUSIC >>
 if appropriate.

 When only showFirstLength is set,
 the 'length property of the music is
 overridden to speed up compiling."
 (let*
  ((show-last (ly:parser-lookup parser 'showLastLength))
   (show-first (ly:parser-lookup parser 'showFirstLength)))
  (cond

   ;; both properties may be set.
   ((and (ly:music? show-first) (ly:music? show-last))
    (let*
     ((orig-length (ly:music-length music))
      (skip-length (ly:moment-sub orig-length (ly:music-length show-last)))
      (begin-length (ly:music-length show-first)))
     (make-simultaneous-music
      (list
       (make-sequential-music
        (list
         (skip-this skip-length)
         ;; let's draw a separator between the beginning and the end
         (context-spec-music (make-property-set 'whichBar "||")
          'Timing)))
       (unskip-this begin-length)
       music))))

   ;; we may only want to print the last length
   ((ly:music? show-last)
    (let*
     ((orig-length (ly:music-length music))
      (skip-length (ly:moment-sub orig-length (ly:music-length show-last))))
     (make-simultaneous-music
      (list
       (skip-this skip-length)
       music))))

   ;; we may only want to print the beginning; in this case
   ;; only the first length will be processed (much faster).
   ((ly:music? show-first)
    (let*
     ((orig-length (ly:music-length music))
      (begin-length (ly:music-length show-first)))
     ;; the first length must not exceed the original length.
     (if (ly:moment<? begin-length orig-length)
      (set! (ly:music-property music 'length)
       (ly:music-length show-first)))
     music))

   (else music))))


(define-public toplevel-music-functions
  (list
   (lambda (music parser) (voicify-music music))
   (lambda (x parser) (music-map music-check-error x))
   (lambda (x parser) (music-map precompute-music-length x))
   (lambda (music parser)

     (music-map (quote-substitute (ly:parser-lookup parser 'musicQuotes))  music))

   ;; switch-on-debugging
   (lambda (x parser) (music-map cue-substitute x))

   (lambda (x parser)
     (skip-as-needed x parser)
   )))

;;;;;;;;;;
;;; general purpose music functions

(define (shift-octave pitch octave-shift)
  (_i "Add @var{octave-shift} to the octave of @var{pitch}.")
  (ly:make-pitch
     (+ (ly:pitch-octave pitch) octave-shift)
     (ly:pitch-notename pitch)
     (ly:pitch-alteration pitch)))


;;;;;;;;;;;;;;;;;
;; lyrics

(define (apply-durations lyric-music durations)
  (define (apply-duration music)
    (if (and (not (equal? (ly:music-length music) ZERO-MOMENT))
	     (ly:duration?  (ly:music-property music 'duration)))
	(begin
	  (set! (ly:music-property music 'duration) (car durations))
	  (set! durations (cdr durations)))))

  (music-map apply-duration lyric-music))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; accidentals

(define (recent-enough? bar-number alteration-def laziness)
  (if (or (number? alteration-def)
	  (equal? laziness #t))
      #t
      (<= bar-number (+ (cadr alteration-def) laziness))))

(define (is-tied? alteration-def)
  (let* ((def (if (pair? alteration-def)
		 (car alteration-def)
		 alteration-def)))

    (if (equal? def 'tied) #t #f)))

(define (extract-alteration alteration-def)
  (cond ((number? alteration-def)
	 alteration-def)
	((pair? alteration-def)
	 (car alteration-def))
	(else 0)))

(define (check-pitch-against-signature context pitch barnum laziness octaveness)
  "Checks the need for an accidental and a @q{restore} accidental against
@code{localKeySignature}. The @var{laziness} is the number of measures
for which reminder accidentals are used (i.e., if @var{laziness} is zero,
only cancel accidentals in the same measure; if @var{laziness} is three,
we cancel accidentals up to three measures after they first appear.
@var{octaveness} is either @code{'same-octave} or @code{'any-octave} and
specifies whether accidentals should be canceled in different octaves."
  (let* ((ignore-octave (cond ((equal? octaveness 'any-octave) #t)
			      ((equal? octaveness 'same-octave) #f)
			      (else
			       (ly:warning (_ "Unknown octaveness type: ~S ") octaveness)
			       (ly:warning (_ "Defaulting to 'any-octave."))
			       #t)))
	 (key-sig (ly:context-property context 'keySignature))
	 (local-key-sig (ly:context-property context 'localKeySignature))
	 (notename (ly:pitch-notename pitch))
	 (octave (ly:pitch-octave pitch))
	 (pitch-handle (cons octave notename))
	 (need-restore #f)
	 (need-accidental #f)
	 (previous-alteration #f)
	 (from-other-octaves #f)
	 (from-same-octave (assoc-get pitch-handle local-key-sig))
	 (from-key-sig (assoc-get notename local-key-sig)))

    ;; If no key signature match is found from localKeySignature, we may have a custom
    ;; type with octave-specific entries of the form ((octave . pitch) alteration)
    ;; instead of (pitch . alteration).  Since this type cannot coexist with entries in
    ;; localKeySignature, try extracting from keySignature instead.
    (if (equal? from-key-sig #f)
	(set! from-key-sig (assoc-get pitch-handle key-sig)))

    ;; loop through localKeySignature to search for a notename match from other octaves
    (let loop ((l local-key-sig))
      (if (pair? l)
	  (let ((entry (car l)))
	    (if (and (pair? (car entry))
		     (= (cdar entry) notename))
		(set! from-other-octaves (cdr entry))
		(loop (cdr l))))))

    ;; find previous alteration-def for comparison with pitch
    (cond
     ;; from same octave?
     ((and (eq? ignore-octave #f)
	   (not (equal? from-same-octave #f))
	   (recent-enough? barnum from-same-octave laziness))
      (set! previous-alteration from-same-octave))

     ;; from any octave?
     ((and (eq? ignore-octave #t)
	   (not (equal? from-other-octaves #f))
	   (recent-enough? barnum from-other-octaves laziness))
      (set! previous-alteration from-other-octaves))

     ;; not recent enough, extract from key signature/local key signature
     ((not (equal? from-key-sig #f))
      (set! previous-alteration from-key-sig)))

    (if (is-tied? previous-alteration)
	(set! need-accidental #t)

	(let* ((prev-alt (extract-alteration previous-alteration))
	       (this-alt (ly:pitch-alteration pitch)))

	  (if (not (= this-alt prev-alt))
	      (begin
		(set! need-accidental #t)
		(if (and (not (= this-alt 0))
			 (or (< (abs this-alt) (abs prev-alt))
			     (< (* prev-alt this-alt) 0)))
		    (set! need-restore #t))))))

    (cons need-restore need-accidental)))

(define-public ((make-accidental-rule octaveness laziness) context pitch barnum measurepos)
  "Creates an accidental rule that makes its decision based on the octave of the note
  and a laziness value.
  octaveness is either 'same-octave or 'any-octave and defines whether the rule should
  respond to accidental changes in other octaves than the current. 'same-octave is the
  normal way to typeset accidentals - an accidental is made if the alteration is different
  from the last active pitch in the same octave. 'any-octave looks at the last active pitch
  in any octave.
  laziness states over how many bars an accidental should be remembered.
  0 is default - accidental lasts over 0 bar lines, that is, to the end of current measure.
  A positive integer means that the accidental lasts over that many bar lines.
  -1 is 'forget immediately', that is, only look at key signature.
  #t is forever."
  (check-pitch-against-signature context pitch barnum laziness octaveness))

(define (key-entry-notename entry)
  "Return the pitch of an entry in localKeySignature. The entry is either of the form
  '(notename . alter) or '((octave . notename) . (alter barnum . measurepos))."
  (if (number? (car entry))
      (car entry)
      (cdar entry)))

(define (key-entry-octave entry)
  "Return the octave of an entry in localKeySignature (or #f if the entry does not have
  an octave)."
  (and (pair? (car entry)) (caar entry)))

(define (key-entry-bar-number entry)
  "Return the bar number of an entry in localKeySignature (or #f if the entry does not
  have a bar number)."
  (and (pair? (car entry)) (caddr entry)))

(define (key-entry-measure-position entry)
  "Return the measure position of an entry in localKeySignature (or #f if the entry does
  not have a measure position)."
  (and (pair? (car entry)) (cdddr entry)))

(define (key-entry-alteration entry)
  "Return the alteration of an entry in localKeySignature."
  (if (number? (car entry))
      (cdr entry)
      (cadr entry)))

(define-public (find-pitch-entry keysig pitch accept-global accept-local)
  "Return the first entry in keysig that matches the pitch.
  accept-global states whether key signature entries should be included.
  accept-local states whether local accidentals should be included.
  if no matching entry is found, #f is returned."
  (if (pair? keysig)
      (let* ((entry (car keysig))
	     (entryoct (key-entry-octave entry))
	     (entrynn (key-entry-notename entry))
	     (oct (ly:pitch-octave pitch))
	     (nn (ly:pitch-notename pitch)))
	(if (and (equal? nn entrynn)
		 (or (and accept-global (equal? #f entryoct))
		     (and accept-local (equal? oct entryoct))))
	    entry
	    (find-pitch-entry (cdr keysig) pitch accept-global accept-local)))
      #f))

(define-public (neo-modern-accidental-rule context pitch barnum measurepos)
  "an accidental rule that typesets an accidental if it differs from the key signature
   AND does not directly follow a note on the same staff-line.
   This rule should not be used alone because it does neither look at bar lines
   nor different accidentals at the same notename"
  (let* ((keysig (ly:context-property context 'localKeySignature))
	 (entry (find-pitch-entry keysig pitch #t #t)))
    (if (equal? #f entry)
	(cons #f #f)
	(let* ((global-entry (find-pitch-entry keysig pitch #t #f))
	       (key-acc (if (equal? global-entry #f)
			    0
			    (key-entry-alteration global-entry)))
	       (acc (ly:pitch-alteration pitch))
	       (entrymp (key-entry-measure-position entry))
	       (entrybn (key-entry-bar-number entry)))
	  (cons #f (not (or (equal? acc key-acc)
			    (and (equal? entrybn barnum) (equal? entrymp measurepos)))))))))

(define-public (teaching-accidental-rule context pitch barnum measurepos)
  "an accidental rule that typesets a cautionary accidental
  if it is included in the key signature AND does not directly follow
  a note on the same staff-line."
  (let* ((keysig (ly:context-property context 'localKeySignature))
	 (entry (find-pitch-entry keysig pitch #t #t)))
    (if (equal? #f entry)
	(cons #f #f)
	(let* ((global-entry (find-pitch-entry keysig pitch #f #f))
	       (key-acc (if (equal? global-entry #f)
			    0
			    (key-entry-alteration global-entry)))
	       (acc (ly:pitch-alteration pitch))
	       (entrymp (key-entry-measure-position entry))
	       (entrybn (key-entry-bar-number entry)))
	  (cons #f (not (or (equal? acc key-acc)
			    (and (equal? entrybn barnum) (equal? entrymp measurepos)))))))))

(define-public (set-accidentals-properties extra-natural
					   auto-accs auto-cauts
					   context)
  (context-spec-music
   (make-sequential-music
    (append (if (boolean? extra-natural)
		(list (make-property-set 'extraNatural extra-natural))
		'())
	    (list (make-property-set 'autoAccidentals auto-accs)
		  (make-property-set 'autoCautionaries auto-cauts))))
   context))

(define-public (set-accidental-style style . rest)
  "Set accidental style to STYLE. Optionally takes a context argument,
e.g. 'Staff or 'Voice. The context defaults to Staff, except for piano styles, which
use GrandStaff as a context. "
  (let ((context (if (pair? rest)
		     (car rest) 'Staff))
	(pcontext (if (pair? rest)
		      (car rest) 'GrandStaff)))
    (ly:export
     (cond
      ;; accidentals as they were common in the 18th century.
      ((equal? style 'default)
       (set-accidentals-properties #t
				   `(Staff ,(make-accidental-rule 'same-octave 0))
				   '()
				   context))
      ;; accidentals from one voice do NOT get cancelled in other voices
      ((equal? style 'voice)
       (set-accidentals-properties #t
				   `(Voice ,(make-accidental-rule 'same-octave 0))
				   '()
				   context))
      ;; accidentals as suggested by Kurt Stone, Music Notation in the 20th century.
      ;; This includes all the default accidentals, but accidentals also needs cancelling
      ;; in other octaves and in the next measure.
      ((equal? style 'modern)
       (set-accidentals-properties #f
				   `(Staff ,(make-accidental-rule 'same-octave 0)
					   ,(make-accidental-rule 'any-octave 0)
					   ,(make-accidental-rule 'same-octave 1))
				   '()
				   context))
      ;; the accidentals that Stone adds to the old standard as cautionaries
      ((equal? style 'modern-cautionary)
       (set-accidentals-properties #f
				   `(Staff ,(make-accidental-rule 'same-octave 0))
				   `(Staff ,(make-accidental-rule 'any-octave 0)
					   ,(make-accidental-rule 'same-octave 1))
				   context))
      ;; same as modern, but accidentals different from the key signature are always
      ;; typeset - unless they directly follow a note of the same pitch.
      ((equal? style 'neo-modern)
       (set-accidentals-properties #f
				   `(Staff ,(make-accidental-rule 'same-octave 0)
					   ,(make-accidental-rule 'any-octave 0)
					   ,(make-accidental-rule 'same-octave 1)
				           ,neo-modern-accidental-rule)
				   '()
				   context))
      ((equal? style 'neo-modern-cautionary)
       (set-accidentals-properties #f
				   `(Staff ,(make-accidental-rule 'same-octave 0))
				   `(Staff ,(make-accidental-rule 'any-octave 0)
					   ,(make-accidental-rule 'same-octave 1)
				           ,neo-modern-accidental-rule)
				   context))
      ;; Accidentals as they were common in dodecaphonic music with no tonality.
      ;; Each note gets one accidental.
      ((equal? style 'dodecaphonic)
       (set-accidentals-properties #f
				   `(Staff ,(lambda (c p bn mp) '(#f . #t)))
				   '()
				   context))
      ;; Multivoice accidentals to be read both by musicians playing one voice
      ;; and musicians playing all voices.
      ;; Accidentals are typeset for each voice, but they ARE cancelled across voices.
      ((equal? style 'modern-voice)
       (set-accidentals-properties  #f
				    `(Voice ,(make-accidental-rule 'same-octave 0)
					    ,(make-accidental-rule 'any-octave 0)
					    ,(make-accidental-rule 'same-octave 1)
				      Staff ,(make-accidental-rule 'same-octave 0)
					    ,(make-accidental-rule 'any-octave 0)
					    ,(make-accidental-rule 'same-octave 1))
				    '()
				    context))
      ;; same as modernVoiceAccidental eccept that all special accidentals are typeset
      ;; as cautionaries
      ((equal? style 'modern-voice-cautionary)
       (set-accidentals-properties #f
				   `(Voice ,(make-accidental-rule 'same-octave 0))
				   `(Voice ,(make-accidental-rule 'any-octave 0)
					   ,(make-accidental-rule 'same-octave 1)
				     Staff ,(make-accidental-rule 'same-octave 0)
				           ,(make-accidental-rule 'any-octave 0)
					   ,(make-accidental-rule 'same-octave 1))
				   context))
      ;; stone's suggestions for accidentals on grand staff.
      ;; Accidentals are cancelled across the staves in the same grand staff as well
      ((equal? style 'piano)
       (set-accidentals-properties #f
				   `(Staff ,(make-accidental-rule 'same-octave 0)
					   ,(make-accidental-rule 'any-octave 0)
					   ,(make-accidental-rule 'same-octave 1)
				     GrandStaff
				           ,(make-accidental-rule 'any-octave 0)
					   ,(make-accidental-rule 'same-octave 1))
				   '()
				   pcontext))
      ((equal? style 'piano-cautionary)
       (set-accidentals-properties #f
				   `(Staff ,(make-accidental-rule 'same-octave 0))
				   `(Staff ,(make-accidental-rule 'any-octave 0)
					   ,(make-accidental-rule 'same-octave 1)
				     GrandStaff
				           ,(make-accidental-rule 'any-octave 0)
					   ,(make-accidental-rule 'same-octave 1))
				   pcontext))

      ;; same as modern, but cautionary accidentals are printed for all sharp or flat
      ;; tones specified by the key signature.
       ((equal? style 'teaching)
       (set-accidentals-properties #f
				    `(Staff ,(make-accidental-rule 'same-octave 0))
				    `(Staff ,(make-accidental-rule 'same-octave 1)
				           ,teaching-accidental-rule)
				   context))

      ;; do not set localKeySignature when a note alterated differently from
      ;; localKeySignature is found.
      ;; Causes accidentals to be printed at every note instead of
      ;; remembered for the duration of a measure.
      ;; accidentals not being remembered, causing accidentals always to
      ;; be typeset relative to the time signature
      ((equal? style 'forget)
       (set-accidentals-properties '()
				   `(Staff ,(make-accidental-rule 'same-octave -1))
				   '()
				   context))
      ;; Do not reset the key at the start of a measure.  Accidentals will be
      ;; printed only once and are in effect until overridden, possibly many
      ;; measures later.
      ((equal? style 'no-reset)
       (set-accidentals-properties '()
				   `(Staff ,(make-accidental-rule 'same-octave #t))
				   '()
				   context))
      (else
       (ly:warning (_ "unknown accidental style: ~S") style)
       (make-sequential-music '()))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (skip-of-length mus)
  "Create a skip of exactly the same length as MUS."
  (let* ((skip
	  (make-music
	   'SkipEvent
	   'duration (ly:make-duration 0 0))))

    (make-event-chord (list (ly:music-compress skip (ly:music-length mus))))))

(define-public (mmrest-of-length mus)
  "Create a mmrest of exactly the same length as MUS."

  (let* ((skip
	  (make-multi-measure-rest
	   (ly:make-duration 0 0) '())))
    (ly:music-compress skip (ly:music-length mus))
    skip))

(define-public (pitch-of-note event-chord)

  (let*
      ((evs (filter (lambda (x) (memq 'note-event (ly:music-property x 'types)))
		    (ly:music-property event-chord 'elements))))

    (if (pair? evs)
	(ly:music-property (car evs) 'pitch)
	#f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (extract-named-music music music-name)
"Return a flat list of all music named @code{music-name}
from @code{music}."
   (let ((extracted-list
          (if (ly:music? music)
              (if (eq? (ly:music-property music 'name) music-name)
                  (list music)
                  (let ((elt (ly:music-property music 'element))
                        (elts (ly:music-property music 'elements)))
                    (if (ly:music? elt)
                        (extract-named-music elt music-name)
                        (if (null? elts)
                            '()
                            (map (lambda(x)
                                    (extract-named-music x music-name ))
                             elts)))))
              '())))
     (flatten-list extracted-list)))

(define-public (event-chord-notes event-chord)
"Return a list of all notes from @{event-chord}."
  (filter
    (lambda (m) (eq? 'NoteEvent (ly:music-property m 'name)))
    (ly:music-property event-chord 'elements)))

(define-public (event-chord-pitches event-chord)
"Return a list of all pitches from @{event-chord}."
  (map (lambda (x) (ly:music-property x 'pitch))
       (event-chord-notes event-chord)))
