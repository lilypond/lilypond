;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 1998--2012 Jan Nieuwenhuizen <janneke@gnu.org>
;;;;                 Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;;
;;;; LilyPond is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; LilyPond is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.

;; for define-safe-public when byte-compiling using Guile V2
(use-modules (scm safe-utility-defs))

(use-modules (ice-9 optargs))
(use-modules (srfi srfi-11))

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

(define-public ly:grob-object
  (make-procedure-with-setter ly:grob-object
                              ly:grob-set-object!))

(define-public ly:grob-parent
  (make-procedure-with-setter ly:grob-parent
                              ly:grob-set-parent!))

(define-public ly:prob-property
  (make-procedure-with-setter ly:prob-property
                              ly:prob-set-property!))

(define-public ly:context-property
  (make-procedure-with-setter ly:context-property
                              ly:context-set-property!))

(define-public (music-map function music)
  "Apply @var{function} to @var{music} and all of the music it contains.

First it recurses over the children, then the function is applied to
@var{music}."
  (let ((es (ly:music-property music 'elements))
        (e (ly:music-property music 'element)))
    (if (pair? es)
        (set! (ly:music-property music 'elements)
              (map (lambda (y) (music-map function y)) es)))
    (if (ly:music? e)
        (set! (ly:music-property music 'element)
              (music-map function  e)))
    (function music)))

(define-public (music-filter pred? music)
  "Filter out music expressions that do not satisfy @var{pred?}."

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
      (if (not (null? e))
          (set! (ly:music-property music 'element) filtered-e))
      (if (not (null? es))
          (set! (ly:music-property music 'elements) filtered-es))
      (if (not (null? as))
          (set! (ly:music-property music 'articulations) filtered-as))
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
      (make-music 'Music)))       ;must return music.

(define*-public (display-music music #:optional (port (current-output-port)))
  "Display music, not done with @code{music-map} for clarity of
presentation."
  (display music port)
  (display ": { " port)
  (let ((es (ly:music-property music 'elements))
        (e (ly:music-property music 'element)))
    (display (ly:music-mutable-properties music) port)
    (if (pair? es)
        (begin (display "\nElements: {\n" port)
               (for-each (lambda (m) (display-music m port)) es)
               (display "}\n" port)))
    (if (ly:music? e)
        (begin
          (display "\nChild:" port)
          (display-music e port))))
  (display " }\n" port)
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
           (append-map inner-markup->make-markup arg))
          ((and (not (string? arg)) (markup? arg)) ;; a markup
           (inner-markup->make-markup arg))
          (else                                  ;; scheme arg
           (music->make-music arg))))
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
  "Generate an expression that, once evaluated, may return an object
equivalent to @var{obj}, that is, for a music expression, a
@code{(make-music ...)} form."
  (cond (;; markup expression
         (markup? obj)
         (markup-expression->make-markup obj))
        (;; music expression
         (ly:music? obj)
         `(make-music
           ',(ly:music-property obj 'name)
           ,@(append-map (lambda (prop)
                           `(',(car prop)
                             ,(music->make-music (cdr prop))))
                         (remove (lambda (prop)
                                   (eqv? (car prop) 'origin))
                                 (ly:music-mutable-properties obj)))))
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
                            ,(ly:duration-scale obj)))
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
which often can be read back in order to generate an equivalent expression."
  (pretty-print (music->make-music obj) port)
  (newline port))

;;;
;;; Scheme music expression --> Lily-syntax-using string translator
;;;
(use-modules (srfi srfi-39)
             (scm display-lily))

(define*-public (display-lily-music expr parser #:optional (port (current-output-port))
                                    #:key force-duration)
  "Display the music expression using LilyPond syntax"
  (memoize-clef-names supported-clefs)
  (parameterize ((*indent* 0)
                 (*previous-duration* (ly:make-duration 2))
                 (*force-duration* force-duration))
                (display (music->lily-string expr parser) port)
                (newline port)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (shift-one-duration-log music shift dot)
  "Add @var{shift} to @code{duration-log} of @code{'duration} in
@var{music} and optionally @var{dot} to any note encountered.
The number of dots in the shifted music may not be less than zero."
  (let ((d (ly:music-property music 'duration)))
    (if (ly:duration? d)
        (let* ((cp (ly:duration-scale d))
               (nd (ly:make-duration
                    (+ shift (ly:duration-log d))
                    (max 0 (+ dot (ly:duration-dot-count d)))
                    cp)))
          (set! (ly:music-property music 'duration) nd)))
    music))

(define-public (shift-duration-log music shift dot)
  (music-map (lambda (x) (shift-one-duration-log x shift dot))
             music))

(define-public (make-repeat name times main alts)
  "Create a repeat music expression, with all properties initialized
properly."
  (define (first-note-duration music)
    "Finds the duration of the first NoteEvent by searching depth-first
through MUSIC."
    ;; NoteEvent or a non-expanded chord-repetition
    ;; We just take anything that actually sports an announced duration.
    (if (ly:duration? (ly:music-property music 'duration))
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
    (if (and (equal? name "tremolo")
             (pair? (extract-named-music main '(EventChord NoteEvent))))
        ;; This works for single-note and multi-note tremolos!
        (let* ((children (if (music-is-of-type? main 'sequential-music)
                             ;; \repeat tremolo n { ... }
                             (length (extract-named-music main '(EventChord
                                                                 NoteEvent)))
                             ;; \repeat tremolo n c4
                             1))
               ;; # of dots is equal to the 1 in bitwise representation (minus 1)!
               (dots (1- (logcount (* times children))))
               ;; The remaining missing multiplicator to scale the notes by
               ;; times * children
               (mult (/ (* times children (ash 1 dots)) (1- (ash 2 dots))))
               (shift (- (ly:intlog2 (floor mult))))
               (note-duration (first-note-duration r))
               (duration-log (if (ly:duration? note-duration)
                                 (ly:duration-log note-duration)
                                 1))
               (tremolo-type (ash 1 duration-log)))
          (set! (ly:music-property r 'tremolo-type) tremolo-type)
          (if (not (and (integer? mult) (= (logcount mult) 1)))
              (ly:music-warning
               main
               (ly:format (_ "invalid tremolo repeat count: ~a") times)))
          ;; Adjust the time of the notes
          (ly:music-compress r (ly:make-moment 1 children))
          ;; Adjust the displayed note durations
          (shift-duration-log r shift dots))
        r)))

(define (calc-repeat-slash-count music)
  "Given the child-list @var{music} in @code{PercentRepeatMusic},
calculate the number of slashes based on the durations.  Returns @code{0}
if durations in @var{music} vary, allowing slash beats and double-percent
beats to be distinguished."
  (let* ((durs (map duration-of-note
                    (extract-named-music music '(EventChord NoteEvent
                                                            RestEvent SkipEvent))))
         (first-dur (car durs)))

    (if (every (lambda (d) (equal? d first-dur)) durs)
        (max (- (ly:duration-log first-dur) 2) 1)
        0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clusters.

(define-public (note-to-cluster music)
  "Replace @code{NoteEvents} by @code{ClusterNoteEvents}."
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
  "Replace all repeats with unfolded repeats."

  (let ((es (ly:music-property music 'elements))
        (e (ly:music-property music 'element)))

    (if (music-is-of-type? music 'repeated-music)
        (let* ((props (ly:music-mutable-properties music))
               (old-name (ly:music-property music 'name))
               (flattened (flatten-alist props)))
          (set! music (apply make-music (cons 'UnfoldedRepeatedMusic
                                              flattened)))

          (if (and (equal? old-name 'TremoloRepeatedMusic)
                   (pair? (extract-named-music e '(EventChord NoteEvent))))
              ;; This works for single-note and multi-note tremolos!
              (let* ((children (if (music-is-of-type? e 'sequential-music)
                                   ;; \repeat tremolo n { ... }
                                   (length (extract-named-music e '(EventChord
                                                                    NoteEvent)))
                                   ;; \repeat tremolo n c4
                                   1))
                     (times (ly:music-property music 'repeat-count))

                     ;; # of dots is equal to the 1 in bitwise representation (minus 1)!
                     (dots (1- (logcount (* times children))))
                     ;; The remaining missing multiplicator to scale the notes by
                     ;; times * children
                     (mult (/ (* times children (ash 1 dots)) (1- (ash 2 dots))))
                     (shift (- (ly:intlog2 (floor mult)))))

                ;; Adjust the time of the notes
                (ly:music-compress music (ly:make-moment children 1))
                ;; Adjust the displayed note durations
                (shift-duration-log music (- shift) (- dots))))))

    (if (pair? es)
        (set! (ly:music-property music 'elements)
              (map unfold-repeats es)))
    (if (ly:music? e)
        (set! (ly:music-property music 'element)
              (unfold-repeats e)))
    music))

(define-public (unfold-repeats-fully music)
  "Unfolds repeats and expands the resulting @code{unfolded-repeated-music}."
  (map-some-music
   (lambda (m)
     (and (music-is-of-type? m 'unfolded-repeated-music)
          (make-sequential-music
           (ly:music-deep-copy
            (let ((n (ly:music-property m 'repeat-count))
                  (alts (ly:music-property m 'elements))
                  (body (ly:music-property m 'element)))
              (cond ((<= n 0) '())
                    ((null? alts) (make-list n body))
                    (else
                     (concatenate
                      (zip (make-list n body)
                           (append! (make-list (max 0 (- n (length alts)))
                                               (car alts))
                                    alts))))))))))
   (unfold-repeats music)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; property setting music objs.

(define-safe-public (check-grob-path path #:optional parser location
                                     #:key
                                     (start 0)
                                     default
                                     (min 1)
                                     max)
  "Check a grob path specification @var{path}, a symbol list (or a
single symbol), for validity and possibly complete it.  Returns the
completed specification, or @code{#f} if invalid.  If optional
@var{parser} is given, a syntax error is raised in that case,
optionally using @var{location}.  If an optional keyword argument
@code{#:start @var{start}} is given, the parsing starts at the given
index in the sequence @samp{Context.Grob.property.sub-property...},
with the default of @samp{0} implying the full path.

If there is no valid first element of @var{path} fitting at the given
path location, an optionally given @code{#:default @var{default}} is
used as the respective element instead without checking it for
validity at this position.

The resulting path after possibly prepending @var{default} can be
constrained in length by optional arguments @code{#:min @var{min}} and
@code{#:max @var{max}}, defaulting to @samp{1} and unlimited,
respectively."
  (let ((path (if (symbol? path) (list path) path)))
    ;; A Guile 1.x bug specific to optargs precludes moving the
    ;; defines out of the let
    (define (unspecial? s)
      (not (or (object-property s 'is-grob?)
               (object-property s 'backend-type?))))
    (define (grob? s)
      (object-property s 'is-grob?))
    (define (property? s)
      (object-property s 'backend-type?))
    (define (check c p) (c p))

    (let* ((checkers
            (and (< start 3)
                 (drop (list unspecial? grob? property?) start)))
           (res
            (cond
             ((null? path)
              ;; tricky.  Should we make use of the default when the
              ;; list is empty?  In most cases, this question should be
              ;; academical as an empty list can only be generated by
              ;; Scheme and is likely an error.  We consider this a case
              ;; of "no valid first element, and default given".
              ;; Usually, invalid use cases should be caught later using
              ;; the #:min argument, and if the user explicitly does not
              ;; catch this, we just follow through.
              (if default (list default) '()))
             ((not checkers)
              ;; no checkers, so we have a valid first element and just
              ;; take the path as-is.
              path)
             (default
               (if ((car checkers) (car path))
                   (and (every check (cdr checkers) (cdr path))
                        path)
                   (and (every check (cdr checkers) path)
                        (cons default path))))
             (else
              (and (every check checkers path)
                   path)))))
      (if (and res
               (if max (<= min (length res) max)
                   (<= min (length res))))
          res
          (begin
            (if parser
                (ly:parser-error parser
                                 (format #f (_ "bad grob property path ~a")
                                         path)
                                 location))
            #f)))))

(define-public (make-grob-property-set grob gprop val)
  "Make a @code{Music} expression that sets @var{gprop} to @var{val} in
@var{grob}.  Does a pop first, i.e., this is not an override."
  (make-music 'OverrideProperty
              'symbol grob
              'grob-property gprop
              'grob-value val
              'pop-first #t))

(define-public (make-grob-property-override grob gprop val)
  "Make a @code{Music} expression that overrides @var{gprop} to @var{val}
in @var{grob}."
  (make-music 'OverrideProperty
              'symbol grob
              'grob-property gprop
              'grob-value val))

(define-public (make-grob-property-revert grob gprop)
  "Revert the grob property @var{gprop} for @var{grob}."
  (make-music 'RevertProperty
              'symbol grob
              'grob-property gprop))

(define direction-polyphonic-grobs
  '(AccidentalSuggestion
    DotColumn
    Dots
    Fingering
    LaissezVibrerTie
    LigatureBracket
    MultiMeasureRest
    PhrasingSlur
    RepeatTie
    Rest
    Script
    Slur
    Stem
    TextScript
    Tie
    TupletBracket
    TrillSpanner))

(define general-grace-settings
  `((Voice Stem font-size -3)
    (Voice Flag font-size -3)
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

(define-public score-grace-settings
  (append
    `((Voice Stem direction ,UP)
      (Voice Slur direction ,DOWN))
    general-grace-settings))

(define-safe-public (make-voice-props-set n)
  (make-sequential-music
   (append
    (map (lambda (x) (make-grob-property-set x 'direction
                                             (if (odd? n) -1 1)))
         direction-polyphonic-grobs)
    (list
     (make-property-set 'graceSettings general-grace-settings)
     (make-grob-property-set 'NoteColumn 'horizontal-shift (quotient n 2))))))

(define-safe-public (make-voice-props-override n)
  (make-sequential-music
   (append
    (map (lambda (x) (make-grob-property-override x 'direction
                                                  (if (odd? n) -1 1)))
         direction-polyphonic-grobs)
    (list
     (make-property-set 'graceSettings general-grace-settings)
     (make-grob-property-override 'NoteColumn 'horizontal-shift (quotient n 2))))))

(define-safe-public (make-voice-props-revert)
  (make-sequential-music
   (append
    (map (lambda (x) (make-grob-property-revert x 'direction))
         direction-polyphonic-grobs)
    (list (make-property-unset 'graceSettings)
          (make-grob-property-revert 'NoteColumn 'horizontal-shift)))))


(define-safe-public (context-spec-music m context #:optional id)
  "Add \\context CONTEXT = ID to M."
  (let ((cm (make-music 'ContextSpeccedMusic
                        'element m
                        'context-type context)))
    (if (string? id)
        (set! (ly:music-property cm 'context-id) id))
    cm))

(define-public (descend-to-context m context)
  "Like @code{context-spec-music}, but only descending."
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
  "Set all of @var{alist} as properties of @var{m}."
  (if (pair? alist)
      (begin
        (set! (ly:music-property m (caar alist)) (cdar alist))
        (set-mus-properties! m (cdr alist)))))

(define-public (music-separator? m)
  "Is @var{m} a separator?"
  (let ((ts (ly:music-property m 'types)))
    (memq 'separator ts)))

;;; expanding repeat chords
(define-public (copy-repeat-chord original-chord repeat-chord duration
                                  event-types)
  "Copies all events in @var{event-types} (be sure to include
@code{rhythmic-events}) from @var{original-chord} over to
@var{repeat-chord} with their articulations filtered as well.  Any
duration is replaced with the specified @var{duration}."
  ;; First remove everything from event-types that can already be
  ;; found in the repeated chord.  We don't need to look for
  ;; articulations on individual events since they can't actually get
  ;; into a repeat chord given its input syntax.

  (define (keep-element? m)
    (any (lambda (t) (music-is-of-type? m t))
         event-types))
  (define origin (ly:music-property repeat-chord 'origin #f))
  (define (set-origin! l)
    (if origin
        (for-each (lambda (m) (set! (ly:music-property m 'origin) origin)) l))
    l)

  (for-each
   (lambda (field)
     (for-each (lambda (e)
                 (for-each (lambda (x)
                             (set! event-types (delq x event-types)))
                           (ly:music-property e 'types)))
               (ly:music-property repeat-chord field)))
   '(elements articulations))

  ;; now treat the elements
  (set! (ly:music-property repeat-chord 'elements)
        (let ((elts
               (set-origin! (ly:music-deep-copy
                             (filter keep-element?
                                     (ly:music-property original-chord
                                                        'elements))))))
          (for-each
           (lambda (m)
             (let ((arts (ly:music-property m 'articulations)))
               (if (pair? arts)
                   (set! (ly:music-property m 'articulations)
                         (set-origin! (filter! keep-element? arts))))
               (if (ly:duration? (ly:music-property m 'duration))
                   (set! (ly:music-property m 'duration) duration))))
           elts)
          (append! elts (ly:music-property repeat-chord 'elements))))
  (let ((arts (filter keep-element?
                      (ly:music-property original-chord
                                         'articulations))))
    (if (pair? arts)
        (set! (ly:music-property repeat-chord 'articulations)
              (append!
               (set-origin! (ly:music-deep-copy arts))
               (ly:music-property repeat-chord 'articulations)))))
  repeat-chord)


(define-public (expand-repeat-chords! event-types music)
  "Walks through @var{music} and fills repeated chords (notable by
having a duration in @code{duration}) with the notes from their
respective predecessor chord."
  (let loop ((music music) (last-chord #f))
    (if (music-is-of-type? music 'event-chord)
        (let ((chord-repeat (ly:music-property music 'duration)))
          (cond
           ((not (ly:duration? chord-repeat))
            (if (any (lambda (m) (ly:duration?
                                  (ly:music-property m 'duration)))
                     (ly:music-property music 'elements))
                music
                last-chord))
           (last-chord
            (set! (ly:music-property music 'duration) '())
            (copy-repeat-chord last-chord music chord-repeat event-types))
           (else
            (ly:music-warning music (_ "Bad chord repetition"))
            #f)))
        (let ((elt (ly:music-property music 'element)))
          (fold loop (if (ly:music? elt) (loop elt last-chord) last-chord)
                (ly:music-property music 'elements)))))
  music)

;;; splitting chords into voices.
(define (voicify-list lst number)
  "Make a list of Musics.

voicify-list :: [ [Music ] ] -> number -> [Music]
LST is a list music-lists.

NUMBER is 0-base, i.e., Voice=1 (upstems) has number 0.
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
  "Recursively split chords that are separated with @code{\\\\}."
  (if (not (ly:music? m))
      (ly:error (_ "music expected: ~S") m))
  (let ((es (ly:music-property m 'elements))
        (e (ly:music-property m 'element)))

    (if (pair? es)
        (set! (ly:music-property m 'elements) (map voicify-music es)))
    (if (ly:music? e)
        (set! (ly:music-property m 'element)  (voicify-music e)))
    (if (and (equal? (ly:music-property m 'name) 'SimultaneousMusic)
             (any music-separator? es))
        (set! m (context-spec-music (voicify-chord m) 'Staff)))
    m))

(define-public (empty-music)
  (make-music 'Music))

;; Make a function that checks score element for being of a specific type.
(define-public (make-type-checker symbol)
  (lambda (elt)
    (grob::has-interface elt symbol)))

(define-public ((outputproperty-compatibility func sym val) grob g-context ao-context)
  (if (func grob)
      (set! (ly:grob-property grob sym) val)))


(define-public ((set-output-property grob-name symbol val)  grob grob-c context)
  "Usage example:
@code{\\applyoutput #(set-output-property 'Clef 'extra-offset '(0 . 1))}"
  (let ((meta (ly:grob-property grob 'meta)))
    (if (equal? (assoc-get 'name meta) grob-name)
        (set! (ly:grob-property grob symbol) val))))


(define-public (skip->rest mus)
  "Replace @var{mus} by @code{RestEvent} of the same duration if it is a
@code{SkipEvent}.  Useful for extracting parts from crowded scores."

  (if  (memq (ly:music-property mus 'name) '(SkipEvent SkipMusic))
       (make-music 'RestEvent 'duration (ly:music-property mus 'duration))
       mus))


(define-public (music-has-type music type)
  (memq type (ly:music-property music 'types)))

(define-public (music-clone music . music-properties)
  "Clone @var{music} and set properties according to
@var{music-properties}, a list of alternating property symbols and
values:
@example\n(music-clone start-span 'span-direction STOP)
@end example
Only properties that are not overriden by @var{music-properties} are
actually fully cloned."
  (let ((old-props (list-copy (ly:music-mutable-properties music)))
        (new-props '())
        (m (ly:make-music (ly:prob-immutable-properties music))))
    (define (set-props mus-props)
      (if (and (not (null? mus-props))
               (not (null? (cdr mus-props))))
          (begin
            (set! old-props (assq-remove! old-props (car mus-props)))
            (set! new-props
                  (assq-set! new-props
                             (car mus-props) (cadr mus-props)))
            (set-props (cddr mus-props)))))
    (set-props music-properties)
    (for-each
     (lambda (pair)
       (set! (ly:music-property m (car pair))
             (ly:music-deep-copy (cdr pair))))
     old-props)
    (for-each
     (lambda (pair)
       (set! (ly:music-property m (car pair)) (cdr pair)))
     new-props)
    m))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; warn for bare chords at start.

(define-public (ly:music-message music msg . rest)
  (let ((ip (ly:music-property music 'origin)))
    (if (ly:input-location? ip)
        (apply ly:input-message ip msg rest)
        (apply ly:message msg rest))))

(define-public (ly:music-warning music msg . rest)
  (let ((ip (ly:music-property music 'origin)))
    (if (ly:input-location? ip)
        (apply ly:input-warning ip msg rest)
        (apply ly:warning msg rest))))

(define-public (ly:event-warning event msg . rest)
  (let ((ip (ly:event-property event 'origin)))
    (if (ly:input-location? ip)
        (apply ly:input-warning ip msg rest)
        (apply ly:warning msg rest))))

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
  "Map F over V.  This function returns nothing."
  (do ((n (vector-length v))
       (i 0 (+ i 1)))
      ((>= i n))
    (f (vector-ref v i))))

(define (vector-reverse-map f v)
  "Map F over V, N to 0 order.  This function returns nothing."
  (do ((i (- (vector-length v) 1) (- i 1)))
      ((< i 0))
    (f (vector-ref v i))))

(define-public (add-grace-property context-name grob sym val)
  "Set @var{sym}=@var{val} for @var{grob} in @var{context-name}."
  (define (set-prop context)
    (let* ((where (or (ly:context-find context context-name) context))
           (current (ly:context-property where 'graceSettings))
           (new-settings (append current
                                 (list (list context-name grob sym val)))))
      (ly:context-set-property! where 'graceSettings new-settings)))
  (make-apply-context set-prop))

(define-public (remove-grace-property context-name grob sym)
  "Remove all @var{sym} for @var{grob} in @var{context-name}."
  (define (sym-grob-context? property sym grob context-name)
    (and (eq? (car property) context-name)
         (eq? (cadr property) grob)
         (eq? (caddr property) sym)))
  (define (delete-prop context)
    (let* ((where (or (ly:context-find context context-name) context))
           (current (ly:context-property where 'graceSettings))
           (prop-settings (filter
                           (lambda(x) (sym-grob-context? x sym grob context-name))
                           current))
           (new-settings current))
      (for-each (lambda(x)
                  (set! new-settings (delete x new-settings)))
                prop-settings)
      (ly:context-set-property! where 'graceSettings new-settings)))
  (make-apply-context delete-prop))


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

(defmacro-public define-syntax-function (type args signature . body)
  "Helper macro for `ly:make-music-function'.
Syntax:
  (define-syntax-function result-type? (parser location arg1 arg2 ...) (arg1-type arg2-type ...)
    ...function body...)

argX-type can take one of the forms @code{predicate?} for mandatory
arguments satisfying the predicate, @code{(predicate?)} for optional
parameters of that type defaulting to @code{#f}, @code{@w{(predicate?
value)}} for optional parameters with a specified default
value (evaluated at definition time).  An optional parameter can be
omitted in a call only when it can't get confused with a following
parameter of different type.

Predicates with syntactical significance are @code{ly:pitch?},
@code{ly:duration?}, @code{ly:music?}, @code{markup?}.  Other
predicates require the parameter to be entered as Scheme expression.

@code{result-type?} can specify a default in the same manner as
predicates, to be used in case of a type error in arguments or
result."

  (define (currying-lambda args doc-string? body)
    (if (and (pair? args)
             (pair? (car args)))
        (currying-lambda (car args) doc-string?
                         `((lambda ,(cdr args) ,@body)))
        (if doc-string?
            `(lambda ,args ,doc-string? ,@body)
            `(lambda ,args ,@body))))

  (set! signature (map (lambda (pred)
                         (if (pair? pred)
                             `(cons ,(car pred)
                                    ,(and (pair? (cdr pred)) (cadr pred)))
                             pred))
                       (cons type signature)))

  (let ((docstring
         (and (pair? body) (pair? (cdr body))
              (if (string? (car body))
                  (car body)
                  (and (pair? (car body))
                       (eq? '_i (caar body))
                       (pair? (cdar body))
                       (string? (cadar body))
                       (null? (cddar body))
                       (cadar body))))))
    ;; When the music function definition contains an i10n doc string,
    ;; (_i "doc string"), keep the literal string only
    `(ly:make-music-function
      (list ,@signature)
      ,(currying-lambda args docstring (if docstring (cdr body) body)))))

(defmacro-public define-music-function rest
  "Defining macro returning music functions.
Syntax:
  (define-music-function (parser location arg1 arg2 ...) (arg1-type? arg2-type? ...)
    ...function body...)

argX-type can take one of the forms @code{predicate?} for mandatory
arguments satisfying the predicate, @code{(predicate?)} for optional
parameters of that type defaulting to @code{#f}, @code{@w{(predicate?
value)}} for optional parameters with a specified default
value (evaluated at definition time).  An optional parameter can be
omitted in a call only when it can't get confused with a following
parameter of different type.

Predicates with syntactical significance are @code{ly:pitch?},
@code{ly:duration?}, @code{ly:music?}, @code{markup?}.  Other
predicates require the parameter to be entered as Scheme expression.

Must return a music expression.  The @code{origin} is automatically
set to the @code{location} parameter."

  `(define-syntax-function (ly:music? (make-music 'Music 'void #t)) ,@rest))


(defmacro-public define-scheme-function rest
  "Defining macro returning Scheme functions.
Syntax:
  (define-scheme-function (parser location arg1 arg2 ...) (arg1-type? arg2-type? ...)
    ...function body...)

argX-type can take one of the forms @code{predicate?} for mandatory
arguments satisfying the predicate, @code{(predicate?)} for optional
parameters of that type defaulting to @code{#f}, @code{@w{(predicate?
value)}} for optional parameters with a specified default
value (evaluated at definition time).  An optional parameter can be
omitted in a call only when it can't get confused with a following
parameter of different type.

Predicates with syntactical significance are @code{ly:pitch?},
@code{ly:duration?}, @code{ly:music?}, @code{markup?}.  Other
predicates require the parameter to be entered as Scheme expression.

Can return arbitrary expressions.  If a music expression is returned,
its @code{origin} is automatically set to the @code{location}
parameter."

  `(define-syntax-function scheme? ,@rest))

(defmacro-public define-void-function rest
  "This defines a Scheme function like @code{define-scheme-function} with
void return value (i.e., what most Guile functions with `unspecified'
value return).  Use this when defining functions for executing actions
rather than returning values, to keep Lilypond from trying to interpret
the return value."
  `(define-syntax-function (void? *unspecified*) ,@rest *unspecified*))

(defmacro-public define-event-function rest
  "Defining macro returning event functions.
Syntax:
  (define-event-function (parser location arg1 arg2 ...) (arg1-type? arg2-type? ...)
    ...function body...)

argX-type can take one of the forms @code{predicate?} for mandatory
arguments satisfying the predicate, @code{(predicate?)} for optional
parameters of that type defaulting to @code{#f}, @code{@w{(predicate?
value)}} for optional parameters with a specified default
value (evaluated at definition time).  An optional parameter can be
omitted in a call only when it can't get confused with a following
parameter of different type.

Predicates with syntactical significance are @code{ly:pitch?},
@code{ly:duration?}, @code{ly:music?}, @code{markup?}.  Other
predicates require the parameter to be entered as Scheme expression.

Must return an event expression.  The @code{origin} is automatically
set to the @code{location} parameter."

  `(define-syntax-function (ly:event? (make-music 'Event 'void #t)) ,@rest))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (cue-substitute quote-music)
  "Must happen after @code{quote-substitute}."

  (if (vector? (ly:music-property quote-music 'quoted-events))
      (let* ((dir (ly:music-property quote-music 'quoted-voice-direction))
             (clef (ly:music-property quote-music 'quoted-music-clef #f))
             (main-voice (case dir ((1) 1) ((-1) 0) (else #f)))
             (cue-voice (and main-voice (- 1 main-voice)))
             (cue-type (ly:music-property quote-music 'quoted-context-type #f))
             (cue-id (ly:music-property quote-music 'quoted-context-id))
             (main-music (ly:music-property quote-music 'element))
             (return-value quote-music))

        (if main-voice
            (set! (ly:music-property quote-music 'element)
                  (make-sequential-music
                   (list
                    (make-voice-props-override main-voice)
                    main-music
                    (make-voice-props-revert)))))

        ;; if we have stem dirs, change both quoted and main music
        ;; to have opposite stems.

        ;; cannot context-spec Quote-music, since context
        ;; for the quotes is determined in the iterator.

        (make-sequential-music
         (delq! #f
                (list
                 (and clef (make-cue-clef-set clef))
                 (and cue-type cue-voice
                      (context-spec-music
                       (make-voice-props-override cue-voice)
                       cue-type cue-id))
                 quote-music
                 (and cue-type cue-voice
                      (context-spec-music
                       (make-voice-props-revert)
                       cue-type cue-id))
                 (and clef (make-cue-clef-unset))))))
      quote-music))

(define-public ((quote-substitute quote-tab) music)
  (let* ((quoted-name (ly:music-property music 'quoted-music-name))
         (quoted-vector (and (string? quoted-name)
                             (hash-ref quote-tab quoted-name #f))))


    (if (string? quoted-name)
        (if (vector? quoted-vector)
            (begin
              (set! (ly:music-property music 'quoted-events) quoted-vector)
              (set! (ly:music-property music 'iterator-ctor)
                    ly:quote-iterator::constructor))
            (ly:music-warning music (ly:format (_ "cannot find quoted music: `~S'") quoted-name))))
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
  "Make duration of the given @code{moment} length."
  (ly:make-duration 0 0
                    (ly:moment-main-numerator moment)
                    (ly:moment-main-denominator moment)))

(define (make-skipped moment bool)
  "Depending on BOOL, set or unset skipTypesetting,
then make SkipMusic of the given MOMENT length, and
then revert skipTypesetting."
  (make-sequential-music
   (list
    (context-spec-music (make-property-set 'skipTypesetting bool)
                        'Score)
    (make-music 'SkipMusic 'duration
                (make-duration-of-length moment))
    (context-spec-music (make-property-set 'skipTypesetting (not bool))
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
       (show-first (ly:parser-lookup parser 'showFirstLength))
       (show-last-length (and (ly:music? show-last)
                              (ly:music-length show-last)))
       (show-first-length (and (ly:music? show-first)
                               (ly:music-length show-first)))
       (orig-length (ly:music-length music)))

    ;;FIXME: if using either showFirst- or showLastLength,
    ;; make sure that skipBars is not set.

    (cond

     ;; both properties may be set.
     ((and show-first-length show-last-length)
      (let
          ((skip-length (ly:moment-sub orig-length show-last-length)))
        (make-simultaneous-music
         (list
          (make-sequential-music
           (list
            (make-skipped skip-length #t)
            ;; let's draw a separator between the beginning and the end
            (context-spec-music (make-property-set 'whichBar "||")
                                'Timing)))
          (make-skipped show-first-length #f)
          music))))

     ;; we may only want to print the last length
     (show-last-length
      (let
          ((skip-length (ly:moment-sub orig-length show-last-length)))
        (make-simultaneous-music
         (list
          (make-skipped skip-length #t)
          music))))

     ;; we may only want to print the beginning; in this case
     ;; only the first length will be processed (much faster).
     (show-first-length
      ;; the first length must not exceed the original length.
      (if (ly:moment<? show-first-length orig-length)
          (set! (ly:music-property music 'length)
                show-first-length))
      music)

     (else music))))


(define-session-public toplevel-music-functions
  (list
   (lambda (music parser) (expand-repeat-chords!
                           (cons 'rhythmic-event
                                 (ly:parser-lookup parser '$chord-repeat-events))
                           music))
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
  (or (number? alteration-def)
      (equal? laziness #t)
      (<= bar-number (+ (cadr alteration-def) laziness))))

(define (accidental-invalid? alteration-def)
  "Checks an alteration entry for being invalid.

Non-key alterations are invalidated when tying into the next bar or
when there is a clef change, since neither repetition nor cancellation
can be omitted when the same note occurs again.

Returns @code{#f} or the reason for the invalidation, a symbol."
  (let* ((def (if (pair? alteration-def)
                  (car alteration-def)
                  alteration-def)))
    (and (symbol? def) def)))

(define (extract-alteration alteration-def)
  (cond ((number? alteration-def)
         alteration-def)
        ((pair? alteration-def)
         (car alteration-def))
        (else 0)))

(define (check-pitch-against-signature context pitch barnum laziness octaveness)
  "Checks the need for an accidental and a @q{restore} accidental against
@code{localKeySignature}.  The @var{laziness} is the number of measures
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
         (from-key-sig (or (assoc-get notename local-key-sig)

                           ;; If no key signature match is found from localKeySignature, we may have a custom
                           ;; type with octave-specific entries of the form ((octave . pitch) alteration)
                           ;; instead of (pitch . alteration).  Since this type cannot coexist with entries in
                           ;; localKeySignature, try extracting from keySignature instead.
                           (assoc-get pitch-handle key-sig))))

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
     ((and (not ignore-octave)
           from-same-octave
           (recent-enough? barnum from-same-octave laziness))
      (set! previous-alteration from-same-octave))

     ;; from any octave?
     ((and ignore-octave
           from-other-octaves
           (recent-enough? barnum from-other-octaves laziness))
      (set! previous-alteration from-other-octaves))

     ;; not recent enough, extract from key signature/local key signature
     (from-key-sig
      (set! previous-alteration from-key-sig)))

    (if (accidental-invalid? previous-alteration)
        (set! need-accidental #t)

        (let* ((prev-alt (extract-alteration previous-alteration))
               (this-alt (ly:pitch-alteration pitch)))

          (if (not (= this-alt prev-alt))
              (begin
                (set! need-accidental #t)
                (if (and (not (= this-alt 0))
                         (and (< (abs this-alt) (abs prev-alt))
                              (> (* prev-alt this-alt) 0)))
                    (set! need-restore #t))))))

    (cons need-restore need-accidental)))

(define-public ((make-accidental-rule octaveness laziness) context pitch barnum measurepos)
  "Create an accidental rule that makes its decision based on the octave of
the note and a laziness value.

@var{octaveness} is either @code{'same-octave} or @code{'any-octave} and
defines whether the rule should respond to accidental changes in other
octaves than the current.  @code{'same-octave} is the normal way to typeset
accidentals -- an accidental is made if the alteration is different from the
last active pitch in the same octave.  @code{'any-octave} looks at the last
active pitch in any octave.

@var{laziness} states over how many bars an accidental should be remembered.
@code{0}@tie{}is the default -- accidental lasts over 0@tie{}bar lines, that
is, to the end of current measure.  A positive integer means that the
accidental lasts over that many bar lines.  @w{@code{-1}} is `forget
immediately', that is, only look at key signature.  @code{#t} is `forever'."

  (check-pitch-against-signature context pitch barnum laziness octaveness))

(define (key-entry-notename entry)
  "Return the pitch of an @var{entry} in @code{localKeySignature}.
The @samp{car} of the entry is either of the form @code{notename} or
of the form @code{(octave . notename)}.  The latter form is used for special
key signatures or to indicate an explicit accidental.

The @samp{cdr} of the entry is either a rational @code{alter} indicating
a key signature alteration, or of the form
@code{(alter . (barnum . measurepos))} indicating an alteration caused by
an accidental in music."
  (if (pair? (car entry))
      (cdar entry)
      (car entry)))

(define (key-entry-octave entry)
  "Return the octave of an entry in @code{localKeySignature}
or @code{#f} if the entry does not have an octave.
See @code{key-entry-notename} for details."
  (and (pair? (car entry)) (caar entry)))

(define (key-entry-bar-number entry)
  "Return the bar number of an entry in @code{localKeySignature}
or @code {#f} if the entry does not have a bar number.
See @code{key-entry-notename} for details."
  (and (pair? (cdr entry)) (caddr entry)))

(define (key-entry-measure-position entry)
  "Return the measure position of an entry in @code{localKeySignature}
or @code {#f} if the entry does not have a measure position.
See @code{key-entry-notename} for details."
  (and (pair? (cdr entry)) (cdddr entry)))

(define (key-entry-alteration entry)
  "Return the alteration of an entry in localKeySignature.

For convenience, returns @code{0} if entry is @code{#f}."
  (if entry
      (if (number? (cdr entry))
          (cdr entry)
          (cadr entry))
      0))

(define-public (find-pitch-entry keysig pitch accept-global accept-local)
  "Return the first entry in @var{keysig} that matches @var{pitch}.
@var{accept-global} states whether key signature entries should be included.
@var{accept-local} states whether local accidentals should be included.
If no matching entry is found, @var{#f} is returned."
  (and (pair? keysig)
       (let* ((entry (car keysig))
              (entryoct (key-entry-octave entry))
              (entrynn (key-entry-notename entry))
              (nn (ly:pitch-notename pitch)))
         (if (and (equal? nn entrynn)
                  (or (not entryoct)
                      (= entryoct (ly:pitch-octave pitch)))
                  (if (key-entry-bar-number entry)
                      accept-local
                      accept-global))
             entry
             (find-pitch-entry (cdr keysig) pitch accept-global accept-local)))))

(define-public (neo-modern-accidental-rule context pitch barnum measurepos)
  "An accidental rule that typesets an accidental if it differs from the
key signature @emph{and} does not directly follow a note on the same
staff line.  This rule should not be used alone because it does neither
look at bar lines nor different accidentals at the same note name."
  (let* ((keysig (ly:context-property context 'localKeySignature))
         (entry (find-pitch-entry keysig pitch #t #t)))
    (if (not entry)
        (cons #f #f)
        (let* ((global-entry (find-pitch-entry keysig pitch #t #f))
               (key-acc (key-entry-alteration global-entry))
               (acc (ly:pitch-alteration pitch))
               (entrymp (key-entry-measure-position entry))
               (entrybn (key-entry-bar-number entry)))
          (cons #f (not (or (equal? acc key-acc)
                            (and (equal? entrybn barnum) (equal? entrymp measurepos)))))))))

(define-public (teaching-accidental-rule context pitch barnum measurepos)
  "An accidental rule that typesets a cautionary accidental if it is
included in the key signature @emph{and} does not directly follow a note
on the same staff line."
  (let* ((keysig (ly:context-property context 'localKeySignature))
         (entry (find-pitch-entry keysig pitch #t #t)))
    (if (not entry)
        (cons #f #f)
        (let* ((entrymp (key-entry-measure-position entry))
               (entrybn (key-entry-bar-number entry)))
          (cons #f (not (and (equal? entrybn barnum) (equal? entrymp measurepos))))))))

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
  "Set accidental style to @var{style}.  Optionally take a context
argument, e.g. @code{'Staff} or @code{'Voice}.  The context defaults
to @code{Staff}, except for piano styles, which use @code{GrandStaff}
as a context."
  (let ((context (if (pair? rest)
                     (car rest) 'Staff))
        (pcontext (if (pair? rest)
                      (car rest) 'GrandStaff)))
    (cond
     ;; accidentals as they were common in the 18th century.
     ((equal? style 'default)
      (set-accidentals-properties #t
                                  `(Staff ,(make-accidental-rule 'same-octave 0))
                                  '()
                                  context))
     ;; accidentals from one voice do NOT get canceled in other voices
     ((equal? style 'voice)
      (set-accidentals-properties #t
                                  `(Voice ,(make-accidental-rule 'same-octave 0))
                                  '()
                                  context))
     ;; accidentals as suggested by Kurt Stone, Music Notation in the 20th century.
     ;; This includes all the default accidentals, but accidentals also needs canceling
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
     ((equal? style 'neo-modern-voice)
      (set-accidentals-properties #f
                                  `(Voice ,(make-accidental-rule 'same-octave 0)
                                          ,(make-accidental-rule 'any-octave 0)
                                          ,(make-accidental-rule 'same-octave 1)
                                          ,neo-modern-accidental-rule
                                          Staff ,(make-accidental-rule 'same-octave 0)
                                          ,(make-accidental-rule 'any-octave 0)
                                          ,(make-accidental-rule 'same-octave 1)
                                          ,neo-modern-accidental-rule)
                                  '()
                                  context))
     ((equal? style 'neo-modern-voice-cautionary)
      (set-accidentals-properties #f
                                  `(Voice ,(make-accidental-rule 'same-octave 0))
                                  `(Voice ,(make-accidental-rule 'any-octave 0)
                                          ,(make-accidental-rule 'same-octave 1)
                                          ,neo-modern-accidental-rule
                                          Staff ,(make-accidental-rule 'same-octave 0)
                                          ,(make-accidental-rule 'any-octave 0)
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
     ;; Accidentals are typeset for each voice, but they ARE canceled across voices.
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
     ;; Accidentals are canceled across the staves in the same grand staff as well
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
      (make-sequential-music '())))))

(define-public (invalidate-alterations context)
  "Invalidate alterations in @var{context}.

Elements of @code{'localKeySignature} corresponding to local
alterations of the key signature have the form
@code{'((octave . notename) . (alter barnum . measurepos))}.
Replace them with a version where @code{alter} is set to @code{'clef}
to force a repetition of accidentals.

Entries that conform with the current key signature are not invalidated."
  (let* ((keysig (ly:context-property context 'keySignature)))
    (set! (ly:context-property context 'localKeySignature)
          (map-in-order
           (lambda (entry)
             (let* ((localalt (key-entry-alteration entry)))
               (if (or (accidental-invalid? localalt)
                       (not (key-entry-bar-number entry))
                       (= localalt
                          (key-entry-alteration
                           (find-pitch-entry
                            keysig
                            (ly:make-pitch (key-entry-octave entry)
                                           (key-entry-notename entry)
                                           0)
                            #t #t))))
                   entry
                   (cons (car entry) (cons 'clef (cddr entry))))))
           (ly:context-property context 'localKeySignature)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (skip-of-length mus)
  "Create a skip of exactly the same length as @var{mus}."
  (let* ((skip
          (make-music
           'SkipEvent
           'duration (ly:make-duration 0 0))))

    (make-event-chord (list (ly:music-compress skip (ly:music-length mus))))))

(define-public (mmrest-of-length mus)
  "Create a multi-measure rest of exactly the same length as @var{mus}."

  (let* ((skip
          (make-multi-measure-rest
           (ly:make-duration 0 0) '())))
    (ly:music-compress skip (ly:music-length mus))
    skip))

(define-public (pitch-of-note event-chord)
  (let ((evs (filter (lambda (x)
                       (music-has-type x 'note-event))
                     (ly:music-property event-chord 'elements))))

    (and (pair? evs)
         (ly:music-property (car evs) 'pitch))))

(define-public (duration-of-note event-chord)
  (cond
   ((pair? event-chord)
    (or (duration-of-note (car event-chord))
        (duration-of-note (cdr event-chord))))
   ((ly:music? event-chord)
    (let ((dur (ly:music-property event-chord 'duration)))
      (if (ly:duration? dur)
          dur
          (duration-of-note (ly:music-property event-chord 'elements)))))
   (else #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (map-some-music map? music)
  "Walk through @var{music}, transform all elements calling @var{map?}
and only recurse if this returns @code{#f}.  @code{elements} or
@code{articulations} that are not music expressions are discarded:
this allows some amount of filtering.

@code{map-some-music} may overwrite the original @var{music}."
  (let loop ((music music))
    (or (map? music)
        (let ((elt (ly:music-property music 'element))
              (elts (ly:music-property music 'elements))
              (arts (ly:music-property music 'articulations)))
          (if (ly:music? elt)
              (set! (ly:music-property music 'element)
                    (loop elt)))
          (if (pair? elts)
              (set! (ly:music-property music 'elements)
                    (filter! ly:music? (map! loop elts))))
          (if (pair? arts)
              (set! (ly:music-property music 'articulations)
                    (filter! ly:music? (map! loop arts))))
          music))))

(define-public (for-some-music stop? music)
  "Walk through @var{music}, process all elements calling @var{stop?}
and only recurse if this returns @code{#f}."
  (let loop ((music music))
    (if (not (stop? music))
        (let ((elt (ly:music-property music 'element)))
          (if (ly:music? elt)
              (loop elt))
          (for-each loop (ly:music-property music 'elements))
          (for-each loop (ly:music-property music 'articulations))))))

(define-public (fold-some-music pred? proc init music)
  "This works recursively on music like @code{fold} does on a list,
calling @samp{(@var{pred?} music)} on every music element.  If
@code{#f} is returned for an element, it is processed recursively
with the same initial value of @samp{previous}, otherwise
@samp{(@var{proc} music previous)} replaces @samp{previous}
and no recursion happens.
The top @var{music} is processed using @var{init} for @samp{previous}."
  (let loop ((music music) (previous init))
    (if (pred? music)
        (proc music previous)
        (fold loop
              (fold loop
                    (let ((elt (ly:music-property music 'element)))
                      (if (null? elt)
                          previous
                          (loop elt previous)))
                    (ly:music-property music 'elements))
              (ly:music-property music 'articulations)))))

(define-public (extract-music music pred?)
  "Return a flat list of all music matching @var{pred?} inside of
@var{music}, not recursing into matches themselves."
  (reverse! (fold-some-music pred? cons '() music)))

(define-public (extract-named-music music music-name)
  "Return a flat list of all music named @var{music-name} (either a
single event symbol or a list of alternatives) inside of @var{music},
not recursing into matches themselves."
  (extract-music
   music
   (if (cheap-list? music-name)
       (lambda (m) (memq (ly:music-property m 'name) music-name))
       (lambda (m) (eq? (ly:music-property m 'name) music-name)))))

(define-public (extract-typed-music music type)
  "Return a flat list of all music with @var{type} (either a single
type symbol or a list of alternatives) inside of @var{music}, not
recursing into matches themselves."
  (extract-music
   music
   (if (cheap-list? type)
       (lambda (m)
         (any (lambda (t) (music-is-of-type? m t)) type))
       (lambda (m) (music-is-of-type? m type)))))

(define*-public (event-chord-wrap! music #:optional parser)
  "Wrap isolated rhythmic events and non-postevent events in
@var{music} inside of an @code{EventChord}.  If the optional
@var{parser} argument is given, chord repeats @samp{q} are expanded
using the default settings.  Otherwise, you need to cater for them
yourself."
  (map-some-music
   (lambda (m)
     (cond ((music-is-of-type? m 'event-chord)
            (if (pair? (ly:music-property m 'articulations))
                (begin
                  (set! (ly:music-property m 'elements)
                        (append (ly:music-property m 'elements)
                                (ly:music-property m 'articulations)))
                  (set! (ly:music-property m 'articulations) '())))
            m)
           ((music-is-of-type? m 'rhythmic-event)
            (let ((arts (ly:music-property m 'articulations)))
              (if (pair? arts)
                  (set! (ly:music-property m 'articulations) '()))
              (make-event-chord (cons m arts))))
           (else #f)))
   (if parser
       (expand-repeat-chords!
        (cons 'rhythmic-event
              (ly:parser-lookup parser '$chord-repeat-events))
        music)
       music)))

(define-public (event-chord-notes event-chord)
  "Return a list of all notes from @var{event-chord}."
  (filter
   (lambda (m) (eq? 'NoteEvent (ly:music-property m 'name)))
   (ly:music-property event-chord 'elements)))

(define-public (event-chord-pitches event-chord)
  "Return a list of all pitches from @var{event-chord}."
  (map (lambda (x) (ly:music-property x 'pitch))
       (event-chord-notes event-chord)))

(define-public (event-chord-reduce music)
  "Reduces event chords in @var{music} to their first note event,
retaining only the chord articulations.  Returns the modified music."
  (map-some-music
   (lambda (m)
     (and (music-is-of-type? m 'event-chord)
          (let*-values (((notes arts) (partition
                                       (lambda (mus)
                                         (music-is-of-type? mus 'rhythmic-event))
                                       (ly:music-property m 'elements)))
                        ((dur) (ly:music-property m 'duration))
                        ((full-arts) (append arts
                                             (ly:music-property m 'articulations)))
                        ((first-note) (and (pair? notes) (car notes))))
            (cond (first-note
                   (set! (ly:music-property first-note 'articulations)
                         full-arts)
                   first-note)
                  ((ly:duration? dur)
                   ;; A repeat chord. Produce an unpitched note.
                   (make-music 'NoteEvent
                               'duration dur
                               'articulations full-arts))
                  (else
                   (ly:music-error m (_ "Missing duration"))
                   (make-music 'NoteEvent
                               'duration (ly:make-duration 2 0 0)
                               'articulations full-arts))))))
   music))


(defmacro-public make-relative (variables reference music)
  "The list of pitch or music variables in @var{variables} is used as
a sequence for creating relativable music from @var{music}.

When the constructed music is used outside of @code{\\relative}, it
just reflects plugging in the @var{variables} into @var{music}.

The action inside of @code{\\relative}, however, is determined by
first relativizing the surrogate @var{reference} with the variables
plugged in and then using the variables relativized as a side effect
of relativizing @var{reference} for evaluating @var{music}.

Since pitches don't have the object identity required for tracing the
effect of the reference call, they are replaced @emph{only} for the
purpose of evaluating @var{reference} with simple pitched note events.

The surrogate @var{reference} expression has to be written with that
in mind.  In addition, it must @emph{not} contain @emph{copies} of
music that is supposed to be relativized but rather the
@emph{originals}.  This @emph{includes} the pitch expressions.  As a
rule, inside of @code{#@{@dots{}#@}} variables must @emph{only} be
introduced using @code{#}, never via the copying construct @code{$}.
The reference expression will usually just be a sequential or chord
expression naming all variables in sequence, implying that following
music will be relativized according to the resulting pitch of the last
or first variable, respectively.

Since the usual purpose is to create more complex music from general
arguments and since music expression parts must not occur more than
once, one @emph{does} generally need to use copying operators in the
@emph{replacement} expression @var{music} when using an argument more
than once there.  Using an argument more than once in @var{reference},
in contrast, does not make sense.

There is another fine point to mind: @var{music} must @emph{only}
contain freshly constructed elements or copied constructs.  This will
be the case anyway for regular LilyPond code inside of
@code{#@{@dots{}#@}}, but any other elements (apart from the
@var{variables} themselves which are already copied) must be created
or copied as well.

The reason is that it is usually permitted to change music in-place as
long as one does a @var{ly:music-deep-copy} on it, and such a copy of
the whole resulting expression will @emph{not} be able to copy
variables/values inside of closures where the information for
relativization is being stored.
"

  ;; pitch and music generator might be stored instead in music
  ;; properties, and it might make sense to create a music type of its
  ;; own for this kind of construct rather than using
  ;; RelativeOctaveMusic
  (define ((make-relative::to-relative-callback variables music-call ref-call)
           music pitch)
    (let* ((ref-vars (map (lambda (v)
                            (if (ly:pitch? v)
                                (make-music 'NoteEvent 'pitch v)
                                (ly:music-deep-copy v)))
                          variables))
           (after-pitch (ly:make-music-relative! (apply ref-call ref-vars) pitch))
           (actual-vars (map (lambda (v r)
                               (if (ly:pitch? v)
                                   (ly:music-property r 'pitch)
                                   r))
                             variables ref-vars))
           (rel-music (apply music-call actual-vars)))
      (set! (ly:music-property music 'element) rel-music)
      after-pitch))
  `(make-music 'RelativeOctaveMusic
               'to-relative-callback
               (,make-relative::to-relative-callback
                (list ,@variables)
                (lambda ,variables ,music)
                (lambda ,variables ,reference))
               'element ,music))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following functions are all associated with the crossStaff
;;  function

(define (close-enough? x y)
  "Values are close enough to ignore the difference"
  (< (abs (- x y)) 0.0001))

(define (extent-combine extents)
  "Combine a list of extents"
  (if (pair? (cdr extents))
      (interval-union (car extents) (extent-combine (cdr extents)))
      (car extents)))

(define ((stem-connectable? ref root) stem)
  "Check if the stem is connectable to the root"
  ;; The root is always connectable to itself
  (or (eq? root stem)
      (and
       ;; Horizontal positions of the stems must be almost the same
       (close-enough? (car (ly:grob-extent root ref X))
                      (car (ly:grob-extent stem ref X)))
       ;; The stem must be in the direction away from the root's notehead
       (positive? (* (ly:grob-property root 'direction)
                     (- (car (ly:grob-extent stem ref Y))
                        (car (ly:grob-extent root ref Y))))))))

(define (stem-span-stencil span)
  "Connect stems if we have at least one stem connectable to the root"
  (let* ((system (ly:grob-system span))
         (root (ly:grob-parent span X))
         (stems (filter (stem-connectable? system root)
                        (ly:grob-object span 'stems))))
    (if (<= 2 (length stems))
        (let* ((yextents (map (lambda (st)
                                (ly:grob-extent st system Y)) stems))
               (yextent (extent-combine yextents))
               (layout (ly:grob-layout root))
               (blot (ly:output-def-lookup layout 'blot-diameter)))
          ;; Hide spanned stems
          (for-each (lambda (st)
                      (set! (ly:grob-property st 'stencil) #f))
                    stems)
          ;; Draw a nice looking stem with rounded corners
          (ly:round-filled-box (ly:grob-extent root root X) yextent blot))
        ;; Nothing to connect, don't draw the span
        #f)))

(define ((make-stem-span! stems trans) root)
  "Create a stem span as a child of the cross-staff stem (the root)"
  (let ((span (ly:engraver-make-grob trans 'Stem '())))
    (ly:grob-set-parent! span X root)
    (set! (ly:grob-object span 'stems) stems)
    ;; Suppress positioning, the stem code is confused by this weird stem
    (set! (ly:grob-property span 'X-offset) 0)
    (set! (ly:grob-property span 'stencil) stem-span-stencil)))

(define-public (cross-staff-connect stem)
  "Set cross-staff property of the stem to this function to connect it to
other stems automatically"
  #t)

(define (stem-is-root? stem)
  "Check if automatic connecting of the stem was requested.  Stems connected
to cross-staff beams are cross-staff, but they should not be connected to
other stems just because of that."
  (eq? cross-staff-connect (ly:grob-property-data stem 'cross-staff)))

(define (make-stem-spans! ctx stems trans)
  "Create stem spans for cross-staff stems"
  ;; Cannot do extensive checks here, just make sure there are at least
  ;; two stems at this musical moment
  (if (<= 2 (length stems))
      (let ((roots (filter stem-is-root? stems)))
        (for-each (make-stem-span! stems trans) roots))))

(define-public (Span_stem_engraver ctx)
  "Connect cross-staff stems to the stems above in the system"
  (let ((stems '()))
    (make-engraver
     ;; Record all stems for the given moment
     (acknowledgers
      ((stem-interface trans grob source)
       (set! stems (cons grob stems))))
     ;; Process stems and reset the stem list to empty
     ((process-acknowledged trans)
      (make-stem-spans! ctx stems trans)
      (set! stems '())))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following is used by the alterBroken function.

(define-public ((value-for-spanner-piece arg) grob)
  "Associate a piece of broken spanner @var{grob} with an element
of list @var{arg}."
  (let* ((orig (ly:grob-original grob))
         (siblings (ly:spanner-broken-into orig)))

    (define (helper sibs arg)
      (if (null? arg)
          arg
          (if (eq? (car sibs) grob)
              (car arg)
              (helper (cdr sibs) (cdr arg)))))

    (if (>= (length siblings) 2)
        (helper siblings arg)
        (car arg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; measure counter

(define (measure-counter-stencil grob)
  "Print a number for a measure count.  The number is centered using
the extents of @code{BreakAlignment} grobs associated with
@code{NonMusicalPaperColumn} grobs.  In the case of an unbroken measure, these
columns are the left and right bounds of a @code{MeasureCounter} spanner.
Broken measures are numbered in parentheses."
  (let* ((orig (ly:grob-original grob))
         (siblings (ly:spanner-broken-into orig)) ; have we been split?
         (bounds (ly:grob-array->list (ly:grob-object grob 'columns)))
         (refp (ly:grob-system grob))
         ;; we use the first and/or last NonMusicalPaperColumn grob(s) of
         ;; a system in the event that a MeasureCounter spanner is broken
         (all-cols (ly:grob-array->list (ly:grob-object refp 'columns)))
         (all-cols
          (filter
           (lambda (col) (eq? #t (ly:grob-property col 'non-musical)))
           all-cols))
         (left-bound
          (if (or (null? siblings) ; spanner is unbroken
                  (eq? grob (car siblings))) ; or the first piece
              (car bounds)
              (car all-cols)))
         (right-bound
          (if (or (null? siblings)
                  (eq? grob (car (reverse siblings))))
              (car (reverse bounds))
              (car (reverse all-cols))))
         (elts-L (ly:grob-array->list (ly:grob-object left-bound 'elements)))
         (elts-R (ly:grob-array->list (ly:grob-object right-bound 'elements)))
         (break-alignment-L
          (filter
           (lambda (elt) (grob::has-interface elt 'break-alignment-interface))
           elts-L))
         (break-alignment-R
          (filter
           (lambda (elt) (grob::has-interface elt 'break-alignment-interface))
           elts-R))
         (break-alignment-L-ext (ly:grob-extent (car break-alignment-L) refp X))
         (break-alignment-R-ext (ly:grob-extent (car break-alignment-R) refp X))
         (num (markup (number->string (ly:grob-property grob 'count-from))))
         (num
          (if (or (null? siblings)
                  (eq? grob (car siblings)))
              num
              (make-parenthesize-markup num)))
         (num (grob-interpret-markup grob num))
         (num (ly:stencil-aligned-to num X (ly:grob-property grob 'self-alignment-X)))
         (num
          (ly:stencil-translate-axis
           num
           (+ (interval-length break-alignment-L-ext)
              (* 0.5
                 (- (car break-alignment-R-ext)
                    (cdr break-alignment-L-ext))))
           X)))
    num))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following are used by the \offset function

(define (find-value-to-offset prop self alist)
  "Return the first value of the property @var{prop} in the property
alist @var{alist} -- after having found @var{self}.  If @var{self} is
not found, return the first value of @var{prop}."
  (let ((segment (member (cons prop self) alist)))
    (if (not segment)
        (assoc-get prop alist)
        (assoc-get prop (cdr segment)))))

(define (offset-multiple-types arg offsets)
  "Displace @var{arg} by @var{offsets} if @var{arg} is a number, a
number pair, or a list of number pairs.  If @var{offsets} is an empty
list or if there is a type-mismatch, @var{arg} will be returned."
  (cond
    ((and (number? arg) (number? offsets))
     (+ arg offsets))
    ((and (number-pair? arg)
          (or (number? offsets)
              (number-pair? offsets)))
     (coord-translate arg offsets))
    ((and (number-pair-list? arg) (number-pair-list? offsets))
     (map
       (lambda (x y) (coord-translate x y))
       arg offsets))
    (else arg)))

(define-public (offsetter property offsets)
  "Apply @var{offsets} to the default values of @var{property} of @var{grob}.
Offsets are restricted to immutable properties and values of type @code{number},
@code{number-pair}, or @code{number-pair-list}."
  (define (self grob)
    (let* ((immutable (ly:grob-basic-properties grob))
           ; We need to search the basic-properties alist for our property to
           ; obtain values to offset.  Our search is complicated by the fact that
           ; calling the music function `offset' as an override conses a pair to
           ; the head of the alist.  This pair must be discounted.  The closure it
           ; contains is named `self' so it can be easily recognized.  If `offset'
           ; is called as a tweak, the basic-property alist is unaffected.
           (target (find-value-to-offset property self immutable))
           ; if target is a procedure, we need to apply it to our grob to calculate
           ; values to offset.
           (vals
             (if (procedure? target)
                 (target grob)
                 target))
           (can-type-be-offset?
             (or (number? vals)
                 (number-pair? vals)
                 (number-pair-list? vals))))

      (if can-type-be-offset?
          ; '(+inf.0 . -inf.0) would offset to itself.  This will be confusing to a
          ; user unaware of the default value of the property, so issue a warning.
          (if (equal? empty-interval vals)
              (ly:warning "default '~a of ~a is ~a and can't be offset"
                property grob vals)
              (let* ((orig (ly:grob-original grob))
                     (siblings
                       (if (ly:spanner? grob)
                           (ly:spanner-broken-into orig)
                           '()))
                     (total-found (length siblings))
                     ; Since there is some flexibility in input syntax,
                     ; structure of `offsets' is normalized.
                     (offsets
                       (if (or (not (pair? offsets))
                               (number-pair? offsets)
                               (and (number-pair-list? offsets)
                                    (number-pair-list? vals)))
                           (list offsets)
                           offsets)))

                (define (helper sibs offs)
                  ; apply offsets to the siblings of broken spanners
                  (if (pair? offs)
                      (if (eq? (car sibs) grob)
                          (offset-multiple-types vals (car offs))
                          (helper (cdr sibs) (cdr offs)))
                      vals))

                (if (>= total-found 2)
                    (helper siblings offsets)
                    (offset-multiple-types vals (car offsets)))))

              (begin
                (ly:warning "the property '~a of ~a cannot be offset" property grob)
                vals))))
    ; return the closure named `self'
    self)
