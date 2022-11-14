;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 1998--2022 Jan Nieuwenhuizen <janneke@gnu.org>
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

(use-modules (ice-9 optargs)
             (ice-9 match)
             (srfi srfi-11))

(define-public (music-is-of-type? mus type)
  "Does @var{mus} belong to the music class @var{type}?"
  (memq type (ly:music-property mus 'types)))

(eval-when (expand load eval)
  (define-public (music-type-predicate types)
    "Return a predicate function that can be used for checking
music to have one of the types listed in @var{types}."
    (if (cheap-list? types)
        (lambda (m)
          (any (lambda (t) (music-is-of-type? m t)) types))
        (lambda (m) (music-is-of-type? m types)))))

(define-public (music-selective-map descend? function music)
  "Apply @var{function} recursively to @var{music}, but refrain
from mapping subexpressions of music that does not satisfy
@var{descend?}."
  (define (worker m)
    (music-selective-map descend? function m))
  (if (descend? music)
      (let ((arts (ly:music-property music 'articulations))
            (es (ly:music-property music 'elements))
            (e (ly:music-property music 'element)))
        (if (pair? es)
            (set! (ly:music-property music 'elements)
                  (map worker es)))
        (if (pair? arts)
            (set! (ly:music-property music 'articulations)
                  (map worker arts)))
        (if (ly:music? e)
            (set! (ly:music-property music 'element)
                  (worker e)))))
  (recompute-music-length (function music)))

(define-public (music-map function music)
  "Apply @var{function} to @var{music} and all of the music it contains.

First it recurses over the children, then the function is applied to
@var{music}."
  (music-selective-map ly:music? function music))

(define-public (music-selective-filter descend? pred? music)
  "Recursively filter out music expressions that do not satisfy
  @var{pred?}, but refrain from filtering the subexpressions of
  music that does not satisfy @var{descend?}."

  (define (inner-music-filter music)
    "Recursive function."
    (if (not (descend? music))
        (if (not (pred? music))
            (set! music '()))
        (let* ((es (ly:music-property music 'elements))
               (e (ly:music-property music 'element))
               (as (ly:music-property music 'articulations))
               (filtered-as (filter ly:music? (map inner-music-filter as)))
               (filtered-e (if (ly:music? e)
                               (inner-music-filter e)
                               e))
               (filtered-es (filter ly:music? (map inner-music-filter es))))
          (if (not (null? e))
              (set! (ly:music-property music 'element) filtered-e))
          (if (not (null? es))
              (set! (ly:music-property music 'elements) filtered-es))
          (if (not (null? as))
              (set! (ly:music-property music 'articulations) filtered-as))
          ;; if filtering invalidated 'element, we remove the music unless
          ;; there are remaining 'elements in which case we just hope and
          ;; pray.
          (if (or (not (pred? music))
                  (and (null? filtered-es)
                       (not (ly:music? filtered-e))
                       (ly:music? e)))
              (set! music '()))
          (if (ly:music? music)
              (recompute-music-length music))))
    music)

  (set! music (inner-music-filter music))
  (if (ly:music? music)
      music
      (make-music 'Music)))       ;must return music.

(define-public (music-filter pred? music)
  "Filter out music expressions that do not satisfy @var{pred?}."
  (music-selective-filter ly:music? pred? music))

(define*-public (display-music music #:optional (port (current-output-port)))
  "Display @var{music}, not done with @code{music-map} for clarity of
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
    "Return a keyword, e.g., `#:bold`, from the `proc` function,
     e.g., `#<procedure bold-markup (layout props arg)>`."
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
  (define (if-nonzero num)
    (if (zero? num) '() (list num)))
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
         `(ly:make-moment
           ,@(let ((main (ly:moment-main obj))
                   (grace (ly:moment-grace obj)))
               (cond ((zero? grace) (list main))
                     ((negative? grace) (list main grace))
                     (else ;;positive grace requires 4-arg form
                      (list (numerator main)
                            (denominator main)
                            (numerator grace)
                            (denominator grace)))))))
        (;; note duration
         (ly:duration? obj)
         `(ly:make-duration ,(ly:duration-log obj)
                            ,@(if (= (ly:duration-scale obj) 1)
                                  (if-nonzero (ly:duration-dot-count obj))
                                  (list (ly:duration-dot-count obj)
                                        (ly:duration-scale obj)))))
        (;; note pitch
         (ly:pitch? obj)
         `(ly:make-pitch ,(ly:pitch-octave obj)
                         ,(ly:pitch-notename obj)
                         ,@(if-nonzero (ly:pitch-alteration obj))))
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
  "Display @var{obj}, typically a music expression, in a friendly fashion,
which often can be read back in order to generate an equivalent expression."
  (pretty-print (music->make-music obj) port)
  (newline port))

;;;
;;; Scheme music expression --> Lily-syntax-using string translator
;;;
(use-modules (srfi srfi-39)
             (lily display-lily))

(define*-public (display-lily-music expr #:optional (port (current-output-port)))
  "Display the music expression @var{expr} using LilyPond syntax."
  (memoize-clef-names supported-clefs)
  (parameterize ((*indent* 0)
                 (*omit-duration* #f))
    (display (music->lily-string expr) port)
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
    ;; clear cached length, since it's no longer valid
    (set! (ly:music-property music 'length) '())
    music))

(define-public (shift-duration-log music shift dot)
  (music-map (lambda (x) (shift-one-duration-log x shift dot))
             music))

(define-public (volta-spec-music number-list music)
  "Add \\volta @var{number-list} to @var{music}."
  (make-music 'VoltaSpeccedMusic
              'element music
              'volta-numbers number-list))

(define-public (make-repeat name times main alts)
  "Create a repeat music expression, with all properties initialized
properly."
  (let* ((sane-times (max times 1)) ; TODO: Warn?
         (type (or (assoc-get name '(("volta" . VoltaRepeatedMusic)
                                     ("segno" . SegnoRepeatedMusic)
                                     ("unfold" . UnfoldedRepeatedMusic)
                                     ("percent" . PercentRepeatedMusic)
                                     ("tremolo" . TremoloRepeatedMusic)))
                   (begin (ly:warning (G_ "unknown repeat type `~S': \
must be volta, unfold, percent, or tremolo") name)
                          'VoltaRepeatedMusic)))
         (alt-music
          (if (ly:music? alts)
              (begin
                ;; TODO: Consider accepting plain sequential-music,
                ;; which would allow this:
                ;;
                ;;     alts = { a b } % note no \alternative here
                ;;     \repeat volta 2 {} \alternative \alts
                ;;
                (if (not (music-is-of-type? alts 'sequential-alternative-music))
                    (ly:music-warning alts (G_ "alternative music expected")))
                alts)
              ;; Accept a bare element list for backward compatibility.
              (make-music 'SequentialAlternativeMusic
                          'elements alts
                          'origin (ly:music-property main 'origin)))))

    ;; If the user did not specify volta numbers, wrap the
    ;; alternatives for consistency with the legacy behavior.
    (define (elaborate-alternative-music alt-music times)
      (let* ((alts (ly:music-property alt-music 'elements))
             (lalts (length alts))
             (talts (if (< times lalts)
                        (let ((message (G_ "More alternatives than repeats.  \
Junking excess alternatives")))
                          ;; The \repeat and \the alternative are not
                          ;; necessarily close together in the source.
                          ;; Warn twice to point to both.
                          (ly:music-warning main message)
                          (ly:music-warning alt-music message)
                          (set! lalts times)
                          (take alts times))
                        alts)))

        (define (is-specced music)
          (music-is-of-type? music 'volta-specification))

        (if (not (any is-specced alts))
            (let* ((alt-1-count (1+ (- times lalts)))
                   ;; volta numbers for each alternative (list of lists)
                   (volta-numbers (cons
                                   (map 1+ (iota alt-1-count))
                                   (map (lambda (i) (list (+ alt-1-count 1 i)))
                                        (iota (- times 1))))))
              ;; wrap the alternatives and set their volta numbers
              (set! talts (map volta-spec-music volta-numbers talts))))
        (make-music 'SequentialAlternativeMusic
                    'elements talts)))

    (define (pass-over-repeated-music music)
      (not (music-is-of-type? music 'repeated-music)))

    (define (map-alternatives m)
      (if (music-is-of-type? m 'sequential-alternative-music)
          (elaborate-alternative-music m sane-times)
          m))

    (make-music type
                'element (music-selective-map
                          pass-over-repeated-music
                          map-alternatives
                          main)
                'repeat-count sane-times
                'elements
                (ly:music-property
                 (elaborate-alternative-music alt-music sane-times)
                 'elements))))

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

(define-public (unfold-repeats types music)
  "Replace repeats of the types given by @var{types} with unfolded repeats.
If @var{types} is an empty list, @code{repeated-music} is taken, unfolding all."
  (let* ((types-list
          (if (or (null? types) (not (list? types)))
              (list types)
              types))
         (repeat-types-alist
          '((volta . volta-repeated-music)
            (segno . segno-repeated-music)
            (percent . percent-repeated-music)
            (tremolo . tremolo-repeated-music)
            (() . repeated-music)))
         (repeat-types-hash (alist->hash-table repeat-types-alist)))
    (for-each
     (lambda (type)
       (let ((repeat-type (hashq-ref repeat-types-hash type)))
         (if repeat-type
             (let ((es (ly:music-property music 'elements))
                   (e (ly:music-property music 'element)))
               (if (music-is-of-type? music repeat-type)
                   (set! music (make-music 'UnfoldedRepeatedMusic music)))
               (if (pair? es)
                   (set! (ly:music-property music 'elements)
                         (map (lambda (x) (unfold-repeats types x)) es)))
               (if (ly:music? e)
                   (set! (ly:music-property music 'element)
                         (unfold-repeats types e))))
             (ly:warning (G_ "unknown repeat-type ~a, ignoring.") type))))
     types-list)
    music))

(define-public (unfold-repeats-fully music)
  "Unfold repeats and expand the resulting @code{unfolded-repeated-music}."
  (map-some-music
   (lambda (m)
     (and (music-is-of-type? m 'unfolded-repeated-music)
          (make-sequential-music
           (ly:music-deep-copy (make-unfolded-set m)))))
   (unfold-repeats '() music)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; property setting music objs.

;; Can't use define* behavior since Guile-1.8 has a bug when combining
;; #:optional with #:key and leaving optional args off.
(define-public (check-grob-path path . rest)
  "Check a grob path specification @var{path}, a symbol list (or a
single symbol), for validity and possibly complete it.  Returns the
completed specification, or @code{#f} if invalid, optionally using
@var{location} for an error message.  If an optional keyword argument
@code{#:start @var{start}} is given, the parsing starts at the given
index in the sequence @samp{Context.@/Grob.@/property.@/sub-property...},
with the default of @samp{0} implying the full path.

If there is no valid first element of @var{path} fitting at the given
path location, an optionally given @code{#:default @var{default}} is
used as the respective element instead without checking it for
validity at this position.

The resulting path after possibly prepending @var{default} can be
constrained in length by optional arguments @code{#:min @var{min}} and
@code{#:max @var{max}}, defaulting to @samp{1} and unlimited,
respectively."
  (define (unspecial? s)
    (not (or (object-property s 'is-grob?)
             (object-property s 'backend-type?))))
  (define (grob? s)
    (object-property s 'is-grob?))
  (define (property? s)
    (object-property s 'backend-type?))
  (define (check c p) (c p))
  (let-keywords
   (if (or (null? rest) (keyword? (car rest)))
       rest
       (cdr rest))
   #f
   ((start 0)
    default
    (min 1)
    max)
   (let* ((path (if (symbol? path) (list path) path))
          (location (and (pair? rest) (not (keyword? (car rest)))
                         (car rest)))
          (checkers
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
           (ly:parser-error
            (format #f (G_ "bad grob property path ~a")
                    path)
            location)
           #f)))))

(define*-public (check-context-path path #:optional location)
  "Check a context property path specification @var{path}, a symbol
list (or a single symbol), for validity and possibly complete it.
Returns the completed specification, or @code{#f} when rising an
error (using optionally @var{location})."
  (let* ((path (if (symbol? path) (list path) path)))
    ;; A Guile 1.x bug specific to optargs precludes moving the
    ;; defines out of the let
    (define (property? s)
      (object-property s 'translation-type?))
    (define (unspecial? s)
      (not (property? s)))
    (define (check c p) (c p))
    (or (case (length path)
          ((1) (and (property? (car path)) (cons 'Bottom path)))
          ((2) (and (unspecial? (car path)) (property? (cadr path)) path))
          (else #f))
        (begin
          (ly:parser-error
           (format #f (G_ "bad context property ~a")
                   path)
           location)
          #f))))

;; Cannot use #:optional and #:key at the same time because of Guile
;; bug in version 1.8
(define-public (check-music-path path . rest)
  "Check a music property path specification @var{path}, a symbol
list (or a single symbol), for validity and possibly complete it.
Returns the completed specification, or @code{#f} when rising an
error (using optionally @var{location})."
  (define (property? s)
    (object-property s 'music-type?))
  (define (unspecial? s)
    (not (property? s)))
  (let-keywords
   (if (or (null? rest) (keyword? (car rest)))
       rest
       (cdr rest))
   #f
   (default)
   (let* ((path (if (symbol? path) (list path) path))
          (location (and (pair? rest) (not (keyword? (car rest)))
                         (car rest))))
     (or (case (length path)
           ((1) (and (property? (car path)) (cons default path)))
           ((2) (and (unspecial? (car path)) (property? (cadr path)) path))
           (else #f))
         (begin
           (ly:parser-error
            (format #f (G_ "bad music property ~a")
                    path)
            location)
           #f)))))

(define-public (make-grob-property-set grob gprop val)
  "Make a @code{Music} expression that overrides a @var{gprop} to
@var{val} in @var{grob}.  Does a pop first, i.e., this is not a
@code{\\temporary \\override}."
  (make-music 'OverrideProperty
              'symbol grob
              'grob-property gprop
              'grob-value val
              'pop-first #t))

(define-public (make-grob-property-override grob gprop val)
  "Make a @code{Music} expression that overrides @var{gprop} to
@var{val} in @var{grob}.  This is a @code{\\temporary \\override},
making it possible to @code{\\revert} to any previous value afterwards."
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

;; Getting a unique context id name

(define-session unique-counter -1)
(define-public (get-next-unique-voice-name)
  (set! unique-counter (1+ unique-counter))
  (format #f "uniqueContext~s" unique-counter))


(define-public (make-voice-props-set n)
  (make-sequential-music
   (append
    (map (lambda (x) (make-grob-property-set x 'direction
                                             (if (odd? n) -1 1)))
         direction-polyphonic-grobs)
    (list
     (make-property-set 'graceSettings general-grace-settings)
     (make-grob-property-set 'NoteColumn 'horizontal-shift (quotient n 2))))))

(define-public (make-voice-props-override n)
  (make-sequential-music
   (append
    (map (lambda (x) (make-grob-property-override x 'direction
                                                  (if (odd? n) -1 1)))
         direction-polyphonic-grobs)
    (list
     (make-property-set 'graceSettings general-grace-settings)
     (make-grob-property-override 'NoteColumn 'horizontal-shift (quotient n 2))))))

(define-public (make-voice-props-revert)
  (make-sequential-music
   (append
    (map (lambda (x) (make-grob-property-revert x 'direction))
         direction-polyphonic-grobs)
    (list (make-property-unset 'graceSettings)
          (make-grob-property-revert 'NoteColumn 'horizontal-shift)))))


(define*-public (context-spec-music m context #:optional id mods)
  "Add @code{\\context @var{context} = @var{id} \\with @var{mods}} to @var{m}."
  (let ((cm (make-music 'ContextSpeccedMusic
                        'element m
                        'context-type context)))
    (if (string? id)
        (set! (ly:music-property cm 'context-id) id))
    (if mods
        (set! (ly:music-property cm 'property-operations)
              (if (ly:context-mod? mods)
                  (ly:get-context-mods mods)
                  mods)))
    cm))

(define*-public (descend-to-context m context #:optional id mods)
  "Like @code{context-spec-music}, but only descending."
  (let ((cm (context-spec-music m context id mods)))
    (ly:music-set-property! cm 'search-direction DOWN)
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

(define-public (make-event-chord elts)
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

(define-public (make-articulation name . properties)
  ;; -----------------------------------------------------------------
  ;; obsoletion handling, may be removed at some point (e.g., for 2.26)
  (if (string? name)
      (begin
        (ly:warning "articulation types should be symbols instead of \
strings since 2.23.6. Please replace (make-articulation \"~a\" ...) \
by (make-articulation '~a ...) or run convert-ly." name name)
        (set! name (string->symbol name))))
  ;; -----------------------------------------------------------------
  (apply make-music 'ArticulationEvent
         'articulation-type name
         properties))

(define-public (make-lyric-event string duration)
  (make-music 'LyricEvent
              'duration duration
              'text string))

(define-public (make-span-event type span-dir)
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

(define-public (get-tweakable-music mus)
  "When tweaking music, return a list of music expressions where the
tweaks should be applied.  Relevant for music wrappers and event
chords."
  (cond ((music-is-of-type? mus 'music-wrapper-music)
         (get-tweakable-music (ly:music-property mus 'element)))
        ((music-is-of-type? mus 'event-chord)
         (filter (music-type-predicate 'rhythmic-event)
                 (ly:music-property mus 'elements)))
        (else (list mus))))

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
  "Copy all events in @var{event-types} (be sure to include
@code{rhythmic-events}) from @var{original-chord} over to
@var{repeat-chord} with their articulations filtered as well.  Any
duration is replaced with the specified @var{duration}."
  ;; First remove everything from event-types that can already be
  ;; found in the repeated chord.  We don't need to look for
  ;; articulations on individual events since they can't actually get
  ;; into a repeat chord given its input syntax.

  (define keep-element? (music-type-predicate event-types))

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
               (ly:music-deep-copy (filter keep-element?
                                           (ly:music-property original-chord
                                                              'elements))
                                   repeat-chord)))
          (for-each
           (lambda (m)
             (let ((arts (ly:music-property m 'articulations)))
               (if (pair? arts)
                   (set! (ly:music-property m 'articulations)
                         (ly:set-origin! (filter! keep-element? arts)
                                         repeat-chord)))
               (if (ly:duration? (ly:music-property m 'duration))
                   (set! (ly:music-property m 'duration) duration))
               (if (ly:music-property m 'cautionary #f)
                   (set! (ly:music-property m 'cautionary) #f))
               (if (ly:music-property m 'force-accidental #f)
                   (set! (ly:music-property m 'force-accidental) #f))))
           elts)
          (append! elts (ly:music-property repeat-chord 'elements))))
  (let ((arts (filter keep-element?
                      (ly:music-property original-chord
                                         'articulations))))
    (if (pair? arts)
        (set! (ly:music-property repeat-chord 'articulations)
              (append!
               (ly:music-deep-copy arts repeat-chord)
               (ly:music-property repeat-chord 'articulations)))))
  repeat-chord)


(define-public (expand-repeat-chords! event-types music)
  "Walk through @var{music} and fill repeated chords (notable by
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
            (ly:music-warning music (G_ "Bad chord repetition"))
            #f)))
        (let ((elt (ly:music-property music 'element)))
          (fold loop (if (ly:music? elt) (loop elt last-chord) last-chord)
                (ly:music-property music 'elements)))))
  music)

;;; This does _not_ copy any articulations.  Rationale: one main
;;; incentive for pitch-repeating durations is after ties, such that
;;; 4~2~8. can stand in for a 15/16 note in \partial 4 position.  In
;;; this use case, any repeated articulations will be a nuisance.
;;;
;;; String assignments in TabStaff might seem like a worthwhile
;;; exception, but they would be better tackled by the respective
;;; engravers themselves (see issue 3662).
;;;
;;; Repeating chords as well seems problematic for things like
;;; \score {
;;;   <<
;;;     \new Staff { c4 c c <c e> }
;;;     \new RhythmicStaff { 4 4 4 4 }
;;;   >>
;;; }
;;;
;;; However, because of MIDI it is not advisable to use RhythmicStaff
;;; without any initial pitch/drum-type.  For music functions taking
;;; pure rhythms as an argument, the running of expand-repeat-notes!
;;; at scorification time is irrelevant: at that point of time, the
;;; music function has already run.

(define-public (expand-repeat-notes! music)
  "Walk through @var{music} and give pitchless notes (not having a
pitch in @code{pitch} or a drum type in @code{drum-type}) the pitch(es)
from the predecessor note/chord if available."
  (let ((last-pitch #f))
    (map-some-music
     (lambda (m)
       (define (set-and-ret last)
         (set! last-pitch last)
         m)
       (cond
        ((music-is-of-type? m 'event-chord)
         (if (any (lambda (m) (music-is-of-type? m 'rhythmic-event))
                  (ly:music-property m 'elements))
             (set! last-pitch m))
         m)
        ((music-is-of-type? m 'note-event)
         (cond
          ((or (ly:music-property m 'pitch #f)
               (ly:music-property m 'drum-type #f))
           => set-and-ret)
          ;; ok, naked rhythm.  Go through the various cases of
          ;; last-pitch
          ;; nothing available: just keep as-is
          ((not last-pitch) m)
          ((ly:pitch? last-pitch)
           (set! (ly:music-property m 'pitch) last-pitch)
           m)
          ((symbol? last-pitch)
           (set! (ly:music-property m 'drum-type) last-pitch)
           m)
          ;; Ok, this is the big bad one: the reference is a chord.
          ;; For now, we use the repeat chord logic.  That's not
          ;; really efficient as cleaning out all articulations is
          ;; quite simpler than what copy-repeat-chord does.
          (else
           (copy-repeat-chord last-pitch
                              (make-music 'EventChord
                                          'elements
                                          (ly:music-property m 'articulations)
                                          'origin
                                          (ly:music-property m 'origin))
                              (ly:music-property m 'duration)
                              '(rhythmic-event)))))
        (else #f)))
     music)))

;;; splitting chords into voices.
(define (voicify-list locs lst id)
  "Make a list of Musics.

voicify-list :: [ [Music ] ] -> id -> [Music]
LST is a list music-lists.

id is 1-based, i.e., Voice=1 (upstems) has number 1.

id may be a symbol or string giving a specific voice id: in this
case, no \\voiceXXX style is selected, merely the context given.

locs is a list of music expressions suitable for giving
error locations (enclosing expression for the first element,
preceding \\\\ separator for the others)
"
  (define (voicify-sublist loc sublist id)
    (cond ((string? id)
           (context-spec-music
            (make-simultaneous-music sublist)
            'Bottom id))
          ((symbol? id)
           (voicify-sublist loc sublist (symbol->string id)))
          ((and (integer? id) (exact? id) (positive? id))
           (context-spec-music
            (make-sequential-music
             (list (make-voice-props-set (1- id))
                   (make-simultaneous-music sublist)))
            'Bottom (number->string id)))
          (else
           (ly:music-warning loc (G_ "Bad voice id: ~a") id)
           (context-spec-music (make-simultaneous-music sublist) 'Bottom))))

  (cond ((null? lst) '())
        ((number? id)
         (cons (voicify-sublist (car locs) (car lst) id)
               (voicify-list (cdr locs) (cdr lst) (1+ id))))
        ((pair? id)
         (cons (voicify-sublist (car locs) (car lst) (car id))
               (voicify-list (cdr locs) (cdr lst) (cdr id))))
        ((null? id)
         (ly:music-warning (car locs) (G_ "\\voices needs more ids"))
         (voicify-list locs lst 1))))

(define (voicify-chord ch id)
  "Split the parts of a chord into different Voices using separator"
  (let ((es (ly:music-property ch 'elements)))
    (set! (ly:music-property  ch 'elements)
          (voicify-list (cons ch (filter music-separator? es))
                        (split-list-by-separator es music-separator?)
                        id))
    ch))

(define*-public (voicify-music m #:optional (id 1))
  "Recursively split chords that are separated with @code{\\\\}.
Optional @var{id} can be a list of context ids to use.  If numeric,
they also indicate a voice type override.  If @var{id} is just a single
number, that's where numbering starts."
  (let loop ((m m))
    (if (not (ly:music? m))
        (ly:error (G_ "music expected: ~S") m))
    (let ((es (ly:music-property m 'elements))
          (e (ly:music-property m 'element)))

      (if (pair? es)
          (set! (ly:music-property m 'elements) (map loop es)))
      (if (ly:music? e)
          (set! (ly:music-property m 'element) (loop e)))
      (if (and (equal? (ly:music-property m 'name) 'SimultaneousMusic)
               (any music-separator? es))
          (context-spec-music (voicify-chord m id) 'Staff)
          m))))

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


(define-public (music-clone music . music-properties)
  "Clone @var{music} and set properties according to
@var{music-properties}, a list of alternating property symbols and
values:

@example
(music-clone start-span 'span-direction STOP)
@end example

Only properties that are not overridden by @var{music-properties} are
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

;; FIXME: Why is this all duplicated from C++? --JeanAS

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

(define-public (ly:music-error music msg . rest)
  (ly:parser-error (apply format #f msg rest)
                   (ly:music-property music 'origin)))

(define-public (ly:event-warning event msg . rest)
  (let ((ip (ly:event-property event 'origin)))
    (if (ly:input-location? ip)
        (apply ly:input-warning ip msg rest)
        (apply ly:warning msg rest))))

(define-public (ly:grob-warning grob path msg . rest)
  (let* ((name (assoc-get 'name (ly:grob-property grob 'meta)))
         (path-string (string-join
                       (map symbol->string
                            (if path
                                ((if (list? path) cons list) name path)
                                (list name)))
                       "."))
         (event (event-cause grob)))
    ;; FIXME: since the message is variadic, this makes such
    ;; warnings untranslatable. --JeanAS
    (if event (apply ly:event-warning event (string-append path-string ": " msg) rest)
        (apply ly:warning (string-append path-string ": " msg) rest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; setting stuff for grace context.
;;

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


(define-syntax-public def-grace-function
  (syntax-rules ()
    ((_ start stop docstring)
     (define-music-function (music) (ly:music?)
       docstring
       (make-music 'GraceMusic
                   'element (make-music 'SequentialMusic
                                        'elements (list (ly:music-deep-copy start)
                                                        music
                                                        (ly:music-deep-copy stop))))))
    ((_ start stop)
     (def-grace-function start stop ""))))

(define-syntax-public define-syntax-function
  (lambda (syntaks)
    "Helper macro for @code{ly:make-music-function}.  Syntax:

  @example
  (define-syntax-function @var{result-type?}
                          (@var{arg1} @var{arg2} @dots{})
                          (@var{type1?} @var{type2?} @dots{})
    @var{function-body})
  @end example

  See @code{define-music-function} for information on type predicates.
  @code{result-type?} can specify a default in the same manner as
  predicates, to be used in case of a type error in arguments or
  result."

    (define (format-docstring docstring args)
      (format #f "~a\n~a"
              (syntax->datum args)
              (syntax->datum docstring)))

    (define (take-body-docstring body)
      (syntax-case body (_i)
        ;; A string and nothing else in the function is not the docstring
        ;; but the return value.
        (((_i doc) b . b*)
         (string? (syntax->datum #'doc))
         ;; If the body starts with (_i "literal string"), strip the _i so that
         ;; the docstring will be recognized on the lambda.
         (values #'doc #'(b . b*)))
        ((doc b . b*)
         (string? (syntax->datum #'doc))
         (values #'doc #'(b . b*)))
        (else
         (values "" body))))

    (define (final-lambda compatibility docstring args fixed-body)
      (let ((fixed-docstring (format-docstring docstring args)))
        (match compatibility
          ((parser-arg location-arg)
           #`(lambda #,args
               #,fixed-docstring
               (let ((#,parser-arg (*parser*))
                     (#,location-arg (*location*)))
                 . #,fixed-body)))
          (#f
           #`(lambda #,args
               #,fixed-docstring
               . #,fixed-body)))))

    (define (currying-lambda args docstring body signature-length)
      (syntax-case args ()
        (((head . head-rest) . rest)
         (currying-lambda #'(head . head-rest)
                          ;; Keep moving docstring to outermost lambda.
                          docstring
                          #`((lambda rest . #,body))
                          signature-length))
        ;; Backwards compatibility heuristic: if the arguments contain 2 more
        ;; elements than the signature, assume they're parser and location.
        ((parser-arg location-arg other-arg ...)
         (eqv? signature-length (length #'(other-arg ...)))
         (final-lambda (list #'parser-arg #'location-arg)
                       docstring
                       #'(other-arg ...)
                       body))
        ((arg ...)
         (final-lambda #f
                       docstring
                       args
                       body))
        ;; If the arguments do not form a list, as with fancy stuff like
        ;; #(define-music-function (arg1 . rest) (integer? (integer? 5) string?) ...),
        ;; fall back to a poorer heuristic: the beginning is list-like
        ;; and it starts with an argument called "parser".
        ((parser-arg location-arg . rest)
         (eq? 'parser (syntax->datum #'parser-arg))
         (final-lambda (list #'parser-arg #'location-arg)
                       docstring
                       #'rest
                       body))
        (else
         (final-lambda #f
                       docstring
                       args
                       body))))

    ;; Argument types can be specified as (predicate? default), where `default`
    ;; gets used when the respective argument is skipped.
    (define (maybe-default s)
      (syntax-case s ()
        ((elt default) #'(cons elt default))
        ((elt) #'(cons elt #f))
        (elt #'elt)))

    (with-syntax (((_ type args (sig ...) . body) syntaks))
      (let-values (((docstring fixed-body)
                    (take-body-docstring #'body)))
        #`(ly:make-music-function
           (list #,(maybe-default #'type)
                 #,@(map maybe-default #'(sig ...)))
           #,(currying-lambda
              #'args
              docstring
              fixed-body
              (length #'(sig ...))))))))

(define-syntax-rule-public (define-music-function elt ...)
  "Define and return a music function.  Syntax:

@example
(define-music-function (@var{arg1} @var{arg2} @dots{})
                       (@var{type1?} @var{type2?} @dots{})
    @var{function-body})
@end example

@var{type1?}, @var{type2?}, etc., can take one of the forms
@code{predicate?} for mandatory arguments satisfying the predicate,
@code{(predicate?)} for optional parameters of that type defaulting to
@code{#f}, @code{(predicate? value)} for optional parameters with a
specified default value (evaluated at definition time).  An optional
parameter can be omitted in a call only when it cannot get confused
with a following parameter of different type.

A music function must return a music expression."
  (define-syntax-function (ly:music? (make-music 'Music 'void #t)) elt ...))


(define-syntax-rule-public (define-scheme-function elt ...)
  "Like @code{define-music-function}, but the return type is not
restricted to music."
  (define-syntax-function scheme? elt ...))

(define-syntax-rule-public (define-void-function elt ...)
  "Like @code{define-music-function}, but the return value must be the
special @samp{*unspecified*} value (i.e., what most Guile functions
with @qq{unspecified} value return).  Use this when defining functions
for executing actions rather than returning values, to keep LilyPond
from trying to interpret the return value."
  (define-syntax-function (void? *unspecified*) elt ... *unspecified*))

(define-syntax-rule-public (define-event-function elt ...)
  "Like @code{define-music-function}, but the return value must be a
post-event."
  (define-syntax-function (ly:event? (make-music 'Event 'void #t)) elt ...))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Urgh.  This documentation string is completely incomprehensible – right
;; now, `quote-substitute` is undocumented.
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
            (ly:music-warning music (format #f (G_ "cannot find quoted music: `~S'") quoted-name))))
    music))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; switch it on here, so parsing and init isn't checked (too slow!)
;;
;; automatic music transformations.

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

(define (recompute-music-length music)
  ;; If the length property is set to a value inconsistent with the
  ;; length callback, correct it.  In other words, avoid setting the
  ;; length property when it doesn't need to be set.
  (let ((length-callback (ly:music-property music 'length-callback)))
    (if (procedure? length-callback)
        (let ((current-length (ly:music-property music 'length)))
          (if (ly:moment? current-length)
              (let ((new-length (length-callback music)))
                (if (not (eq? current-length new-length))
                    (set! (ly:music-property music 'length) new-length)))))))
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

(define (skip-as-needed music)
  "Replace @var{music} by
@example
<<
  @{
    \\set skipTypesetting = ##f
    \\skip %@{ length of \\showFirstLength %@}
    \\set skipTypesetting = ##t
    \\skip %@{ length of music not to be typeset @}
    \\set skipTypesetting = ##f
  @}
  @var{music}
>>
@end example
@noindent
if appropriate.

When only @code{showFirstLength} is set, the @code{length} property of
the music is overridden to speed up compiling."
  (let*
      ((show-last (ly:parser-lookup 'showLastLength))
       (show-first (ly:parser-lookup 'showFirstLength))
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
          (make-skipped skip-length #t)
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
          ;; ugh: setting a length inconsistent with the elements is
          ;; crude and fragile
          (set! (ly:music-property music 'length)
                show-first-length))
      (make-simultaneous-music
       (list
        (make-sequential-music
         (list
          (make-music 'SkippedMusic 'element show-first)
          ;; Continue slightly beyond the requested point to allow
          ;; Skip_typesetting_engraver to observe a t->f transition in
          ;; skipTypesetting and create a StaffEllipsis.
          (context-spec-music (make-property-set 'skipTypesetting #t) 'Score)
          (make-grace-music
           (make-music 'SkipMusic 'duration (ly:make-duration -10 0)))
          (context-spec-music (make-property-set 'skipTypesetting #f) 'Score)
          ))
        music
        )))

     (else music))))


(define-session-public toplevel-music-functions
  (list
   (lambda (music) (expand-repeat-chords!
                    (cons 'rhythmic-event
                          (ly:parser-lookup '$chord-repeat-events))
                    music))
   expand-repeat-notes!
   voicify-music
   (lambda (x) (music-map music-check-error x))
   (lambda (x) (music-map precompute-music-length x))
   (lambda (music)
     (music-map (quote-substitute (ly:parser-lookup 'musicQuotes))  music))

   (lambda (x) (music-map cue-substitute x))

   skip-as-needed))

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

(define (check-pitch-against-signature context pitch barnum laziness octaveness all-naturals)
  "Checks the need for an accidental and a @q{restore} accidental against
@code{localAlterations} and @code{keyAlterations}.
The @var{laziness} is the number of measures
for which reminder accidentals are used (i.e., if @var{laziness} is zero,
only cancel accidentals in the same measure; if @var{laziness} is three,
we cancel accidentals up to three measures after they first appear.
@var{octaveness} is either @code{'same-octave} or @code{'any-octave} and
specifies whether accidentals should be canceled in different octaves.
If @var{all-naturals} is ##t, notes that do not occur in @code{keyAlterations}
also get an accidental."
  (let* ((ignore-octave (cond ((equal? octaveness 'any-octave) #t)
                              ((equal? octaveness 'same-octave) #f)
                              (else
                               (ly:warning (G_ "Unknown octaveness type: ~S ") octaveness)
                               (ly:warning (G_ "Defaulting to 'any-octave."))
                               #t)))
         (key (ly:context-property context 'keyAlterations))
         (local (ly:context-property context 'localAlterations))
         (notename (ly:pitch-notename pitch))
         (octave (ly:pitch-octave pitch))
         (pitch-handle (cons octave notename))
         (need-restore #f)
         (need-accidental #f)
         (previous-alteration #f)
         (from-other-octaves #f)
         (from-same-octave (assoc-get pitch-handle local))
         (from-key-sig (or (assoc-get notename local)

                           ;; If no notename match is found from localAlterations, we may have a custom
                           ;; type with octave-specific entries of the form ((octave . pitch) alteration)
                           ;; instead of (pitch . alteration).  Since this type cannot coexist with entries in
                           ;; localAlterations, try extracting from keyAlterations instead.
                           (assoc-get pitch-handle key))))

    ;; loop through localAlterations to search for a notename match from other octaves
    (let loop ((l local))
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

          (if (or (and all-naturals (eq? #f previous-alteration)) (not (= this-alt prev-alt)))
              (begin
                (set! need-accidental #t)
                (if (and (not (= this-alt 0))
                         (and (< (abs this-alt) (abs prev-alt))
                              (> (* prev-alt this-alt) 0)))
                    (set! need-restore #t))))))

    (cons need-restore need-accidental)))

(define-public ((make-accidental-rule octaveness laziness) context pitch barnum)
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

  (check-pitch-against-signature context pitch barnum laziness octaveness #f))

(define-public ((make-accidental-dodecaphonic-rule octaveness laziness) context pitch barnum)
  "Variation on function make-accidental-rule that creates an dodecaphonic
accidental rule."

  (check-pitch-against-signature context pitch barnum laziness octaveness #t))

(define (key-entry-notename entry)
  "Return the pitch of an @var{entry} in @code{localAlterations}.
The @samp{car} of the entry is either of the form @code{notename} or
of the form @code{(octave . notename)}.  The latter form is used for special
key signatures or to indicate an explicit accidental.

The @samp{cdr} of the entry is either a rational @code{alter} indicating
a key signature alteration, or of the form
@code{(alter . (barnum . end-mom))} indicating an alteration caused by
an accidental in music."
  (if (pair? (car entry))
      (cdar entry)
      (car entry)))

(define (key-entry-octave entry)
  "Return the octave of an entry in @code{localAlterations}
or @code{#f} if the entry does not have an octave.
See @code{key-entry-notename} for details."
  (and (pair? (car entry)) (caar entry)))

(define (key-entry-bar-number entry)
  "Return the bar number of an entry in @code{localAlterations}
or @code{#f} if the entry does not have a bar number.
See @code{key-entry-notename} for details."
  (and (pair? (cdr entry)) (caddr entry)))

(define (key-entry-end-mom entry)
  "Return the end moment of an entry in @code{localAlterations}
or @code{#f} if the entry does not have an end moment.
See @code{key-entry-notename} for details."
  (and (pair? (cdr entry)) (cdddr entry)))

(define (key-entry-alteration entry)
  "Return the alteration of an entry in localAlterations

For convenience, returns @code{0} if entry is @code{#f}."
  (if entry
      (if (number? (cdr entry))
          (cdr entry)
          (cadr entry))
      0))

(define-public (find-pitch-entry keysig pitch accept-global accept-local)
  "Return the first entry in @var{keysig} that matches @var{pitch}
by notename and octave.  Alteration is not considered.
@var{accept-global} states whether key signature entries should be included.
@var{accept-local} states whether local accidentals should be included.
If no matching entry is found, @code{#f} is returned."
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

(define-public (neo-modern-accidental-rule context pitch barnum)
  "An accidental rule that typesets an accidental if it differs from the
key signature @emph{and} does not directly follow a note on the same
staff line.  This rule should not be used alone because it does neither
look at bar lines nor different accidentals at the same note name."
  (let* ((keysig (ly:context-property context 'localAlterations))
         (entry (find-pitch-entry keysig pitch #t #t)))
    (if (not entry)
        (cons #f #f)
        (let* ((global-entry (find-pitch-entry keysig pitch #t #f))
               (key-acc (key-entry-alteration global-entry))
               (acc (ly:pitch-alteration pitch))
               (entry-end-mom (key-entry-end-mom entry))
               (entry-bn (key-entry-bar-number entry))
               (now (ly:context-current-moment context)))
          (cons #f (not (or (equal? acc key-acc)
                            (and (equal? entry-bn barnum)
                                 (equal? entry-end-mom now)))))))))

(define-public (dodecaphonic-no-repeat-rule context pitch barnum)
  "An accidental rule that typesets an accidental before every
note (just as in the dodecaphonic accidental style) @emph{except} if
the note is immediately preceded by a note with the same pitch. This
is a common accidental style in contemporary notation."
  (let* ((keysig (ly:context-property context 'localAlterations))
         (entry (find-pitch-entry keysig pitch #f #t)))
    (if (not entry)
        (cons #f #t)
        (let ((entry-end-mom (key-entry-end-mom entry))
              (entry-bn (key-entry-bar-number entry))
              (entry-alt (key-entry-alteration entry))
              (alt (ly:pitch-alteration pitch))
              (now (ly:context-current-moment context)))
          (cons #t ; FIXME: why is this different from dodecaphonic? --JeanAS
                (not (and (equal? entry-bn barnum)
                          (or (equal? now entry-end-mom)
                              (ly:moment<? now entry-end-mom))
                          (equal? entry-alt alt))))))))

(define-public (teaching-accidental-rule context pitch barnum)
  "An accidental rule that typesets a cautionary accidental if it is
included in the key signature @emph{and} does not directly follow a note
on the same staff line."
  (let* ((keysig (ly:context-property context 'localAlterations))
         (entry (find-pitch-entry keysig pitch #t #t))
         (now (ly:context-current-moment context)))
    (if (not entry)
        (cons #f #f)
        (let* ((global-entry (find-pitch-entry keysig pitch #f #f))
               (key-acc (key-entry-alteration global-entry))
               (acc (ly:pitch-alteration pitch))
               (entry-end-mom (key-entry-end-mom entry))
               (entry-bn (key-entry-bar-number entry))
               (now (ly:context-current-moment context)))
          (cons #f (not (or (equal? acc key-acc)
                            (and (equal? entry-bn barnum)
                                 (equal? entry-end-mom now)))))))))

(define-session-public accidental-styles
  ;; An alist containing specification for all accidental styles.
  ;; Each accidental style needs three entries for the context properties
  ;; extraNatural, autoAccidentals and autoCautionaries.
  ;; An optional fourth entry may specify a default context for the accidental
  ;; style, for use with the piano styles.
  `(
    ;; accidentals as they were common in the 18th century.
    (default #t
      (Staff ,(make-accidental-rule 'same-octave 0))
      ())
    ;; accidentals from one voice do NOT get canceled in other voices
    (voice #t
           (Voice ,(make-accidental-rule 'same-octave 0))
           ())
    ;; accidentals as suggested by Kurt Stone in
    ;; `Music Notation in the 20th century'.
    ;; This includes all the default accidentals, but accidentals also need
    ;; canceling in other octaves and in the next measure.
    (modern #f
            (Staff ,(make-accidental-rule 'same-octave 0)
                   ,(make-accidental-rule 'any-octave 0)
                   ,(make-accidental-rule 'same-octave 1))
            ())
    ;; the accidentals that Stone adds to the old standard as cautionaries
    (modern-cautionary #f
                       (Staff ,(make-accidental-rule 'same-octave 0))
                       (Staff ,(make-accidental-rule 'any-octave 0)
                              ,(make-accidental-rule 'same-octave 1)))
    ;; same as modern, but accidentals different from the key signature are
    ;; always typeset - unless they directly follow a note of the same pitch.
    (neo-modern #f
                (Staff ,(make-accidental-rule 'same-octave 0)
                       ,(make-accidental-rule 'any-octave 0)
                       ,(make-accidental-rule 'same-octave 1)
                       ,neo-modern-accidental-rule)
                ())
    (neo-modern-cautionary #f
                           (Staff ,(make-accidental-rule 'same-octave 0))
                           (Staff ,(make-accidental-rule 'any-octave 0)
                                  ,(make-accidental-rule 'same-octave 1)
                                  ,neo-modern-accidental-rule))
    (neo-modern-voice #f
                      (Voice ,(make-accidental-rule 'same-octave 0)
                             ,(make-accidental-rule 'any-octave 0)
                             ,(make-accidental-rule 'same-octave 1)
                             ,neo-modern-accidental-rule
                             Staff
                             ,(make-accidental-rule 'same-octave 0)
                             ,(make-accidental-rule 'any-octave 0)
                             ,(make-accidental-rule 'same-octave 1)
                             ,neo-modern-accidental-rule)
                      ())
    (neo-modern-voice-cautionary #f
                                 (Voice ,(make-accidental-rule 'same-octave 0))
                                 (Voice ,(make-accidental-rule 'any-octave 0)
                                        ,(make-accidental-rule 'same-octave 1)
                                        ,neo-modern-accidental-rule
                                        Staff
                                        ,(make-accidental-rule 'same-octave 0)
                                        ,(make-accidental-rule 'any-octave 0)
                                        ,(make-accidental-rule 'same-octave 1)
                                        ,neo-modern-accidental-rule))

    ;; Accidentals as they were common in dodecaphonic music with no tonality.
    ;; Each note gets one accidental.
    (dodecaphonic #f
                  (Staff ,(lambda (c p bn) '(#f . #t)))
                  ())
    ;; As in dodecaphonic style with the exception that immediately
    ;; repeated notes (in the same voice) don't get an accidental
    (dodecaphonic-no-repeat #f
                            (Staff ,dodecaphonic-no-repeat-rule)
                            ())
    ;; Variety of the dodecaphonic style. Each note gets an accidental,
    ;; except notes that were already handled in the same measure.
    (dodecaphonic-first #f
                        (Staff ,(make-accidental-dodecaphonic-rule 'same-octave 0))
                        ())

    ;; Multivoice accidentals to be read both by musicians playing one voice
    ;; and musicians playing all voices. Accidentals are typeset for each
    ;; voice, but they ARE canceled across voices.
    (modern-voice #f
                  (Voice ,(make-accidental-rule 'same-octave 0)
                         ,(make-accidental-rule 'any-octave 0)
                         ,(make-accidental-rule 'same-octave 1)
                         Staff
                         ,(make-accidental-rule 'same-octave 0)
                         ,(make-accidental-rule 'any-octave 0)
                         ,(make-accidental-rule 'same-octave 1))
                  ())
    ;; same as modernVoiceAccidental except that all special accidentals
    ;; are typeset as cautionaries
    (modern-voice-cautionary #f
                             (Voice ,(make-accidental-rule 'same-octave 0))
                             (Voice ,(make-accidental-rule 'any-octave 0)
                                    ,(make-accidental-rule 'same-octave 1)
                                    Staff
                                    ,(make-accidental-rule 'same-octave 0)
                                    ,(make-accidental-rule 'any-octave 0)
                                    ,(make-accidental-rule 'same-octave 1)))

    ;; Stone's suggestions for accidentals on grand staff.
    ;; Accidentals are canceled across the staves
    ;; in the same grand staff as well
    (piano #f
           (Staff ,(make-accidental-rule 'same-octave 0)
                  ,(make-accidental-rule 'any-octave 0)
                  ,(make-accidental-rule 'same-octave 1)
                  GrandStaff
                  ,(make-accidental-rule 'any-octave 0)
                  ,(make-accidental-rule 'same-octave 1))
           ()
           GrandStaff)
    (piano-cautionary #f
                      (Staff ,(make-accidental-rule 'same-octave 0))
                      (Staff ,(make-accidental-rule 'any-octave 0)
                             ,(make-accidental-rule 'same-octave 1)
                             GrandStaff
                             ,(make-accidental-rule 'any-octave 0)
                             ,(make-accidental-rule 'same-octave 1))
                      GrandStaff)

    ;; Accidentals on a choir staff for simultaneous reading of the
    ;; own voice and the surrounding choir. Similar to piano, except
    ;; that the first alteration within a voice is always printed.
    (choral #f
            (Voice ,(make-accidental-rule 'same-octave 0)
                   Staff
                   ,(make-accidental-rule 'same-octave 1)
                   ,(make-accidental-rule 'any-octave 0)
                   ,(make-accidental-rule 'same-octave 1)
                   ChoirStaff
                   ,(make-accidental-rule 'any-octave 0)
                   ,(make-accidental-rule 'same-octave 1))
            ()
            ChoirStaff)
    (choral-cautionary #f
                       (Voice ,(make-accidental-rule 'same-octave 0)
                              Staff
                              ,(make-accidental-rule 'same-octave 0))
                       (Staff ,(make-accidental-rule 'any-octave 0)
                              ,(make-accidental-rule 'same-octave 1)
                              ChoirStaff
                              ,(make-accidental-rule 'any-octave 0)
                              ,(make-accidental-rule 'same-octave 1))
                       ChoirStaff)

    ;; same as modern, but cautionary accidentals are printed for all
    ;; non-natural tones specified by the key signature.
    (teaching #f
              (Staff ,(make-accidental-rule 'same-octave 0))
              (Staff ,(make-accidental-rule 'same-octave 1)
                     ,teaching-accidental-rule))

    ;; do not set localAlterations when a note alterated differently from
    ;; localAlterations is found.
    ;; Causes accidentals to be printed at every note instead of
    ;; remembered for the duration of a measure.
    ;; accidentals not being remembered, causing accidentals always to
    ;; be typeset relative to the time signature
    (forget ()
            (Staff ,(make-accidental-rule 'same-octave -1))
            ())
    ;; Do not reset the key at the start of a measure.  Accidentals will be
    ;; printed only once and are in effect until overridden, possibly many
    ;; measures later.
    (no-reset ()
              (Staff ,(make-accidental-rule 'same-octave #t))
              ())
    ))

(define-public (set-accidental-style style . rest)
  "Set accidental style to @var{style}.  Optionally take a context
argument, e.g., @code{'Staff} or @code{'Voice}.  The context defaults
to @code{Staff}, except for piano styles, which use @code{GrandStaff}
as a context."
  (let ((spec (assoc-get style accidental-styles)))
    (if spec
        (let ((extra-natural (first spec))
              (auto-accs (second spec))
              (auto-cauts (third spec))
              (context (cond ((pair? rest) (car rest))
                             ((= 4 (length spec)) (fourth spec))
                             (else 'Staff))))
          (context-spec-music
           (make-sequential-music
            (append (if (boolean? extra-natural)
                        (list (make-property-set 'extraNatural extra-natural))
                        '())
                    (list (make-property-set 'autoAccidentals auto-accs)
                          (make-property-set 'autoCautionaries auto-cauts))))
           context))
        (begin
          (ly:warning (G_ "unknown accidental style: ~S") style)
          (make-sequential-music '())))))

(define-public (invalidate-alterations context)
  "Invalidate alterations in @var{context}.

Elements of @code{'localAlterations} corresponding to local
alterations of the key signature have the form
@code{'((octave . notename) . (alter barnum . end-mom))}.
Replace them with a version where @code{alter} is set to @code{'clef}
to force a repetition of accidentals.

Entries that conform with the current key signature are not invalidated."
  (let* ((keysig (ly:context-property context 'keyAlterations)))
    (set! (ly:context-property context 'localAlterations)
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
           (ly:context-property context 'localAlterations)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (ly:music-compress mus scale)
  "Compress @var{mus} by @var{scale}."
  (let ((factor (scale->factor scale)))
    ;; compress the 'duration property of all elements recursively
    (for-some-music
     (lambda (m)
       (let ((d (ly:music-property m 'duration)))
         (if (ly:duration? d)
             (set! (ly:music-property m 'duration)
                   (ly:duration-compress d factor)))
         #f))
     mus)
    mus))

(define-public (skip-of-length mus)
  "Create a skip of exactly the same length as @var{mus}."
  (let* ((skip
          (make-music
           'SkipEvent
           'duration (ly:make-duration 0 0))))

    (make-event-chord (list (ly:music-compress skip (ly:music-length mus))))))

(define-public (skip-of-moment-span start-moment end-moment)
  "Make skip music fitting between @var{start-moment} and
@var{end-moment}.  The grace part of @var{end-moment} matters only if
@var{start-moment} and @var{end-mom} have the same main part."
  (let ((delta-moment (ly:moment-sub end-moment start-moment)))
    (if (zero? (ly:moment-main delta-moment))
        ;; start and end have same main part
        (if (zero? (ly:moment-grace delta-moment))
            ;; neither main time nor grace time
            (make-skip-music ZERO-DURATION)
            ;; grace time only
            (make-grace-music
             (make-skip-music
              (ly:make-duration 0 0 (ly:moment-grace delta-moment)))))
        ;; start and end have different main parts
        (if (zero? (ly:moment-grace start-moment))
            ;; main time only
            (make-skip-music (make-duration-of-length delta-moment))
            ;; grace time and main time
            (make-sequential-music
             (list
              (make-grace-music
               (make-skip-music
                (ly:make-duration 0 0 (- (ly:moment-grace start-moment)))))
              (make-skip-music
               (ly:make-duration 0 0 (ly:moment-main delta-moment)))))))))

(define-public (mmrest-of-length mus)
  "Create a multi-measure rest of exactly the same length as @var{mus}."

  (let* ((skip
          (make-multi-measure-rest
           (ly:make-duration 0 0) '())))
    (ly:music-compress skip (ly:music-length mus))
    skip))

(define-public (pitch-of-note event-chord)
  (let ((evs (filter (lambda (x)
                       (music-is-of-type? x 'note-event))
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
  (extract-music music (music-type-predicate type)))

(define-public (event-chord-wrap! music)
  "Wrap isolated rhythmic events and non-postevent events in
@var{music} inside of an @code{EventChord}.  Chord repeats @samp{q}
are expanded using the default settings of the parser."
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
   (expand-repeat-notes!
    (expand-repeat-chords!
     (cons 'rhythmic-event
           (ly:parser-lookup '$chord-repeat-events))
     music))))

(define-public (event-chord-notes event-chord)
  "Return a list of all notes from @var{event-chord}."
  (filter
   (lambda (m) (eq? 'NoteEvent (ly:music-property m 'name)))
   (ly:music-property event-chord 'elements)))

(define-public (event-chord-pitches event-chord)
  "Return a list of all pitches from @var{event-chord}."
  (map (lambda (x) (ly:music-property x 'pitch))
       (event-chord-notes event-chord)))

(define-public (music-pitches music)
  "Return a list of all pitches from @var{music}."
  ;; Opencoded for efficiency.
  (reverse!
   (let loop ((music music) (pitches '()))
     (let ((p (ly:music-property music 'pitch)))
       (if (ly:pitch? p)
           (cons p pitches)
           (let ((elt (ly:music-property music 'element)))
             (fold loop
                   (if (ly:music? elt)
                       (loop elt pitches)
                       pitches)
                   (ly:music-property music 'elements))))))))

(define-public (event-chord-reduce music)
  "Reduce event chords in @var{music} to their first note event,
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
                   ;; This is an empty chord.  Ugh.  We cannot really
                   ;; reduce this in any manner, so we just keep it.
                   m)))))
   music))

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

(define-syntax-rule-public (make-relative (variables ...) reference music)
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
  (make-music 'RelativeOctaveMusic
              'to-relative-callback
              (make-relative::to-relative-callback
               (list variables ...)
               (lambda (variables ...)
                 music)
               (lambda (variables ...)
                 reference))
              'element music))

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
  "Connect cross-staff stems to the stems above in the system."
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

(define-public (value-for-spanner-piece property args)
  "Associate a piece of broken spanner @var{grob} with an element
of list @var{arg}."
  (define ((worker caller) grob . rest)
    (if (ly:spanner? grob)
        (let* ((orig (ly:grob-original grob))
               (siblings (if (eq? orig grob)
                             (list grob)
                             (ly:spanner-broken-into orig))))
          (let loop ((args args) (siblings siblings))
            (cond
             ((null? args)
              '())
             ((eq? grob (car siblings))
              (let ((val (car args)))
                (apply caller val grob rest)))
             (else
              (loop (cdr args)
                    (cdr siblings))))))
        (ly:grob-warning grob
                         property
                         "this grob is not a spanner")))
  (ly:make-unpure-pure-container
   (worker ly:unpure-call)
   (worker ly:pure-call)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following are used by the \offset function

(define (find-value-to-offset grob prop self alist)
  "Return the first value of the property @var{prop} in the property
alist @var{alist} -- after having found @var{self}.  If @var{self} is
not found, return the first value of @var{prop}."
  (let* ((lookfor (cons prop self))
         (segment (member lookfor alist)))
    (if (not segment)
        (assoc-get prop alist)
        (if (member lookfor (cdr segment))
            (begin
              (ly:grob-warning grob prop (G_ "giving up on cloned grob transform"))
              (find-value-to-offset grob prop self (cdr segment)))
            (assoc-get prop (cdr segment))))))

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
    (map coord-translate arg offsets))
   (else arg)))

(define-public (grob-transformer property func)
  "Create an override value good for applying @var{func} to either
pure or unpure values.  @var{func} is called with the respective grob
as first argument and the default value (after resolving all callbacks)
as the second."
  (define (worker self caller grob . rest)
    (let* ((immutable (ly:grob-basic-properties grob))
           ;; We need to search the basic-properties alist for our
           ;; property to obtain values to offset.  Our search is
           ;; complicated by the fact that calling the music function
           ;; `offset' as an override conses a pair to the head of the
           ;; alist.  This pair must be discounted.  The closure it
           ;; contains is named `self' so it can be easily recognized.
           ;; If `offset' is called as a tweak, the basic-property
           ;; alist is unaffected.
           (target (find-value-to-offset grob property self immutable))
           ;; if target is a procedure, we need to apply it to our
           ;; grob to calculate values to offset.
           (vals (apply caller target grob rest)))
      (func grob vals)))
  ;; return the container named `self'.  The container self-reference
  ;; seems like chasing its own tail but gets dissolved by
  ;; define/lambda separating binding and referencing of "self".
  (define self (ly:make-unpure-pure-container
                (lambda (grob)
                  (worker self ly:unpure-call grob))
                (lambda (grob . rest)
                  (apply worker self ly:pure-call grob rest))))
  self)

(define-public (offsetter property offsets)
  "Apply @var{offsets} to the default values of @var{property} of @var{grob}.
Offsets are restricted to immutable properties and values of type @code{number},
@code{number-pair}, or @code{number-pair-list}."
  (define (offset-fun grob vals)
    (let ((can-type-be-offset?
           (or (number? vals)
               (number-pair? vals)
               (number-pair-list? vals))))
      (if can-type-be-offset?
          ;; '(+inf.0 . -inf.0) would offset to itself.  This will be
          ;; confusing to a user unaware of the default value of the
          ;; property, so issue a warning.
          (if (equal? empty-interval vals)
              (ly:warning (G_ "default '~a of ~a is ~a and can't be offset")
                          property grob vals)
              (let* ((orig (ly:grob-original grob))
                     (siblings
                      (if (ly:spanner? grob)
                          (ly:spanner-broken-into orig)
                          '()))
                     (total-found (length siblings))
                     ;; Since there is some flexibility in input
                     ;; syntax, structure of `offsets' is normalized.
                     (offsets
                      (if (or (not (pair? offsets))
                              (number-pair? offsets)
                              (and (number-pair-list? offsets)
                                   (number-pair-list? vals)))
                          (list offsets)
                          offsets)))

                (define (helper sibs offs)
                  ;; apply offsets to the siblings of broken spanners
                  (if (pair? offs)
                      (if (eq? (car sibs) grob)
                          (offset-multiple-types vals (car offs))
                          (helper (cdr sibs) (cdr offs)))
                      vals))

                (if (>= total-found 2)
                    (helper siblings offsets)
                    (offset-multiple-types vals (car offsets)))))

          (begin
            (ly:warning (G_ "the property '~a of ~a cannot be offset") property grob)
            vals))))
  (grob-transformer property offset-fun))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; \magnifyMusic and \magnifyStaff

;; defined as a function instead of a list because the
;; all-grob-descriptions alist is not available yet
(define-public (find-named-props prop-name grob-descriptions)
  "Used by @code{\\magnifyMusic} and @code{\\magnifyStaff}.  If
@var{grob-descriptions} is equal to the @code{all-grob-descriptions}
alist (defined in @file{scm/define-grobs.scm}), this finds all grobs
that can have a value for the @var{prop-name} property, and return them
as a list in the following format:
@example
'((grob prop-name)
  (grob prop-name)
  ...)
@end example"
  (define (find-grobs-with-interface interface grob-descriptions)
    (define (has-this-interface? grob-desc)
      (let* ((meta (ly:assoc-get 'meta (cdr grob-desc)))
             (interfaces (ly:assoc-get 'interfaces meta '())))
        (memq interface interfaces)))
    (let* ((grob-descriptions-with-this-interface
            (filter has-this-interface? grob-descriptions))
           (grob-names-with-this-interface
            (map car grob-descriptions-with-this-interface)))
      grob-names-with-this-interface))
  (let* ((interface
          (case prop-name
            ((baseline-skip word-space) 'text-interface)
            ((space-alist)              'break-aligned-interface)
            (else (ly:programming-error
                   "find-named-props: no interface associated with ~s"
                   prop-name))))
         (grobs-with-this-prop
          (find-grobs-with-interface interface grob-descriptions)))
    (map (lambda (x) (list x prop-name))
         grobs-with-this-prop)))


(define (magnifyStaff-is-set? context mag)
  (let* ((Staff (ly:context-find context 'Staff))
         (old-mag (ly:context-property Staff 'magnifyStaffValue)))
    (not (null? old-mag))))

(define (staff-magnification-is-changing? context mag)
  (let* ((Staff (ly:context-find context 'Staff))
         (old-mag (ly:context-property Staff 'magnifyStaffValue 1)))
    (not (= old-mag mag))))

(define-public (scale-fontSize func-name mag)
  "Used by @code{\\magnifyMusic} and @code{\\magnifyStaff}.  Look up the
current @code{fontSize} in the appropriate context and scale it by the
magnification factor @var{mag}.  @var{func-name} is either
@code{'magnifyMusic} or @code{'magnifyStaff}."
  (make-apply-context
   (lambda (context)
     (if (or (eq? func-name 'magnifyMusic)
             ;; for \magnifyStaff, only scale the fontSize
             ;; if staff magnification is changing
             ;; and does not equal 1
             (and (staff-magnification-is-changing? context mag)
                  (not (= mag 1))))
         (let* ((where (case func-name
                         ((magnifyMusic) context)
                         ((magnifyStaff) (ly:context-find context 'Staff))))
                (fontSize (ly:context-property where 'fontSize 0))
                (new-fontSize (+ fontSize (magnification->font-size mag))))
           (ly:context-set-property! where 'fontSize new-fontSize))))))

(define-public (revert-fontSize func-name mag)
  "Used by @code{\\magnifyMusic} and @code{\\magnifyStaff}.  Calculate
the previous @code{fontSize} value (before scaling) by factoring out the
magnification factor @var{mag} (if @var{func-name} is
@code{'magnifyMusic}), or by factoring out the context property
@code{magnifyStaffValue} (if @var{func-name} is @code{'magnifyStaff}).
Revert the @code{fontSize} in the appropriate context accordingly.

With @code{\\magnifyMusic}, the scaling is reverted after the music
block it operates on.  @code{\\magnifyStaff} does not operate on a music
block, so the scaling from a previous call (if there is one) is reverted
before the new scaling takes effect."
  (make-apply-context
   (lambda (context)
     (if (or (eq? func-name 'magnifyMusic)
             ;; for \magnifyStaff...
             (and
              ;; don't revert the user's fontSize choice
              ;; the first time \magnifyStaff is called
              (magnifyStaff-is-set? context mag)
              ;; only revert the previous fontSize
              ;; if staff magnification is changing
              (staff-magnification-is-changing? context mag)))
         (let* ((where
                 (case func-name
                   ((magnifyMusic) context)
                   ((magnifyStaff) (ly:context-find context 'Staff))))
                (old-mag
                 (case func-name
                   ((magnifyMusic) mag)
                   ((magnifyStaff)
                    (ly:context-property where 'magnifyStaffValue 1))))
                (fontSize (ly:context-property where 'fontSize 0))
                (old-fontSize (- fontSize (magnification->font-size old-mag))))
           (ly:context-set-property! where 'fontSize old-fontSize))))))

(define-public (scale-props func-name mag allowed-to-shrink? props)
  "Used by @code{\\magnifyMusic} and @code{\\magnifyStaff}.  For each
prop in @var{props}, find the current value of the requested prop, scale
it by the magnification factor @var{mag}, and do the equivalent of a
@code{\\temporary@tie{}\\override} with the new value in the appropriate
context.  If @var{allowed-to-shrink?} is @code{#f}, don't let the new
value be less than the current value.  @var{func-name} is either
@code{'magnifyMusic} or @code{'magnifyStaff}.  The @var{props} list is
formatted like:
@example
'((Stem thickness)
  (Slur line-thickness)
  ...)
@end example"
  (make-apply-context
   (lambda (context)
     (define (scale-prop grob-prop-list)
       (let* ((grob (car grob-prop-list))
              (prop (cadr grob-prop-list))
              (where (if (eq? grob 'SpacingSpanner)
                         (ly:context-find context 'Score)
                         (case func-name
                           ((magnifyMusic) context)
                           ((magnifyStaff) (ly:context-find context 'Staff)))))
              (grob-def (ly:context-grob-definition where grob)))
         (if (eq? prop 'space-alist)
             (let* ((space-alist (ly:assoc-get prop grob-def))
                    (scale-spacing-tuple (lambda (x)
                                           (cons (car x)
                                                 (cons (cadr x)
                                                       (* mag (cddr x))))))
                    (scaled-tuples (if space-alist
                                       (map scale-spacing-tuple space-alist)
                                       '()))
                    (new-alist (append scaled-tuples space-alist)))
               (ly:context-pushpop-property where grob prop new-alist))
             (let* ((val (ly:assoc-get prop grob-def (case prop
                                                       ((baseline-skip) 3)
                                                       ((word-space)    0.6)
                                                       (else            1))))
                    (proc (lambda (x)
                            (if allowed-to-shrink?
                                (* x mag)
                                (* x (max 1 mag)))))
                    (new-val (if (number-pair? val)
                                 (cons (proc (car val))
                                       (proc (cdr val)))
                                 (proc val))))
               (ly:context-pushpop-property where grob prop new-val)))))
     (if (or (eq? func-name 'magnifyMusic)
             ;; for \magnifyStaff, only scale the properties
             ;; if staff magnification is changing
             ;; and does not equal 1
             (and (staff-magnification-is-changing? context mag)
                  (not (= mag 1))))
         (for-each scale-prop props)))))

(define-public (revert-props func-name mag props)
  "Used by @code{\\magnifyMusic} and @code{\\magnifyStaff}.  Revert each
prop in @var{props} in the appropriate context.  @var{func-name} is
either @code{'magnifyMusic} or @code{'magnifyStaff}.  The @var{props}
list is formatted like:
@example
'((Stem thickness)
  (Slur line-thickness)
  ...)
@end example"
  (make-apply-context
   (lambda (context)
     (define (revert-prop grob-prop-list)
       (let* ((grob (car grob-prop-list))
              (prop (cadr grob-prop-list))
              (where (if (eq? grob 'SpacingSpanner)
                         (ly:context-find context 'Score)
                         (case func-name
                           ((magnifyMusic) context)
                           ((magnifyStaff) (ly:context-find context 'Staff))))))
         (ly:context-pushpop-property where grob prop)))
     (if (or (eq? func-name 'magnifyMusic)
             ;; for \magnifyStaff...
             (and
              ;; don't revert the user's property overrides
              ;; the first time \magnifyStaff is called
              (magnifyStaff-is-set? context mag)
              ;; revert the overrides from the previous \magnifyStaff,
              ;; but only if staff magnification is changing
              (staff-magnification-is-changing? context mag)))
         (for-each revert-prop props)))))

;; \magnifyMusic only
(define-public (scale-beam-thickness mag)
  "Used by @code{\\magnifyMusic}.  Scaling @code{Beam.beam-thickness}
exactly to the @var{mag} value will not work.  This uses two reference
values for @code{beam-thickness} to determine an acceptable value when
scaling, then does the equivalent of a
@code{\\temporary@tie{}\\override} with the new value."
  (make-apply-context
   (lambda (context)
     (let* ((grob-def (ly:context-grob-definition context 'Beam))
            (val (ly:assoc-get 'beam-thickness grob-def 0.48))
            (ratio-to-default (/ val 0.48))
            ;; gives beam-thickness=0.48 when mag=1 (like default),
            ;; gives beam-thickness=0.35 when mag=0.63 (like CueVoice)
            (scaled-default (+ 119/925 (* mag 13/37)))
            (new-val (* scaled-default ratio-to-default)))
       (ly:context-pushpop-property context 'Beam 'beam-thickness new-val)))))

;; tag management
;;

(define tag-groups (make-hash-table))

(define-public (define-tag-group tags)
  "Define a tag group consisting of the given @var{tags}, a@tie{}list
of symbols.  Returns @code{#f} if successful, and an error message if
there is a conflicting tag group definition."
  (cond ((not (symbol-list? tags)) (format #f (G_ "not a symbol list: ~a") tags))
        ((any (lambda (tag) (hashq-ref tag-groups tag)) tags)
         => (lambda (group) (and (not (lset= eq? group tags))
                                 (format #f (G_ "conflicting tag group ~a") group))))
        (else
         (for-each
          (lambda (elt) (hashq-set! tag-groups elt tags))
          tags)
         #f)))

;; Isolate LilyPond's internal tags from the user's tags.
(define-tag-group '($autoChange))
(define-tag-group '($partCombine))

;; Save the default tag groups and restore them after every session.
(define default-tag-groups (hash-table->alist tag-groups))
(call-after-session (lambda ()
                      (set! tag-groups (alist->hash-table default-tag-groups))))

(define-public (tag-group-get tag)
  "Return the tag group (as a list of symbols) that the given
@var{tag} symbol belongs to, @code{#f} if none."
  (hashq-ref tag-groups tag))

(define-public (tags-remove-predicate tags)
  "Return a predicate that returns @code{#f} for any music that is to
be removed by @code{\\removeWithTag} on the given symbol or list of
symbols @var{tags}."
  (if (symbol? tags)
      (lambda (m)
        (not (memq tags (ly:music-property m 'tags))))
      (lambda (m)
        (not (any (lambda (t) (memq t tags))
                  (ly:music-property m 'tags))))))

(define-public (tags-keep-predicate tags)
  "Return a predicate that returns @code{#f} for any music that is to
be removed by @code{\\keepWithTag} on the given symbol or list of symbols
@var{tags}."
  (if (symbol? tags)
      (let ((group (tag-group-get tags)))
        (lambda (m)
          (let ((music-tags (ly:music-property m 'tags)))
            (or
             (null? music-tags) ; redundant but very frequent
             ;; We know of only one tag to keep.  Either we find it in
             ;; the music tags, or all music tags must be from a
             ;; different group
             (memq tags music-tags)
             (not (any (lambda (t) (eq? (tag-group-get t) group)) music-tags))))))
      (let ((groups (delete-duplicates (map tag-group-get tags) eq?)))
        (lambda (m)
          (let ((music-tags (ly:music-property m 'tags)))
            (or
             (null? music-tags) ; redundant but very frequent
             (any (lambda (t) (memq t tags)) music-tags)
             ;; if no tag matches, no tag group should match either
             (not (any (lambda (t) (memq (tag-group-get t) groups)) music-tags))))))))
