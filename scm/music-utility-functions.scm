;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 1998--2026 Jan Nieuwenhuizen <janneke@gnu.org>
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


(use-modules (lily curried-definitions)
             (ice-9 optargs))

(define-public (make-grob-property-set grob gprop val)
  "Make a @code{Music} expression that overrides a @var{gprop} to
@var{val} in @var{grob}.  Does a pop first, i.e., this is not a
@code{\\temporary \\override}."
  (make-music 'OverrideProperty
              'symbol grob
              'grob-property-path (list gprop)
              'grob-value val
              'pop-first #t))

(define-public (make-grob-property-override grob gprop val)
  "Make a @code{Music} expression that overrides @var{gprop} to
@var{val} in @var{grob}.  This is a @code{\\temporary \\override},
making it possible to @code{\\revert} to any previous value afterwards."
  (make-music 'OverrideProperty
              'symbol grob
              'grob-property-path (list gprop)
              'grob-value val))

(define-public (make-grob-property-revert grob gprop)
  "Revert the grob property @var{gprop} for @var{grob}."
  (make-music 'RevertProperty
              'symbol grob
              'grob-property-path (list gprop)))

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

(define*-public (ascend-to-context m context #:optional id mods)
  "Like @code{context-spec-music}, but only ascending."
  (let ((cm (context-spec-music m context id mods)))
    (ly:music-set-property! cm 'search-direction UP)
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

(define-public (make-initial-context-music music)
  (make-music 'InitialContextMusic
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

;;; splitting chords into voices.
(define (voicify-list locs lst id)
  "Make a list of Musics.

voicify-list :: [ [Music ] ] -> id -> [Music]
LST is a list music-lists.

id is 1-based, i.e., Voice=1 (up-stems) has number 1.

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

; Make a function that checks score element for being of a specific type.
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

(define-public (skip-of-length mus)
  "Create a skip of exactly the same length as @var{mus}."
  (let* ((skip
          (make-music
           'SkipEvent
           'duration (ly:make-duration 0 0))))
    (ly:music-compress skip (ly:music-length mus))))
