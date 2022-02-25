;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2004--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

;; todo: figure out how to make module,
;; without breaking nested ly scopes

(define-class <Voice-state> ()
  (event-list #:init-value '() #:accessor events #:init-keyword #:events)
  (when-moment #:accessor moment #:init-keyword #:moment)
  (tuning #:accessor tuning #:init-keyword #:tuning)
  (split-index #:accessor split-index)
  (vector-index)
  (state-vector)
  ;;;
  ;; spanner-state is an alist
  ;; of (SYMBOL . RESULT-INDEX), which indicates where
  ;; said spanner was started.
  (spanner-state #:init-value '() #:accessor span-state))

(define-method (write (x <Voice-state> ) file)
  (display (moment x) file)
  (display " evs = " file)
  (display (events x) file)
  (display " active = " file)
  (display (span-state x) file)
  (display "\n" file))

;; Return the duration of the longest event in the Voice-state.
(define-method (duration (vs <Voice-state>))
  (define (duration-max event d1)
    (let ((d2 (ly:event-property event 'duration #f)))
      (if d2
          (if (ly:duration<? d1 d2) d2 d1)
          d1)))

  (fold duration-max ZERO-DURATION (events vs)))

(define-method (note-events (vs <Voice-state>))
  (define (f? x)
    (ly:in-event-class? x 'note-event))
  (filter f? (events vs)))

;; Return a list of note events which is sorted and stripped of
;; properties that we do not want to prevent combining parts.
(define-method (comparable-note-events (vs <Voice-state>))
  (define (note<? note1 note2)
    (let ((p1 (ly:event-property note1 'pitch))
          (p2 (ly:event-property note2 'pitch)))
      (cond ((ly:pitch<? p1 p2) #t)
            ((ly:pitch<? p2 p1) #f)
            (else (ly:duration<? (ly:event-property note1 'duration)
                                 (ly:event-property note2 'duration))))))
  ;; TODO we probably should compare articulations too
  (sort (map (lambda (x)
               (ly:make-stream-event
                (ly:make-event-class 'note-event)
                (list (cons 'duration (ly:event-property x 'duration))
                      (cons 'pitch (ly:event-property x 'pitch)))))
             (note-events vs))
        note<?))

(define-method (silence-events (vs <Voice-state>))
  (let ((result (filter (lambda(x)
                          (or (ly:in-event-class? x 'rest-event)
                              (ly:in-event-class? x 'multi-measure-rest-event)))
                        (events vs))))
    ;; There may be skips in the same part with rests for various
    ;; reasons.  Regard the skips only if there are no rests.
    (if (not (pair? result))
        (set! result (filter (lambda(x) (ly:in-event-class? x 'skip-event))
                             (events vs))))
    result))

(define-method (any-mmrest-events (vs <Voice-state>))
  (define (f? x)
    (ly:in-event-class? x 'multi-measure-rest-event))
  (any f? (events vs)))

(define-method (previous-voice-state (vs <Voice-state>))
  (let ((i (slot-ref vs 'vector-index))
        (v (slot-ref vs 'state-vector)))
    (if (< 0 i)
        (vector-ref v (1- i))
        #f)))

;; true if the part has ended
(define-method (done? (vs <Voice-state>))
  (let ((i (slot-ref vs 'vector-index))
        (v (slot-ref vs 'state-vector)))
    ;; the last entry represents the end of the part
    (= (1+ i) (vector-length v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class <Split-state> ()
  ;; The automatically determined split configuration
  (configuration #:init-value '() #:accessor configuration)
  ;; Allow overriding split configuration, takes precedence over configuration
  (forced-configuration #:init-value #f #:accessor forced-configuration)
  (when-moment #:accessor moment #:init-keyword #:moment)
  ;; voice-states are states starting with the Split-state or later
  ;;
  (is #:init-keyword #:voice-states #:accessor voice-states)
  (synced  #:init-keyword #:synced #:init-value  #f #:getter synced?))


(define-method (write (x <Split-state> ) f)
  (display (moment x) f)
  (display " = " f)
  (display (configuration x) f)
  (if (synced? x)
      (display " synced "))
  (display "\n" f))

(define-method (current-or-previous-voice-states (ss <Split-state>))
  "Return voice states meeting the following conditions.  For a voice
in sync, return the current voice state.  For a voice out of sync,
return the previous voice state."
  (let* ((vss (voice-states ss))
         (vs1 (car vss))
         (vs2 (cdr vss)))
    (if (and vs1 (not (equal? (moment vs1) (moment ss))))
        (set! vs1 (previous-voice-state vs1)))
    (if (and vs2 (not (equal? (moment vs2) (moment ss))))
        (set! vs2 (previous-voice-state vs2)))
    (cons vs1 vs2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (previous-span-state vs)
  (let ((p (previous-voice-state vs)))
    (if p (span-state p) '())))

(define (make-voice-states evl)
  (let* ((states (map (lambda (v)
                        (make <Voice-state>
                          #:moment (caar v)
                          #:tuning (cdar v)
                          #:events (map car (cdr v))))
                      (reverse evl))))

    ;; TODO: Add an entry at +inf.0 and see if it allows us to remove
    ;; the many instances of conditional code handling the case that
    ;; there is no voice state at a given moment.

    (let ((vec (list->vector (reverse! states))))
      (do ((i 0 (1+ i)))
          ((= i (vector-length vec)) vec)
        (slot-set! (vector-ref vec i) 'vector-index i)
        (slot-set! (vector-ref vec i) 'state-vector vec)))))

(define (make-split-state vs1 vs2)
  "Merge lists VS1 and VS2, containing Voice-state objects into vector
of Split-state objects, crosslinking the Split-state vector and
Voice-state objects
"
  (define (helper ss-idx ss-list idx1 idx2)
    (let* ((state1 (if (< idx1 (vector-length vs1)) (vector-ref vs1 idx1) #f))
           (state2 (if (< idx2 (vector-length vs2)) (vector-ref vs2 idx2) #f))
           (min (cond ((and state1 state2) (moment-min (moment state1) (moment state2)))
                      (state1 (moment state1))
                      (state2 (moment state2))
                      (else #f)))
           (inc1 (if (and state1 (equal? min (moment state1))) 1 0))
           (inc2 (if (and state2 (equal? min (moment state2))) 1 0))
           (ss-object (if min
                          (make <Split-state>
                            #:moment min
                            #:voice-states (cons state1 state2)
                            #:synced (= inc1 inc2))
                          #f)))
      (if state1
          (set! (split-index state1) ss-idx))
      (if state2
          (set! (split-index state2) ss-idx))
      (if min
          (helper (1+ ss-idx)
                  (cons ss-object ss-list)
                  (+ idx1 inc1)
                  (+ idx2 inc2))
          ss-list)))
  (list->vector (reverse! (helper 0 '() 0  0) '())))

(define (analyse-spanner-states voice-state-vec)

  (define (helper index active)
    "Analyse EVS at INDEX, given state ACTIVE."

    (define (analyse-tie-start active ev)
      (if (ly:in-event-class? ev 'tie-event)
          (acons 'tie (split-index (vector-ref voice-state-vec index))
                 active)
          active))

    (define (analyse-tie-end active ev)
      (if (ly:in-event-class? ev 'note-event)
          (assoc-remove! active 'tie)
          active))

    (define (analyse-absdyn-end active ev)
      (if (or (ly:in-event-class? ev 'absolute-dynamic-event)
              (and (ly:in-event-class? ev 'span-dynamic-event)
                   (equal? STOP (ly:event-property ev 'span-direction))))
          (assoc-remove! (assoc-remove! active 'cresc) 'decr)
          active))

    (define (active<? a b)
      (cond ((symbol<? (car a) (car b)) #t)
            ((symbol<? (car b) (car a)) #f)
            (else (< (cdr a) (cdr b)))))

    (define (analyse-span-event active ev)
      (let* ((name (car (ly:event-property ev 'class)))
             (key (cond ((equal? name 'slur-event) 'slur)
                        ((equal? name 'phrasing-slur-event) 'tie)
                        ((equal? name 'beam-event) 'beam)
                        ((equal? name 'crescendo-event) 'cresc)
                        ((equal? name 'decrescendo-event) 'decr)
                        (else #f)))
             (sp (ly:event-property ev 'span-direction)))
        (if (and (symbol? key) (ly:dir? sp))
            (if (= sp STOP)
                (assoc-remove! active key)
                (acons key
                       (split-index (vector-ref voice-state-vec index))
                       active))
            active)))

    (define (analyse-events active evs)
      "Run all analyzers on ACTIVE and EVS"
      (define (run-analyzer analyzer active evs)
        (if (pair? evs)
            (run-analyzer analyzer (analyzer active (car evs)) (cdr evs))
            active))
      (define (run-analyzers analyzers active evs)
        (if (pair? analyzers)
            (run-analyzers (cdr analyzers)
                           (run-analyzer (car analyzers) active evs)
                           evs)
            active))
      (sort ;; todo: use fold or somesuch.
       (run-analyzers (list analyse-absdyn-end analyse-span-event
                            ;; note: tie-start/span comes after tie-end/absdyn.
                            analyse-tie-end analyse-tie-start)
                      active evs)
       active<?))

    ;; must copy, since we use assoc-remove!
    (if (< index (vector-length voice-state-vec))
        (begin
          (set! active (analyse-events active (events (vector-ref voice-state-vec index))))
          (set! (span-state (vector-ref voice-state-vec index))
                (list-copy active))
          (helper (1+ index) active))))

  (helper 0 '()))

(define recording-group-functions
  ;;Selected parts from @var{toplevel-music-functions} not requiring @code{parser}.
  (list
   (lambda (music) (expand-repeat-chords! '(rhythmic-event) music))
   expand-repeat-notes!))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-public (recording-group-emulate music odef)
  "Interpret @var{music} according to @var{odef}, but store all events
in a chronological list, similar to the @code{Recording_group_engraver} in
LilyPond version 2.8 and earlier."
  (let*
      ((context-list '())
       (now-mom (ly:make-moment 0 0))
       (global (ly:make-global-context odef))
       (mom-listener (lambda (tev) (set! now-mom (ly:event-property tev 'moment))))
       (new-context-listener
        (lambda (sev)
          (let*
              ((child (ly:event-property sev 'context))
               (this-moment-list (cons (ly:context-id child) '()))
               (dummy (set! context-list (cons this-moment-list context-list)))
               (acc '())
               (accumulate-event-listener
                (lambda (ev)
                  (set! acc (cons (cons ev #t) acc))))
               (remove-context-listener
                (lambda (ev)
                  "Add a final entry to record the end moment."
                  (let ((this-moment (cons (cons
                                            now-mom
                                            #f ; instrumentTransposition
                                            )
                                           '() ; events
                                           )))
                    (set-cdr! this-moment-list
                              (cons this-moment (cdr this-moment-list))))))
               (save-acc-listener
                (lambda (tev)
                  (if (pair? acc)
                      (let ((this-moment
                             (cons (cons now-mom
                                         (ly:context-property child 'instrumentTransposition))
                                   ;; The accumulate-event-listener above creates
                                   ;; the list of events in reverse order, so we
                                   ;; have to revert it to the original order again
                                   (reverse acc))))
                        (set-cdr! this-moment-list
                                  (cons this-moment (cdr this-moment-list)))
                        (set! acc '()))))))
            (ly:add-listener accumulate-event-listener
                             (ly:context-event-source child) 'StreamEvent)
            (ly:add-listener remove-context-listener
                             (ly:context-event-source child) 'RemoveContext)
            (ly:add-listener save-acc-listener
                             (ly:context-event-source global) 'OneTimeStep)))))
    (ly:add-listener new-context-listener
                     (ly:context-events-below global) 'AnnounceNewContext)
    (ly:add-listener mom-listener (ly:context-event-source global) 'Prepare)
    (ly:interpret-music-expression
     (make-non-relative-music
      (fold (lambda (x m) (x m)) music recording-group-functions))
     global)
    context-list))

(define-public (determine-split-list evl1 evl2 chord-range)
  "Event lists @var{evl1} and @var{evl2} should be ascending.
@var{chord-range} is a pair of numbers @code{(min . max)} defining the distance
in steps between notes that may be combined into a chord or unison."
  (let* ((pc-debug #f)
         (voice-state-vec1 (make-voice-states evl1))
         (voice-state-vec2 (make-voice-states evl2))
         (result (make-split-state voice-state-vec1 voice-state-vec2))
         (chord-min-diff (car chord-range))
         (chord-max-diff (cdr chord-range)))

    ;; Go through all moments recursively and check if the events of that
    ;; moment contain a part-combine-force-event override. If so, store its
    ;; value in the forced-configuration field, which will override. The
    ;; previous configuration is used to determine non-terminated settings.
    (define (analyse-forced-combine result-idx prev-res)

      (define (get-forced-event x)
        (cond
         ((and (ly:in-event-class? x 'SetProperty)
               (eq? (ly:event-property x 'symbol) 'partCombineForced))
          (cons (ly:event-property x 'value #f)
                (ly:event-property x 'once #f)))
         ((and (ly:in-event-class? x 'UnsetProperty)
               (eq? (ly:event-property x 'symbol) 'partCombineForced))
          (cons #f (ly:event-property x 'once #f)))
         (else #f)))

      (define (part-combine-events vs)
        (if (not vs)
            '()
            (filter-map get-forced-event (events vs))))
      ;; end part-combine-events

      ;; forced-result: Take the previous config and analyse whether
      ;; any change happened.... Return new once and permanent config
      (define (forced-result evt state)
        ;; sanity check, evt should always be (new-state . once)
        (if (not (and (pair? evt) (pair? state)))
            state
            (if (cdr evt)
                ;; Once-event, leave permanent state unchanged
                (cons (car evt) (cdr state))
                ;; permanent change, leave once state unchanged
                (cons (car state) (car evt)))))
      ;; end forced-combine-result

      ;; body of analyse-forced-combine:
      (if (< result-idx (vector-length result))
          (let* ((now-state (vector-ref result result-idx)) ; current result
                 ;; Extract all part-combine force events
                 (evts (if (synced? now-state)
                           (append
                            (part-combine-events (car (voice-states now-state)))
                            (part-combine-events (cdr (voice-states now-state))))
                           '()))
                 ;; result is (once-state permament-state):
                 (state (fold forced-result (cons 'automatic prev-res) evts))
                 ;; Now let once override permanent changes:
                 (force-state (if (equal? (car state) 'automatic)
                                  (cdr state)
                                  (car state))))
            (set! (forced-configuration (vector-ref result result-idx))
                  force-state)
            ;; For the next moment, ignore the once override (car stat)
            ;; and pass on the permanent override, stored as (cdr state)
            (analyse-forced-combine (1+ result-idx) (cdr state)))))
    ;; end analyse-forced-combine


    (define (analyse-time-step result-idx)
      (define (put x . index)
        "Put the result to X, starting from INDEX backwards.

Only set if not set previously.
"
        (let ((i (if (pair? index) (car index) result-idx)))
          (if (and (<= 0 i)
                   (not (symbol? (configuration (vector-ref result i)))))
              (begin
                (set! (configuration (vector-ref result i)) x)
                (put x (1- i))))))

      (define (copy-state-from state-vec vs)
        (define (copy-one-state key-idx)
          (let* ((idx (cdr key-idx))
                 (prev-ss (vector-ref result idx))
                 (prev (configuration prev-ss)))
            (if (symbol? prev)
                (put prev))))
        (for-each copy-one-state (span-state vs)))

      (define (analyse-notes now-state)
        (let* ((vs1 (car (voice-states now-state)))
               (vs2 (cdr (voice-states now-state)))
               (notes1 (comparable-note-events vs1))
               (notes2 (comparable-note-events vs2)))
          (cond
           ;; if neither part has notes, do nothing
           ((and (not (pair? notes1)) (not (pair? notes2))))

           ;; if one part has notes and the other does not
           ((or (not (pair? notes1)) (not (pair? notes2))) (put 'apart))

           ;; if either part has a chord
           ((or (> (length notes1) 1)
                (> (length notes2) 1))
            (if (and (<= chord-min-diff 0) ; user requests combined unisons
                     (equal? notes1 notes2)) ; both parts have the same chord
                (put 'chords)
                (put 'apart)))

           ;; if the durations are different
           ;; TODO articulations too?
           ((and (not (equal? (ly:event-property (car notes1) 'duration)
                              (ly:event-property (car notes2) 'duration))))
            (put 'apart))

           (else
            ;; Is the interval outside of chord-range?
            (if (let ((diff (ly:pitch-steps
                             (ly:pitch-diff
                              (ly:event-property (car notes1) 'pitch)
                              (ly:event-property (car notes2) 'pitch)))))
                  (or (< diff chord-min-diff)
                      (> diff chord-max-diff)
                      ))
                (put 'apart)
                ;; copy previous split state from spanner state
                (begin
                  (if (previous-voice-state vs1)
                      (copy-state-from voice-state-vec1
                                       (previous-voice-state vs1)))
                  (if (previous-voice-state vs2)
                      (copy-state-from voice-state-vec2
                                       (previous-voice-state vs2)))
                  (if (and (null? (span-state vs1)) (null? (span-state vs2)))
                      (put 'chords))))))))

      (if (< result-idx (vector-length result))
          (let* ((now-state (vector-ref result result-idx))
                 (vs1 (car (voice-states now-state)))
                 (vs2 (cdr (voice-states now-state))))

            (cond ((not vs1) (put 'apart))
                  ((not vs2) (put 'apart))
                  (else
                   (let ((active1 (previous-span-state vs1))
                         (active2 (previous-span-state vs2))
                         (new-active1 (span-state vs1))
                         (new-active2 (span-state vs2)))
                     (if #f ; debug
                         (display (list (moment now-state) result-idx
                                        active1 "->" new-active1
                                        active2 "->" new-active2
                                        "\n")))
                     (if (and (synced? now-state)
                              (equal? active1 active2)
                              (equal? new-active1 new-active2))
                         (analyse-notes now-state)

                         ;; active states different:
                         (put 'apart)))

                   ;; go to the next one, if it exists.
                   (analyse-time-step (1+ result-idx)))))))

    (define (analyse-a2 result-idx)
      (if (< result-idx (vector-length result))
          (let* ((now-state (vector-ref result result-idx))
                 (vs1 (car (voice-states now-state)))
                 (vs2 (cdr (voice-states now-state))))

            (define (analyse-synced-silence)
              (let ((rests1 (if vs1 (silence-events vs1) '()))
                    (rests2 (if vs2 (silence-events vs2) '())))
                (cond

                 ;; equal rests or equal skips, but not one of each
                 ((and (= 1 (length rests1))
                       (= 1 (length rests2))
                       (equal? (ly:event-property (car rests1) 'class)
                               (ly:event-property (car rests2) 'class))
                       (equal? (ly:event-property (car rests1) 'duration)
                               (ly:event-property (car rests2) 'duration)))
                  (set! (configuration now-state) 'unisilence))

                 ;; rests of different durations or mixed with
                 ;; skips or multi-measure rests
                 (else
                  ;; TODO For skips, route the rest to the shared
                  ;; voice and the skip to the voice for its part?
                  (set! (configuration now-state) 'apart-silence))

                 )))

            (define (analyse-unsynced-silence vs1 vs2)
              (let ((any-mmrests1 (if vs1 (any-mmrest-events vs1) #f))
                    (any-mmrests2 (if vs2 (any-mmrest-events vs2) #f)))
                (cond
                 ;; If a multi-measure rest begins now while the other
                 ;; part has an ongoing multi-measure rest (or has
                 ;; ended), start displaying the one that begins now.
                 ((and any-mmrests1
                       (equal? (moment vs1) (moment now-state))
                       (or (not vs2) any-mmrests2))
                  (set! (configuration now-state) 'silence1))

                 ;; as above with parts swapped
                 ((and any-mmrests2
                       (equal? (moment vs2) (moment now-state))
                       (or (not vs1) any-mmrests1))
                  (set! (configuration now-state) 'silence2))
                 )))

            (if (or vs1 vs2)
                (let ((notes1 (if vs1 (comparable-note-events vs1) '()))
                      (notes2 (if vs2 (comparable-note-events vs2) '())))
                  (cond ((and (equal? (configuration now-state) 'chords)
                              (pair? notes1)
                              (equal? notes1 notes2))
                         (set! (configuration now-state) 'unisono))

                        ((synced? now-state)
                         (if (and (= 0 (length notes1))
                                  (= 0 (length notes2)))
                             (analyse-synced-silence)))

                        (else ;; not synchronized
                         (let* ((vss
                                 (current-or-previous-voice-states now-state))
                                (vs1 (car vss))
                                (vs2 (cdr vss)))
                           (if (and
                                (or (not vs1) (= 0 (length (note-events vs1))))
                                (or (not vs2) (= 0 (length (note-events vs2)))))
                               (analyse-unsynced-silence vs1 vs2))))
                        )))
            (analyse-a2 (1+ result-idx)))))

    (define (analyse-solo12 result-idx)

      (define (previous-config vs)
        (let* ((pvs (previous-voice-state vs))
               (spi (if pvs (split-index pvs) #f))
               (prev-split (if spi (vector-ref result spi) #f)))
          (if prev-split
              (configuration prev-split)
              'apart)))

      (define (put-range x a b)
        ;; (display (list "put range "  x a b "\n"))
        (do ((i a (1+ i)))
            ((> i b) b)
          (set! (configuration (vector-ref result i)) x)))

      (define (put x)
        ;; (display (list "putting "  x "\n"))
        (set! (configuration (vector-ref result result-idx)) x))

      (define (current-voice-state now-state voice-num)
        (define vs ((if (= 1 voice-num) car cdr)
                    (voice-states now-state)))
        (if (or (not vs) (equal? (moment now-state) (moment vs)))
            vs
            (previous-voice-state vs)))

      (define (try-solo type start-idx current-idx)
        "Find a maximum stretch that can be marked as solo.  Only set
the mark when there are no spanners active.

      return next idx to analyse.
"
        (if (< current-idx (vector-length result))
            (let* ((now-state (vector-ref result current-idx))
                   (solo-state (current-voice-state now-state (if (equal? type 'solo1) 1 2)))
                   (silent-state (current-voice-state now-state (if (equal? type 'solo1) 2 1)))
                   (silent-notes (if silent-state (note-events silent-state) '()))
                   (solo-notes (if solo-state (note-events solo-state) '())))
              ;; (display (list "trying " type " at "  (moment now-state) solo-state silent-state        "\n"))
              (cond ((not (equal? (configuration now-state) 'apart))
                     current-idx)
                    ((> (length silent-notes) 0) start-idx)
                    ((not solo-state)
                     (put-range type start-idx current-idx)
                     current-idx)
                    ((and
                      (null? (span-state solo-state)))

                     ;;
                     ;; This includes rests. This isn't a problem: long rests
                     ;; will be shared with the silent voice, and be marked
                     ;; as unisilence. Therefore, long rests won't
                     ;;  accidentally be part of a solo.
                     ;;
                     (put-range type start-idx current-idx)
                     (try-solo type (1+ current-idx) (1+  current-idx)))
                    (else
                     (try-solo type start-idx (1+ current-idx)))))
            ;; try-solo
            start-idx))

      (define (analyse-apart-silence result-idx)
        "Analyse 'apart-silence starting at RESULT-IDX.  Return next index."

        (define (analyse-synced-apart-silence vs1 vs2)
          (let* ((rests1 (silence-events vs1))
                 (rests2 (silence-events vs2)))
            (cond
             ;; multiple rests in the same part
             ((and (or (not (= 1 (length rests1)))
                       (not (= 1 (length rests2)))))
              (put 'apart-silence))

             ;; rest with multi-measure rest: choose the rest
             ((and (ly:in-event-class? (car rests1) 'rest-event)
                   (ly:in-event-class? (car rests2) 'multi-measure-rest-event))
              (put 'silence1))

             ;; as above with parts swapped
             ((and (ly:in-event-class? (car rests1) 'multi-measure-rest-event)
                   (ly:in-event-class? (car rests2) 'rest-event))
              (put 'silence2))

             ;; mmrest in both parts: choose the shorter one
             ;; (equal mmrests are classified as unisilence earlier,
             ;; so they shouldn't be seen here)
             ((and (ly:in-event-class? (car rests1) 'multi-measure-rest-event)
                   (ly:in-event-class? (car rests2) 'multi-measure-rest-event))
              (if (ly:duration<? (ly:event-property (car rests1) 'duration)
                                 (ly:event-property (car rests2) 'duration))
                  (put 'silence1)
                  (put 'silence2)))

             (else
              (put 'apart-silence)))))

        (define (analyse-unsynced-apart-silence vs1 vs2)
          (let* ((prev-state (if (> result-idx 0)
                                 (vector-ref result (- result-idx 1))
                                 #f))
                 (prev-config (if prev-state
                                  (configuration prev-state)
                                  'apart-silence)))
            (cond
             ;; remain in the silence1/2 states until resync
             ((equal? prev-config 'silence1)
              (put 'silence1))

             ((equal? prev-config 'silence2)
              (put 'silence2))

             (else
              (put 'apart-silence)))))

        (let* ((now-state (vector-ref result result-idx))
               (vs1 (current-voice-state now-state 1))
               (vs2 (current-voice-state now-state 2)))
          (cond
           ;; part 1 has ended
           ((or (not vs1) (done? vs1))
            (put 'silence2))

           ;; part 2 has ended
           ((or (not vs2) (done? vs2))
            (put 'silence1))

           ((synced? now-state)
            (analyse-synced-apart-silence vs1 vs2))

           (else
            (analyse-unsynced-apart-silence vs1 vs2)))

          (1+ result-idx)))

      (define (analyse-apart result-idx)
        "Analyse 'apart starting at RESULT-IDX.  Return next index."
        (let* ((now-state (vector-ref result result-idx))
               (vs1 (current-voice-state now-state 1))
               (vs2 (current-voice-state now-state 2))
               ;; (vs1 (car (voice-states now-state)))
               ;; (vs2 (cdr (voice-states now-state)))
               (notes1 (if vs1 (note-events vs1) '()))
               (notes2 (if vs2 (note-events vs2) '()))
               (n1 (length notes1))
               (n2 (length notes2)))
          ;; (display (list "analyzing step " result-idx "  moment " (moment now-state) vs1 vs2  "\n"))
          (max
           ;; we should always increase.
           (cond ((and (= n1 0) (= n2 0))
                  ;; If we hit this, it means that the previous passes
                  ;; have designated as 'apart what is really
                  ;; 'apart-silence.
                  (analyse-apart-silence result-idx))
                 ((and (= n2 0)
                       (equal? (moment vs1) (moment now-state))
                       (null? (previous-span-state vs1)))
                  (try-solo 'solo1 result-idx result-idx))
                 ((and (= n1 0)
                       (equal? (moment vs2) (moment now-state))
                       (null? (previous-span-state vs2)))
                  (try-solo 'solo2 result-idx result-idx))

                 (else (1+ result-idx)))
           ;; analyse-moment
           (1+ result-idx))))

      (if (< result-idx (vector-length result))
          (let ((conf (configuration (vector-ref result result-idx))))
            (cond
             ((equal? conf 'apart)
              (analyse-solo12 (analyse-apart result-idx)))
             ((equal? conf 'apart-silence)
              (analyse-solo12 (analyse-apart-silence result-idx)))
             (else
              (analyse-solo12 (1+ result-idx))))))) ; analyse-solo12

    (analyse-spanner-states voice-state-vec1)
    (analyse-spanner-states voice-state-vec2)
    (if #f
        (begin
          (display voice-state-vec1)
          (display "***\n")
          (display voice-state-vec2)
          (display "***\n")
          (display result)
          (display "***\n")))

    ;; Extract all forced combine strategies, i.e. events inserted by
    ;; \partCombine(Apart|Automatic|SoloI|SoloII|Chords)[Once]
    ;; They will in the end override the automaically determined ones.
    ;; Initial state for both voices is no override
    (analyse-forced-combine 0 #f)
    ;; Now go through all time steps in a loop and find a combination strategy
    ;; based only on the events of that one moment (i.e. neglecting longer
    ;; periods of solo/apart, etc.)
    (analyse-time-step 0)
    ;; (display result)
    ;; Check for unisono or unisilence moments
    (analyse-a2 0)
    ;;(display result)
    (analyse-solo12 0)
    ;; (display result)
    (set! result (map
                  ;; forced-configuration overrides, if it is set
                  (lambda (x) (cons (moment x) (or (forced-configuration x) (configuration x))))
                  (vector->list result)))
    (if #f ;; pc-debug
        (display result))
    result))

(define-public default-part-combine-mark-state-machine
  ;; (current-state . ((split-state-event .
  ;;                      (output-voice output-event next-state)) ...))
  '((Initial . ((solo1   . (solo   SoloOneEvent Solo1))
                (solo2   . (solo   SoloTwoEvent Solo2))
                (unisono . (shared UnisonoEvent Unisono))))
    (Solo1   . ((apart   . (#f     #f           Initial))
                (chords  . (#f     #f           Initial))
                (solo2   . (solo   SoloTwoEvent Solo2))
                (unisono . (shared UnisonoEvent Unisono))))
    (Solo2   . ((apart   . (#f     #f           Initial))
                (chords  . (#f     #f           Initial))
                (solo1   . (solo   SoloOneEvent Solo1))
                (unisono . (shared UnisonoEvent Unisono))))
    (Unisono . ((apart   . (#f     #f           Initial))
                (chords  . (#f     #f           Initial))
                (solo1   . (solo   SoloOneEvent Solo1))
                (solo2   . (solo   SoloTwoEvent Solo2))))))

(define-public (make-part-combine-marks state-machine split-list)
  "Generate a sequence of part combiner events from a split list."

  (define (get-state state-name)
    (assq-ref state-machine state-name))

  (let ((full-seq '()) ; sequence of { \context Voice = "x" {} ... }
        (segment '()) ; sequence within \context Voice = "x" {...}
        (prev-moment ZERO-MOMENT)
        (prev-voice #f)
        (state (get-state 'Initial)))

    (define (commit-segment)
      "Add the current segment to the full sequence and begin another."
      (if (pair? segment)
          (set! full-seq
                (cons (make-music 'ContextSpeccedMusic
                                  'context-id (symbol->string prev-voice)
                                  'context-type 'Voice
                                  'element (make-sequential-music (reverse! segment)))
                      full-seq)))
      (set! segment '()))

    (define (handle-split split)
      (let* ((moment (car split))
             (action (assq-ref state (cdr split))))
        (if action
            (let ((voice (car action))
                  (part-combine-event (cadr action))
                  (next-state-name (caddr action)))
              (if part-combine-event
                  (let ((dur (ly:moment-sub moment prev-moment)))
                    ;; start a new segment when the voice changes
                    (if (not (eq? voice prev-voice))
                        (begin
                          (commit-segment)
                          (set! prev-voice voice)))
                    (if (not (equal? dur ZERO-MOMENT))
                        (set! segment (cons (make-music 'SkipEvent
                                                        'duration (make-duration-of-length dur)) segment)))
                    (set! segment (cons (make-music part-combine-event) segment))

                    (set! prev-moment moment)))
              (set! state (get-state next-state-name))))))

    (for-each handle-split split-list)
    (commit-segment)
    (make-sequential-music (reverse! full-seq))))

(define-public default-part-combine-context-change-state-machine-one
  ;; (current-state . ((split-state-event . (output-voice next-state)) ...))
  '((Initial . ((apart         . (one    . Initial))
                (apart-silence . (one    . Initial))
                (apart-spanner . (one    . Initial))
                (chords        . (shared . Initial))
                (silence1      . (shared . Initial))
                (silence2      . (null   . Demoted))
                (solo1         . (solo   . Initial))
                (solo2         . (null   . Demoted))
                (unisono       . (shared . Initial))
                (unisilence    . (shared . Initial))))

    ;; After a part has been used as the exclusive input for a
    ;; passage, we want to use it by default for unisono/unisilence
    ;; passages because Part_combine_iterator might have killed
    ;; multi-measure rests in the other part.  Here we call such a
    ;; part "promoted".  Part one begins promoted.
    (Demoted . ((apart         . (one    . Demoted))
                (apart-silence . (one    . Demoted))
                (apart-spanner . (one    . Demoted))
                (chords        . (shared . Demoted))
                (silence1      . (shared . Initial))
                (silence2      . (null   . Demoted))
                (solo1         . (solo   . Initial))
                (solo2         . (null   . Demoted))
                (unisono       . (null   . Demoted))
                (unisilence    . (null   . Demoted))))))

(define-public default-part-combine-context-change-state-machine-two
  ;; (current-state . ((split-state-event . (output-voice next-state)) ...))
  '((Initial . ((apart         . (two    . Initial))
                (apart-silence . (two    . Initial))
                (apart-spanner . (two    . Initial))
                (chords        . (shared . Initial))
                (silence1      . (null   . Initial))
                (silence2      . (shared . Promoted))
                (solo1         . (null   . Initial))
                (solo2         . (solo   . Promoted))
                (unisono       . (null   . Initial))
                (unisilence    . (null   . Initial))))

    ;; See the part-one state machine for the meaning of "promoted".
    (Promoted . ((apart         . (two    . Promoted))
                 (apart-silence . (two    . Promoted))
                 (apart-spanner . (two    . Promoted))
                 (chords        . (shared . Promoted))
                 (silence1      . (null   . Initial))
                 (silence2      . (shared . Promoted))
                 (solo1         . (null   . Initial))
                 (solo2         . (solo   . Promoted))
                 (unisono       . (shared . Promoted))
                 (unisilence    . (shared . Promoted))))))

(define-public (make-part-combine-context-changes state-machine split-list)
  "Generate a sequence of part combiner context changes from a split list."

  (define (get-state state-name)
    (assq-ref state-machine state-name))

  (let* ((change-list '())
         (prev-moment
          ;; the start moment is in the first entry of the split list
          (if (and (pair? split-list) (pair? (car split-list)))
              (caar split-list)
              ZERO-MOMENT))
         (prev-change-moment prev-moment)
         (prev-voice #f)
         (state (get-state 'Initial)))

    (define (handle-split split)
      (let* ((moment (car split))
             (action (assq-ref state (cdr split))))
        (if action
            (let ((voice (car action))
                  (next-state-name (cdr action)))
              (if (not (eq? voice prev-voice))
                  (begin
                    (set! change-list
                          (cons (skip-of-moment-span prev-change-moment moment)
                                change-list))
                    (set! change-list
                          (cons (make-music
                                 'ContextChange
                                 'change-tag '$partCombine
                                 'change-to-type 'Voice
                                 'change-to-id (symbol->string voice))
                                change-list))
                    (set! prev-change-moment moment)
                    (set! prev-voice voice)))
              (set! prev-moment moment)
              (set! state (get-state next-state-name))))))

    ;; (display split-list)
    (for-each handle-split split-list)
    ;; add a final skip so that the length of the music is correct
    (set! change-list (cons (skip-of-moment-span prev-change-moment prev-moment)
                            change-list))
    (let ((result (make-sequential-music (reverse! change-list))))
      ;; (display-lily-music result)
      result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (add-quotable name mus)
  (let* ((tab (eval 'musicQuotes (current-module)))
         (voicename (get-next-unique-voice-name))
         ;; recording-group-emulate returns an assoc list (reversed!), so
         ;; hand it a proper unique context name and extract that key:
         (ctx-spec (context-spec-music mus 'Voice voicename))
         (listener (ly:parser-lookup 'partCombineListener))
         (context-list (reverse (recording-group-emulate ctx-spec listener)))
         (raw-voice (assoc voicename context-list))
         (quote-contents (and raw-voice (reverse! (cdr raw-voice)))))

    (define (has-events? e)
      (and (pair? e)
           (pair? (car e))
           (pair? (cdar e))))

    ;; If the context-specced quoted music does not contain anything, try to
    ;; use the first child, i.e. the next in context-list after voicename
    ;; That's the case e.g. for \addQuote "x" \relative c \new Voice {...}
    ;;
    ;; Note that if raw-voice is #f, so is quote-contents, in which
    ;; case the following loop is skipped.
    (if (not (has-events? quote-contents))
        (let find-non-empty ((current-tail (member raw-voice context-list)))
          ;; if voice has contents, use them, otherwise check next ctx
          (if (null? current-tail)
              #f
              (let ((candidate (and (pair? (car current-tail))
                                    (pair? (cdar current-tail))
                                    (reverse! (cdar current-tail)))))
                (if (has-events? candidate)
                    (set! quote-contents candidate)
                    (find-non-empty (cdr current-tail)))))))

    (if (has-events? quote-contents)
        (hash-set! tab name (list->vector quote-contents))
        (ly:music-warning mus (G_ "quoted music `~a' is empty") name))))
