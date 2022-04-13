;;;; song.scm --- Festival singing mode output
;;;;
;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2006--2022 Brailcom, o.p.s.
;;;; Author: Milan Zamazal <pdm@brailcom.org>
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


(define-module (lily song)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-39)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 receive)
  #:use-module (lily)
  #:use-module (lily song-util))


;;; Configuration


;; The word to be sung in places where notes are played without lyrics.
;; If it is #f, the places without lyrics are omitted on the output.
(define-public *skip-word* (make-parameter "-skip-"))

;; If true, use syllables in the Festival XML file.
;; If false, use whole words instead; this is necessary in languages like
;; English, were the phonetic form cannot be deduced from syllables well enough.
(define-public *syllabify* (make-parameter #f))

;; Base Festival octave to which LilyPond notes are mapped.
(define-public *base-octave* (make-parameter 5))
;; The resulting base octave is sum of *base-octave* and
;; *base-octave-shift*.  This is done to work around a Festival bug
;; causing Festival to segfault or produce invalid pitch on higher pitches.
;; (define *base-octave-shift* -2)
(define *base-octave-shift* (make-parameter 0))

;; The coefficient by which the notes just before \breathe are shortened.
(define-public *breathe-shortage* (make-parameter 0.8))


;;; LilyPond interface


(define-public (output-file music tempo filename)
  (if *debug*
      (debug-enable 'backtrace))
  (ly:message "Writing Festival XML file ~a..." filename)
  (let ((port (open-output-file filename)))
    (set-port-encoding! port "UTF-8")
    (write-header port tempo)
    (write-lyrics port music)
    (write-footer port)
    (close-port port))
  #f)


;;; Utility functions


(define pp-pitch-names '((0 . "c") (1 . "des") (2 . "d") (3 . "es") (4 . "e") (5 . "f")
                         (6 . "ges") (7 . "g") (8 . "as") (9 . "a") (10 . "bes") (11 . "b")))
(define (pp object)
  (cond
   ((list? object)
    (format #f "[~{~a ~}]" (map pp object)))
   ((skip? object)
    (format #f "skip(~a)" (skip-duration object)))
   ((lyrics? object)
    (format #f "~a(~a)~a" (lyrics-text object) (lyrics-duration object)
            (if (lyrics-unfinished object) "-" "")))
   ((note? object)
    (let ((pitch (ly:pitch-semitones (note-pitch object))))
      (format #f "~a~a~a~a"
              (assoc-get (modulo pitch 12) pp-pitch-names)
              (let ((octave (+ (inexact->exact (floor (/ pitch 12))) 1)))
                (cond
                 ((= octave 0)
                  "")
                 ((> octave 0)
                  (make-string octave #\'))
                 ((< octave 0)
                  (make-string (- octave) #\,))))
              (pp-duration (note-duration object))
              (if (> (note-joined object) 0) "-" ""))))
   ((rest? object)
    (format #f "r~a" (pp-duration (rest-duration object))))
   (else
    object)))

(define (pp-duration duration)
  (set! duration (/ 4 duration))
  (if (< (abs (- duration (inexact->exact duration))) 0.0001)
      (inexact->exact duration)
      (/ (round (* duration 100)) 100)))

(define-public (warning object-with-origin message . args)
  (let ((origin (cond
                 ((not object-with-origin)
                  #f)
                 ((note? object-with-origin)
                  (note-origin object-with-origin))
                 ((rest? object-with-origin)
                  (rest-origin object-with-origin))
                 ((ly:input-location? object-with-origin)
                  object-with-origin)
                 ((ly:music? object-with-origin)
                  (ly:music-property object-with-origin 'origin))
                 (else
                  (format #t "Minor programming error: ~a~%" object-with-origin)
                  #f))))
    (if origin
        (ly:input-message origin "***Song Warning***")
        (format #t "~%***Song Warning***"))
    (apply ly:message message (map pp args))))


;;; Analysis functions


(define *default-tempo* #f)
(define *tempo-compression* #f)

(define (duration->number duration)
  (exact->inexact (ly:moment-main (ly:duration-length duration))))

(define (tempo->beats music)
  (let* ((tempo-spec (find-child-named music 'SequentialMusic))
         (tempo (cond
                 (tempo-spec
                  (let ((tempo-event (find-child-named tempo-spec
                                                       'TempoChangeEvent)))
                    (and tempo-event
                         (let ((count (ly:music-property tempo-event
                                                         'metronome-count)))
                           (* (if (pair? count)
                                  (round (/ (+ (car count) (cdr count)) 2))
                                  count)
                              (duration->number
                               (ly:music-property tempo-event 'tempo-unit)))))))
                 (else
                  (format #t "Programming error (tempo->beats): ~a~%"
                          tempo-spec)))))
    (debug-enable 'backtrace)
    (and tempo
         (set! *default-tempo* (property-value
                                (find-child tempo-spec (lambda (elt)
                                                         (music-property? elt 'tempoWholesPerMinute)))))
         (round (* tempo (expt 2 (+ 2 (*base-octave-shift*))))))))

(defstruct music-context
  music
  context)

(define (collect-lyrics-music music)
  ;; Returns list of music-context instances.
  (let ((music-context-list '()))
    (process-music
     music
     (lambda (music*)
       (cond
        ((music-name? music* 'LyricCombineMusic)
         (push! (make-music-context #:music music*
                                    #:context (ly:music-property music* 'associated-context))
                music-context-list)
         #t)
        ((and (music-name? music* 'ContextSpeccedMusic)
              (music-property-value? music* 'context-type 'Lyrics)
              (not (find-child-named music* 'LyricCombineMusic)))
         (let ((name-node (find-child music* (lambda (node) (music-property? node 'associatedVoice)))))
           (if name-node
               (push! (make-music-context #:music music* #:context (property-value name-node))
                      music-context-list)))
         #t)
        (else
         #f))))
    (debug "Lyrics contexts" (reverse music-context-list))))

(defstruct lyrics
  text
  duration
  unfinished
  ignore-melismata
  context)

(defstruct skip
  duration
  context)

(define (get-lyrics music context)
  ;; Returns list of lyrics and skip instances.
  (let ((lyrics-list '())
        (next-ignore-melismata #f)
        (ignore-melismata #f)
        (next-current-voice context)
        (current-voice context))
    (process-music
     music
     (lambda (music)
       (cond
        ;; true lyrics
        ((music-name? music '(EventChord LyricEvent))
         (let ((lyric-event (find-child-named music 'LyricEvent)))
           (push! (make-lyrics
                   #:text (ly:music-property lyric-event 'text)
                   #:duration (* (duration->number (ly:music-property lyric-event 'duration)) 4)
                   #:unfinished (and (not (*syllabify*)) (find-child-named music 'HyphenEvent))
                   #:ignore-melismata ignore-melismata
                   #:context current-voice)
                  lyrics-list))
         ;; LilyPond delays applying settings
         (set! ignore-melismata next-ignore-melismata)
         (set! current-voice next-current-voice)
         #t)
        ;; skipping
        ((music-name? music 'SkipMusic)
         (push! (make-skip
                 #:duration (* (duration->number (ly:music-property music 'duration)) 4)
                 #:context current-voice)
                lyrics-list)
         #t)
        ;; parameter change
        ((music-property? music 'ignoreMelismata)
         (set! next-ignore-melismata (property-value music))
         #t)
        ((music-property? music 'associatedVoice)
         (set! next-current-voice (property-value music))
         #t)
        ;; anything else
        (else
         #f))))
    (debug "Raw lyrics" (reverse lyrics-list))))

(defstruct score-voice
  context
  elements ; list of score-* instances
  )

(defstruct score-choice
  lists ; of lists of score-* instances
  (n-assigned 0) ; number of lists having a verse-block
  )

(defstruct score-repetice
  count ; number of repetitions
  elements ; list of score-* instances
  )

(defstruct score-notes
  note/rest-list                        ; list of note and rest instances
  (verse-block-list '())                ; lyrics attached to notes -- multiple
                                        ; elements are possible for
                                        ; multiple stanzas
  )

(defstruct note
  pitch
  duration
  joined ; to the next note
  origin
  )

(defstruct rest
  duration
  origin
  )

(define (get-notes music)
  ;; Returns list of score-* instances.
  (get-notes* music #t))

(define (get-notes* music autobeaming*)
  ;; Returns list of score-* instances.
  (let* ((result-list '())
         (in-slur 0)
         (autobeaming autobeaming*)
         (last-note-spec #f))
    (process-music
     music
     (lambda (music)
       (cond
        ;; context change
        ((music-has-property? music 'context-id)
         (let ((context (ly:music-property music 'context-id))
               (children (music-elements music)))
           (add! (make-score-voice #:context (debug "Changing context" context)
                                   #:elements (append-map (lambda (elt)
                                                            (get-notes* elt autobeaming))
                                                          children))
                 result-list))
         #t)
        ;; timing change
        ((music-property? music 'timeSignatureFraction)
         (let ((value (property-value music)))
           (debug "Timing change" value)))
        ;; simultaneous notes
        ((music-name? music 'SimultaneousMusic)
         (let ((simultaneous-lists (map (lambda (child)
                                          (get-notes* child autobeaming))
                                        (ly:music-property music 'elements))))
           (debug "Simultaneous lists" simultaneous-lists)
           (add! (make-score-choice #:lists simultaneous-lists) result-list))
         #t)
        ;; repetice
        ((music-name? music 'VoltaRepeatedMusic)
         (let ((repeat-count (ly:music-property music 'repeat-count))
               (children (music-elements music)))
           (add! (make-score-repetice #:count repeat-count
                                      #:elements (append-map
                                                  (lambda (elt) (get-notes* elt autobeaming))
                                                  children))
                 result-list))
         #t)
        ;; a note or rest
        ((or (music-name? music 'EventChord)
             (music-name? music 'MultiMeasureRestMusic)) ; 2.10
         (debug "Simple music event" music)
         (if *tempo-compression*
             (set! music (ly:music-compress (ly:music-deep-copy music) *tempo-compression*)))
         (let ((note (find-child-named music 'NoteEvent))
               (rest (if (music-name? music 'MultiMeasureRestMusic) ; 2.10
                         music
                         (or (find-child-named music 'RestEvent)
                             (find-child-named music 'MultiMeasureRestEvent) ; 2.8
                             ))))
           (cond
            (note
             (debug "Note" note)
             (let* ((pitch (ly:music-property note 'pitch))
                    (duration (* (duration->number (ly:music-property note 'duration)) 4))
                    (events (filter identity (list
                                              (find-child-named music 'SlurEvent)
                                              (find-child-named music 'ManualMelismaEvent)
                                              (and (not autobeaming)
                                                   (find-child-named music 'BeamEvent)))))
                    (slur-start (length (filter (lambda (e) (music-property-value? e 'span-direction -1))
                                                events)))
                    (slur-end (length (filter (lambda (e) (music-property-value? e 'span-direction 1))
                                              events))))
               (set! in-slur (+ in-slur slur-start (- slur-end)))
               (let ((note-spec (make-note #:pitch pitch #:duration duration #:joined in-slur
                                           #:origin (ly:music-property note 'origin)))
                     (last-result (and (not (null? result-list)) (last result-list))))
                 (set! last-note-spec note-spec)
                 (if (and last-result
                          (score-notes? last-result))
                     (set-score-notes-note/rest-list!
                      last-result
                      (append (score-notes-note/rest-list last-result) (list note-spec)))
                     (add! (make-score-notes #:note/rest-list (list note-spec)) result-list)))))
            (rest
             (debug "Rest" rest)
             (let* ((duration (* (duration->number (ly:music-property rest 'duration)) 4))
                    (rest-spec (make-rest #:duration duration
                                          #:origin (ly:music-property rest 'origin)))
                    (last-result (and (not (null? result-list)) (last result-list))))
               (if (and last-result
                        (score-notes? last-result))
                   (set-score-notes-note/rest-list! last-result
                                                    (append (score-notes-note/rest-list last-result)
                                                            (list rest-spec)))
                   (add! (make-score-notes #:note/rest-list (list rest-spec)) result-list))))))
         (filter
          (lambda (m)
            (not (music-name? m '(RestEvent
                                  NoteEvent
                                  LyricEvent
                                  MultiMeasureRestEvent))))
          (ly:music-property music 'elements)))
        ((music-name? music '(RestEvent
                              NoteEvent
                              LyricEvent
                              MultiMeasureRestEvent))
         (make-music 'EventChord
                     'elements
                     (cons music
                           (ly:music-property music 'articulations))))
        ;; autobeaming change
        ((music-property? music 'autoBeaming)
         (set! autobeaming (property-value music))
         #t)
        ;; melisma change
        ((music-property? music 'melismaBusy) ; 2.10
         (let ((change (if (property-value music) 1 -1)))
           (set! in-slur (+ in-slur change))
           (if last-note-spec
               (set-note-joined! last-note-spec (+ (note-joined last-note-spec) change))))
         #t)
        ;; tempo change
        ((music-property? music 'tempoWholesPerMinute)
         (set! *tempo-compression* (ly:moment-div *default-tempo* (property-value music)))
         #t)
        ;; breathe
        ((music-name? music 'BreathingEvent)
         (if last-note-spec
             (let* ((note-duration (note-duration last-note-spec))
                    (rest-spec (make-rest #:duration (* note-duration (- 1 (*breathe-shortage*)))
                                          #:origin (ly:music-property music 'origin))))
               (set-note-duration! last-note-spec (* note-duration (*breathe-shortage*)))
               (add! (make-score-notes #:note/rest-list (list rest-spec)) result-list))
             (warning music "\\\\breathe without previous note known"))
         #t)
        ;; anything else
        (else
         #f))))
    (debug "Raw notes" result-list)))

(defstruct verse-block ; lyrics for a given piece of music
  verse-list
  (fresh #t) ; if #t, this block hasn't been yet included in the final output
  )

(defstruct parallel-blocks ; several parallel blocks (e.g. stanzas)
  block-list ; list of verse-blocks
  )

(defstruct sequential-blocks
  block-list ; list of verse-blocks
  )

(defstruct repeated-blocks
  block-list ; list of verse-blocks
  count ; number of repetitions
  )

(defstruct verse ;
  text ; separate text element (syllable or word)
  notelist/rests ; list of note lists (slurs) and rests
  (unfinished #f) ; whether to be merged with the following verse
  )

(define (find-lyrics-score score-list context accept-default)
  ;; Returns score-* element of context or #f (if there's no such any).
  (and (not (null? score-list))
       (or (find-lyrics-score* (car score-list) context accept-default)
           (find-lyrics-score (cdr score-list) context accept-default))))

(define (find-lyrics-score* score context accept-default)
  (cond
   ((and (score-voice? score)
         (equal? (score-voice-context score) context))
    score)
   ((score-voice? score)
    (find-lyrics-score (score-voice-elements score) context #f))
   ((score-choice? score)
    (letrec ((lookup (lambda (lists)
                       (if (null? lists)
                           #f
                           (or (find-lyrics-score (car lists) context accept-default)
                               (lookup (cdr lists)))))))
      (lookup (score-choice-lists score))))
   ((score-repetice? score)
    (if accept-default
        score
        (find-lyrics-score (score-repetice-elements score) context accept-default)))
   ((score-notes? score)
    (if accept-default
        score
        #f))
   (else
    (error "Unknown score element" score))))

(define (insert-lyrics! lyrics/skip-list score-list context)
  ;; Add verse-block-lists to score-list.
  ;; Each processed score-notes instance must receive at most one block in each
  ;; insert-lyrics! call.  (It can get other blocks if more pieces of
  ;; lyrics are attached to the same score part.)
  (let ((lyrics-score-list (find-lyrics-score score-list context #f)))
    (debug "Lyrics+skip list" lyrics/skip-list)
    (debug "Corresponding score-* list" score-list)
    (if lyrics-score-list
        (insert-lyrics*! lyrics/skip-list (list lyrics-score-list) context)
        (warning #f "Lyrics context not found: ~a" context))))

(define (insert-lyrics*! lyrics/skip-list score-list context)
  (debug "Processing lyrics" lyrics/skip-list)
  (debug "Processing score" score-list)
  (cond
   ((and (null? lyrics/skip-list)
         (null? score-list))
    #f)
   ((null? lyrics/skip-list)
    (warning #f "Extra notes: ~a ~a" context score-list))
   ((null? score-list)
    (warning #f "Extra lyrics: ~a ~a" context lyrics/skip-list))
   (else
    (let* ((lyrics/skip (car lyrics/skip-list))
           (lyrics-context ((if (lyrics? lyrics/skip) lyrics-context skip-context) lyrics/skip))
           (score (car score-list)))
      (cond
       ((score-voice? score)
        (let ((new-context (score-voice-context score)))
          (if (equal? new-context lyrics-context)
              (insert-lyrics*! lyrics/skip-list
                               (append (score-voice-elements score)
                                       (if (null? (cdr score-list))
                                           '()
                                           (list (make-score-voice #:context context
                                                                   #:elements (cdr score-list)))))
                               new-context)
              (insert-lyrics*! lyrics/skip-list (cdr score-list) context))))
       ((score-choice? score)
        (let* ((lists* (score-choice-lists score))
               (lists lists*)
               (n-assigned (score-choice-n-assigned score))
               (n 0)
               (allow-default #f)
               (score* #f))
          (while (and (not score*)
                      (not (null? lists)))
                 (set! score* (find-lyrics-score (car lists) lyrics-context allow-default))
                 (set! lists (cdr lists))
                 (if (not score*)
                     (set! n (+ n 1)))
                 (if (and (null? lists)
                          (not allow-default)
                          (equal? lyrics-context context))
                     (begin
                       (set! allow-default #t)
                       (set! n 0)
                       (set! lists (score-choice-lists score)))))
          (debug "Selected score" score*)
          (if (and score*
                   (>= n n-assigned))
              (begin
                (if (> n n-assigned)
                    (receive (assigned-elts unassigned-elts) (split-at lists* n-assigned)
                      (set-score-choice-lists! score (append assigned-elts
                                                             (list (list-ref lists* n))
                                                             (take unassigned-elts (- n n-assigned))
                                                             lists))))
                (set-score-choice-n-assigned! score (+ n-assigned 1))))
          (insert-lyrics*! lyrics/skip-list (append (if score* (list score*) '()) (cdr score-list)) context)))
       ((score-repetice? score)
        (insert-lyrics*! lyrics/skip-list
                         (append (score-repetice-elements score) (cdr score-list)) context))
       ((score-notes? score)
        ;; This is the only part which actually attaches the processed lyrics.
        ;; The subsequent calls return verses which we collect into a verse block.
        ;; We add the block to the score element.
        (if (equal? lyrics-context context)
            (set! lyrics/skip-list (really-insert-lyrics! lyrics/skip-list score context)))
        (insert-lyrics*! lyrics/skip-list (cdr score-list) context))
       (else
        (error "Unknown score element in lyrics processing" score)))))))

(define (really-insert-lyrics! lyrics/skip-list score context)
  ;; Return new lyrics/skip-list.
  ;; Score is modified by side effect.
  (debug "Assigning notes" score)
  (let ((note-list (score-notes-note/rest-list score))
        (unfinished-verse #f)
        (verse-list '()))
    (while (not (null? note-list))
           (if (null? lyrics/skip-list)
               (let ((final-rests '()))
                 (while (and (not (null? note-list))
                             (rest? (car note-list)))
                        (push! (car note-list) final-rests)
                        (set! note-list (cdr note-list)))
                 (if (not (null? final-rests))
                     (set! verse-list (append verse-list
                                              (list (make-verse #:text ""
                                                                #:notelist/rests (reverse! final-rests))))))
                 (if (not (null? note-list))
                     (begin
                       (warning (car note-list) "Missing lyrics: ~a ~a" context note-list)
                       (set! note-list '()))))
               (let ((lyrics/skip (car lyrics/skip-list)))
                 (receive (notelist/rest note-list*) (if (lyrics? lyrics/skip)
                                                         (consume-lyrics-notes lyrics/skip note-list context)
                                                         (consume-skip-notes lyrics/skip note-list context))
                   (debug "Consumed notes" (list lyrics/skip notelist/rest))
                   (set! note-list note-list*)
                   (cond
                    ((null? notelist/rest)
                     #f)
                    ;; Lyrics
                    ((and (lyrics? lyrics/skip)
                          unfinished-verse)
                     (set-verse-text!
                      unfinished-verse
                      (string-append (verse-text unfinished-verse) (lyrics-text lyrics/skip)))
                     (set-verse-notelist/rests!
                      unfinished-verse
                      (append (verse-notelist/rests unfinished-verse) (list notelist/rest)))
                     (if (not (lyrics-unfinished lyrics/skip))
                         (set! unfinished-verse #f)))
                    ((lyrics? lyrics/skip)
                     (let ((verse (make-verse #:text (if (rest? notelist/rest)
                                                         ""
                                                         (lyrics-text lyrics/skip))
                                              #:notelist/rests (list notelist/rest))))
                       (add! verse verse-list)
                       (set! unfinished-verse (if (lyrics-unfinished lyrics/skip) verse #f))))
                    ;; Skip
                    ((skip? lyrics/skip)
                     (cond
                      ((rest? notelist/rest)
                       (if (null? verse-list)
                           (set! verse-list (list (make-verse #:text ""
                                                              #:notelist/rests (list notelist/rest))))
                           (let ((last-verse (last verse-list)))
                             (set-verse-notelist/rests!
                              last-verse
                              (append (verse-notelist/rests last-verse) (list notelist/rest))))))
                      ((pair? notelist/rest)
                       (add! (make-verse #:text (*skip-word*) #:notelist/rests (list notelist/rest))
                             verse-list))
                      (else
                       (error "Unreachable branch reached")))
                     (set! unfinished-verse #f)))
                   (if (not (rest? notelist/rest))
                       (set! lyrics/skip-list (cdr lyrics/skip-list)))))))
    (if unfinished-verse
        (set-verse-unfinished! unfinished-verse #t))
    (set-score-notes-verse-block-list!
     score
     (append (score-notes-verse-block-list score)
             (list (make-verse-block #:verse-list verse-list)))))
  lyrics/skip-list)

(define (consume-lyrics-notes lyrics note-list context)
  ;; Returns list of note instances + new note-list.
  (assert (lyrics? lyrics))
  (if (and (not (null? note-list))
           (rest? (car note-list)))
      (values (car note-list) (cdr note-list))
      (let ((ignore-melismata (lyrics-ignore-melismata lyrics))
            (join #t)
            (consumed '()))
        (while (and join
                    (not (null? note-list)))
               (let ((note (car note-list)))
                 (push! note consumed)
                 (let ((note-slur (note-joined note)))
                   (if (< note-slur 0)
                       (warning note "Slur underrun"))
                   (set! join (and (not ignore-melismata) (> note-slur 0)))))
               (set! note-list (cdr note-list)))
        (if join
            (warning (safe-car (if (null? note-list) consumed note-list))
                     "Unfinished slur: ~a ~a" context consumed))
        (values (reverse consumed) note-list))))

(define (consume-skip-notes skip note-list context)
  ;; Returns either note list (skip word defined) or rest instance (no skip word) + new note-list.
  (assert (skip? skip))
  (let ((duration (skip-duration skip))
        (epsilon 0.001)
        (consumed '()))
    (while (and (> duration epsilon)
                (not (null? note-list)))
           (let ((note (car note-list)))
             (assert (note? note))
             (push! note consumed)
             (set! duration (- duration (note-duration note))))
           (set! note-list (cdr note-list)))
    (set! consumed (reverse! consumed))
    (cond
     ((> duration epsilon)
      (warning (if (null? note-list) (safe-last consumed) (safe-car note-list))
               "Excessive skip: ~a ~a ~a ~a" context skip duration consumed))
     ((< duration (- epsilon))
      (warning (if (null? note-list) (safe-last consumed) (safe-car note-list))
               "Skip misalignment: ~a ~a ~a ~a" context skip duration consumed)))
    (values (if (*skip-word*)
                consumed
                '())
            note-list)))

(define (extract-verse-blocks score)
  ;; Returns list of blocks and parallel blocks.
  (debug "Extracting verse blocks" score)
  (cond
   ((score-voice? score)
    (append-map extract-verse-blocks (score-voice-elements score)))
   ((score-choice? score)
    (list (make-parallel-blocks
           #:block-list (map (lambda (block-list)
                               (make-sequential-blocks
                                #:block-list (append-map extract-verse-blocks block-list)))
                             (score-choice-lists score)))))
   ((score-repetice? score)
    (list (make-repeated-blocks #:count (score-repetice-count score)
                                #:block-list (append-map extract-verse-blocks
                                                         (score-repetice-elements score)))))
   ((score-notes? score)
    (list (make-parallel-blocks #:block-list (score-notes-verse-block-list score))))
   (else
    (error "Invalid score element" score))))

(define (extract-verses score-list)
  ;; Returns (final) list of verses.
  ;; The primary purpose of this routine is to build complete stanzas from
  ;; lists of verse blocks.
  ;; Extract verse-blocks and process them until no unprocessed stanzas remain.
  (debug "Final score list" score-list)
  (let ((verse-block-list (debug "Verse blocks" (append-map extract-verse-blocks score-list))))
    (letrec ((combine (lambda (lst-1 lst-2)
                        (debug "Combining lists" (list lst-1 lst-2))
                        (if (null? lst-2)
                            lst-1
                            (let ((diff (- (length lst-1) (length lst-2))))
                              (if (< diff 0)
                                  (let ((last-elt (last lst-1)))
                                    (while (< diff 0)
                                           (add! last-elt lst-1)
                                           (set! diff (+ diff 1))))
                                  (let ((last-elt (last lst-2)))
                                    (while (> diff 0)
                                           (add! last-elt lst-2)
                                           (set! diff (- diff 1)))))
                              (debug "Combined" (map append lst-1 lst-2))))))
             (expand* (lambda (block)
                        (cond
                         ((parallel-blocks? block)
                          (append-map (lambda (block) (expand (list block)))
                                      (parallel-blocks-block-list block)))
                         ((sequential-blocks? block)
                          (expand (sequential-blocks-block-list block)))
                         ((repeated-blocks? block)
                          ;; Only simple repetice without nested parallel sections is supported.
                          (let ((count (repeated-blocks-count block))
                                (expanded (expand (repeated-blocks-block-list block)))
                                (expanded* '()))
                            (while (not (null? expanded))
                                   (let ((count* count)
                                         (item '()))
                                     (while (and (> count* 0) (not (null? expanded)))
                                            (set! item (append item (car expanded)))
                                            (set! expanded (cdr expanded))
                                            (set! count* (- count* 1)))
                                     (push! item expanded*)))
                            (reverse expanded*)))
                         (else
                          (list (list block))))))
             (expand (lambda (block-list)
                       (debug "Expanding list" block-list)
                       (if (null? block-list)
                           '()
                           (debug "Expanded" (combine (expand* (car block-list))
                                                      (expand (cdr block-list)))))))
             (merge (lambda (verse-list)
                      (cond
                       ((null? verse-list)
                        '())
                       ((verse-unfinished (car verse-list))
                        (let ((verse-1 (first verse-list))
                              (verse-2 (second verse-list)))
                          (merge (cons (make-verse #:text (string-append (verse-text verse-1)
                                                                         (verse-text verse-2))
                                                   #:notelist/rests (append (verse-notelist/rests verse-1)
                                                                            (verse-notelist/rests verse-2))
                                                   #:unfinished (verse-unfinished verse-2))
                                       (cddr verse-list)))))
                       (else
                        (cons (car verse-list) (merge (cdr verse-list))))))))
      (debug "Final verses" (merge (append-map (lambda (lst) (append-map verse-block-verse-list lst))
                                               (expand verse-block-list)))))))

(define (handle-music music)
  ;; Returns list of verses.
  ;; The main analysis function.
  (if *debug*
      (display-scheme-music music))
  (let ((score-list (debug "Final raw notes" (get-notes music)))
        (music-context-list (collect-lyrics-music music)))
    (for-each (lambda (music-context)
                (let ((context (music-context-context music-context)))
                  (set! *tempo-compression* #f)
                  (insert-lyrics! (get-lyrics (music-context-music music-context) context)
                                  score-list context)
                  (debug "Final score list" score-list)))
              music-context-list)
    (extract-verses score-list)))


;;; Output


(define festival-note-mapping '((0 "C") (1 "C#") (2 "D") (3 "D#") (4 "E") (5 "F") (6 "F#")
                                (7 "G") (8 "G#") (9 "A") (10 "A#") (11 "B")))
(define (festival-pitch pitch)
  (let* ((semitones (ly:pitch-semitones pitch))
         (octave (inexact->exact (floor (/ semitones 12))))
         (tone (modulo semitones 12)))
    (format #f "~a~a" (car (assoc-get tone festival-note-mapping))
            (+ octave (*base-octave*) (*base-octave-shift*)))))

(define (write-header port tempo)
  (let ((beats (or (tempo->beats tempo) 100)))
    (format port "<?xml version=\"1.0\"?>
<!DOCTYPE SINGING PUBLIC \"-//SINGING//DTD SINGING mark up//EN\" \"Singing.v0_1.dtd\" []>
<SINGING BPM=\"~d\">
" beats)))

(define (write-footer port)
  (format port "</SINGING>~%"))

(define (write-lyrics port music)
  (let ((rest-dur 0))
    (for-each (lambda (verse)
                (let ((text (verse-text verse))
                      (note/rest-list (verse-notelist/rests verse)))
                  (receive (rest-list note-listlist) (partition rest? note/rest-list)
                    (debug "Rest list" rest-list)
                    (debug "Note list" note-listlist)
                    (if (not (null? rest-list))
                        (set! rest-dur (+ rest-dur (apply + (map rest-duration rest-list)))))
                    (if (not (null? note-listlist))
                        (begin
                          (if (> rest-dur 0)
                              (begin
                                (write-rest-element port rest-dur)
                                (set! rest-dur 0)))
                          (write-lyrics-element port text note-listlist))))))
              (handle-music music))
    (if (> rest-dur 0)
        (write-rest-element port rest-dur))))

(define (write-lyrics-element port text slur-list)
  (let ((fmt "~{~{~a~^+~}~^,~}")
        (transform (lambda (function)
                     (map (lambda (slur)
                            (let ((rests (filter rest? slur)))
                              (if (not (null? rests))
                                  (begin
                                    (warning (car rests) "Rests in a slur: ~a" slur)
                                    (set! slur (remove rest? slur)))))
                            (map function slur))
                          slur-list)))
        (note-festival-pitch (lambda (n) (festival-pitch (note-pitch n)))))
    (format port "<DURATION BEATS=\"~@?\"><PITCH NOTE=\"~@?\">~a</PITCH></DURATION>~%"
            fmt (transform note-duration)
            fmt (transform note-festival-pitch)
            text)))

(define (write-rest-element port duration)
  (format port "<REST BEATS=\"~a\"></REST>~%" duration))
