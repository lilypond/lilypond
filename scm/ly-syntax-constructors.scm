;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2006--2015 Erik Sandberg <mandolaerik@gmail.com>
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

(define-module (scm ly-syntax-constructors)
  #:use-module (lily)
  #:use-module (srfi srfi-1))

(define-public (music-function-call-error fun m)
  (let* ((sig (ly:music-function-signature fun))
         (pred (if (pair? (car sig)) (caar sig) (car sig))))
    (ly:parser-error
     (format #f (_ "~a function cannot return ~a")
             (type-name pred)
             (value->lily-string m))
     (*location*))
    (and (pair? (car sig)) (cdar sig))))

;; Music function: Apply function and check return value.
;; args are in reverse order, rest may specify additional ones
;;
;; If args is not a proper list, an error has been flagged earlier
;; and no fallback value had been available.  In this case,
;; we don't call the function but rather return the general
;; fallback.
(define-public (music-function fun args . rest)
  (let* ((sig (ly:music-function-signature fun))
         (pred (if (pair? (car sig)) (caar sig) (car sig)))
         (good (proper-list? args))
         (m (and good (apply (ly:music-function-extract fun)
                             (reverse! args rest)))))
    (if (and good (pred m))
        (if (ly:music? m) (ly:set-origin! m) m)
        (if good
            (music-function-call-error fun m)
            (and (pair? (car sig)) (cdar sig))))))

(define-public (argument-error n pred arg)
  (ly:parser-error
   (format #f
           (_ "wrong type for argument ~a.  Expecting ~a, found ~s")
           n (type-name pred) (music->make-music arg))
   (*location*)))

(define-public (void-music)
  (ly:set-origin! (make-music 'Music)))

(define-public (sequential-music mlist)
  (ly:set-origin! (make-sequential-music mlist)))

(define-public (simultaneous-music mlist)
  (ly:set-origin! (make-simultaneous-music mlist)))

(define-public (event-chord mlist)
  (ly:set-origin! (make-music 'EventChord
                              'elements mlist)))

(define-public (unrelativable-music mus)
  (ly:set-origin! (make-music 'UnrelativableMusic
                              'element mus)))

(define-public (context-change type id)
  (ly:set-origin! (make-music 'ContextChange
                              'change-to-type type
                              'change-to-id id)))

(define-public (tempo text . rest)
  (let* ((unit (and (pair? rest)
                    (car rest)))
         (count (and unit
                     (cadr rest)))
         (range-tempo? (pair? count))
         (tempo-change (ly:set-origin! (make-music 'TempoChangeEvent
                                                   'text text
                                                   'tempo-unit unit
                                                   'metronome-count count)))
         (tempo-set
          (and unit
               (context-spec-music
                (make-property-set 'tempoWholesPerMinute
                                   (ly:moment-mul
                                    (ly:make-moment
                                     (if range-tempo?
                                         (round (/ (+ (car count) (cdr count))
                                                   2))
                                         count)
                                     1)
                                    (ly:duration-length unit)))
                'Score))))

    (if tempo-set
        (make-sequential-music (list tempo-change tempo-set))
        tempo-change)))

(define-public (repeat type num body alts)
  (ly:set-origin! (make-repeat type num body alts)))

(define (script-to-mmrest-text music)
  "Extract @code{'direction} and @code{'text} from @var{music}, and transform
into a @code{MultiMeasureTextEvent}."

  (if (music-is-of-type? music 'script-event)
      (make-music 'MultiMeasureTextEvent music)
      music))

(define-public (multi-measure-rest duration articulations)
  (ly:set-origin! (make-music 'MultiMeasureRestMusic
                              'articulations (map script-to-mmrest-text articulations)
                              'duration duration)))

(define-public (repetition-chord duration articulations)
  (ly:set-origin! (make-music 'EventChord
                              'duration duration
                              'elements articulations)))

(define-public (context-specification type id ops create-new mus)
  (let ((csm (context-spec-music mus type id)))
    (set! (ly:music-property csm 'property-operations) ops)
    (if create-new (set! (ly:music-property csm 'create-new) #t))
    (ly:set-origin! csm)))

(define-public (composed-markup-list commands markups)
  ;; `markups' being a list of markups, eg (markup1 markup2 markup3),
  ;; and `commands' a list of commands with their scheme arguments, in reverse order,
  ;; eg: ((italic) (raise 4) (bold)), maps the commands on each markup argument, eg:
  ;;  ((bold (raise 4 (italic markup1)))
  ;;   (bold (raise 4 (italic markup2)))
  ;;   (bold (raise 4 (italic markup3))))

  (define (compose arg)
    (fold
     (lambda (cmd prev) (append cmd (list prev)))
     arg
     commands))
  (let loop ((markups markups) (completed '()))
    (cond ((null? markups) (reverse! completed))
          ((markup? (car markups))
           (loop (cdr markups)
                 (cons (compose (car markups)) completed)))
          (else
           (call-with-values
               (lambda () (break! markup? markups))
             (lambda (complex rest)
               (loop rest
                     (reverse!
                      (make-map-markup-commands-markup-list
                       compose complex) completed))))))))

(define-public (property-operation ctx music-type symbol . args)
  (let* ((props (case music-type
                  ((PropertySet) (list 'value (car args)))
                  ((PropertyUnset) '())
                  ((OverrideProperty) (list 'grob-value (car args)
                                            'grob-property-path (if (list? (cadr args))
                                                                    (cadr args)
                                                                    (cdr args))
                                            'pop-first #t))
                  ((RevertProperty)
                   (if (list? (car args))
                       (list 'grob-property-path (car args))
                       (list 'grob-property-path args)))
                  (else (ly:error (_ "Invalid property operation ~a") music-type))))
         (m (ly:set-origin! (apply make-music music-type
                                   'symbol symbol
                                   props))))
    (ly:set-origin! (make-music 'ContextSpeccedMusic
                                'element m
                                'context-type ctx))))

(define (get-first-context-id! mus)
  "Find the name of a ContextSpeccedMusic, possibly naming it"
  (let ((id (ly:music-property mus 'context-id)))
    (if (eq? (ly:music-property mus 'name) 'ContextSpeccedMusic)
        (if (and (string? id)
                 (not (string-null? id)))
            id
            ;; We may reliably give a new context a unique name, but
            ;; not an existing one
            (if (ly:music-property mus 'create-new #f)
                (let ((id (get-next-unique-voice-name)))
                  (set! (ly:music-property mus 'context-id) id)
                  id)
                '()))
        '())))

(define-public (lyric-event text duration)
  (ly:set-origin! (make-lyric-event text duration)))

(define-public (lyric-combine sync sync-type music)
  ;; CompletizeExtenderEvent is added following the last lyric in MUSIC
  ;; to signal to the Extender_engraver that any pending extender should
  ;; be completed if the lyrics end before the associated voice.
  (append! (ly:music-property music 'elements)
           (list (make-music 'CompletizeExtenderEvent)))
  (ly:set-origin!
   (make-music 'LyricCombineMusic
               'element music
               'associated-context sync
               'associated-context-type sync-type)))

(define-public (add-lyrics music addlyrics-list)
  (let* ((existing-voice-name (get-first-context-id! music))
         (voice-name (if (string? existing-voice-name)
                         existing-voice-name
                         (get-next-unique-voice-name)))
         (voice (if (string? existing-voice-name)
                    music
                    (make-music 'ContextSpeccedMusic
                                'element music
                                'context-type 'Voice
                                'context-id voice-name
                                'origin (ly:music-property music 'origin))))
         (voice-type (ly:music-property voice 'context-type))
         (lyricstos (map
                     (lambda (mus)
                       (with-location
                        (ly:music-property mus 'origin)
                        (ly:set-origin! (make-music 'ContextSpeccedMusic
                                                    'create-new #t
                                                    'context-type 'Lyrics
                                                    'element
                                                    (lyric-combine
                                                     voice-name voice-type mus)))))
                     addlyrics-list)))
    (make-simultaneous-music (cons voice lyricstos))))
