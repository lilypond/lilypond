;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2006--2012 Erik Sandberg <mandolaerik@gmail.com>
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

;; TODO: use separate module for syntax
;; constructors. Also create wrapper around the constructor?
(defmacro define-ly-syntax (args . body)
  `(define-public ,args ,@body))

;; A ly-syntax constructor takes two extra parameters, parser and
;; location. These are mainly used for reporting errors and
;; warnings. This function is a syntactic sugar which uses the
;; location arg to set the origin of the returned music object; this
;; behaviour is usually desired
(defmacro define-ly-syntax-loc (args . body)
  `(define-public ,args
     (let ((m ,(cons 'begin body)))
       (set! (ly:music-property m 'origin) ,(third args))
       m)))
;; Like define-ly-syntax-loc, but adds parser and location
;; parameters. Useful for simple constructors that don't need to
;; report errors.
(defmacro define-ly-syntax-simple (args . body)
  `(define-public ,(cons* (car args)
                          'parser
                          'location
                          (cdr args))
     (let ((m ,(cons 'begin body)))
       (set! (ly:music-property m 'origin) location)
       m)))

;; Music function: Apply function and check return value.
;; args are in reverse order, rest may specify additional ones
;;
;; If args is not a proper list, an error has been flagged earlier
;; and no fallback value had been available.  In this case,
;; we don't call the function but rather return the general
;; fallback.
(define-ly-syntax (music-function parser loc fun args . rest)
  (let* ((sig (ly:music-function-signature fun))
         (pred (if (pair? (car sig)) (caar sig) (car sig)))
         (good (proper-list? args))
         (m (and good (apply (ly:music-function-extract fun)
                             parser loc (reverse! args rest)))))
    (if (and good (pred m))
        (begin
          (if (ly:music? m)
              (set! (ly:music-property m 'origin) loc))
          m)
        (begin
          (if good
              (ly:parser-error parser
                               (format #f (_ "~a function cannot return ~a")
                                       (type-name pred) m)
                               loc))
          (and (pair? (car sig)) (cdar sig))))))

(define-ly-syntax (argument-error parser location n pred arg)
  (ly:parser-error
   parser
   (format #f
           (_ "wrong type for argument ~a.  Expecting ~a, found ~s")
           n (type-name pred) (music->make-music arg))
   location))

(define-ly-syntax-simple (void-music)
  (make-music 'Music))

(define-ly-syntax-simple (sequential-music mlist)
  (make-sequential-music mlist))

(define-ly-syntax-simple (simultaneous-music mlist)
  (make-simultaneous-music mlist))

(define-ly-syntax-simple (event-chord mlist)
  (make-music 'EventChord
              'elements mlist))

(define-ly-syntax-simple (unrelativable-music mus)
  (make-music 'UnrelativableMusic
              'element mus))

(define-ly-syntax-simple (context-change type id)
  (make-music 'ContextChange
              'change-to-type type
              'change-to-id id))

(define-ly-syntax (tempo parser location text . rest)
  (let* ((unit (and (pair? rest)
                    (car rest)))
         (count (and unit
                     (cadr rest)))
         (range-tempo? (pair? count))
         (tempo-change (make-music 'TempoChangeEvent
                                   'origin location
                                   'text text
                                   'tempo-unit unit
                                   'metronome-count count))
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

(define-ly-syntax-simple (repeat type num body alts)
  (make-repeat type num body alts))

(define (script-to-mmrest-text music)
  "Extract @code{'direction} and @code{'text} from @var{music}, and transform
into a @code{MultiMeasureTextEvent}."

  (if (music-is-of-type? music 'script-event)
      (make-music 'MultiMeasureTextEvent music)
      music))

(define-ly-syntax (multi-measure-rest parser location duration articulations)
  (make-music 'MultiMeasureRestMusic
              'articulations (map script-to-mmrest-text articulations)
              'duration duration
              'origin location))

(define-ly-syntax (repetition-chord parser location duration articulations)
  (make-music 'EventChord
              'duration duration
              'elements articulations
              'origin location))

(define-ly-syntax-simple (context-specification type id ops create-new mus)
  (let ((csm (context-spec-music mus type id)))
    (set! (ly:music-property csm 'property-operations) ops)
    (if create-new (set! (ly:music-property csm 'create-new) #t))
    csm))

(define-ly-syntax (composed-markup-list parser location commands markups)
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

(define-ly-syntax (property-operation parser location ctx music-type symbol . args)
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
         (m (apply make-music music-type
                   'symbol symbol
                   'origin location
                   props)))
    (make-music 'ContextSpeccedMusic
                'element m
                'context-type ctx
                'origin location)))

;; TODO: It seems that this function rarely returns anything useful.
(define (get-first-context-id type mus)
  "Find the name of a ContextSpeccedMusic with given type"
  (let ((id (ly:music-property mus 'context-id)))
    (if (and (eq? (ly:music-property mus 'type) 'ContextSpeccedMusic)
             (eq? (ly:music-property mus 'context-type) type)
             (string? id)
             (not (string-null? id)))
        id
        '())))

(define unique-counter -1)
(define (get-next-unique-voice-name)
  (set! unique-counter (1+ unique-counter))
  (call-with-output-string (lambda (p) (format p "uniqueContext~s" unique-counter))))

(define-ly-syntax-simple (lyric-event text duration)
  (make-lyric-event text duration))

(define (lyric-combine-music sync music loc)
  ;; CompletizeExtenderEvent is added following the last lyric in MUSIC
  ;; to signal to the Extender_engraver that any pending extender should
  ;; be completed if the lyrics end before the associated voice.
  (append! (ly:music-property music 'elements)
           (list (make-music 'CompletizeExtenderEvent)))
  (make-music 'LyricCombineMusic
              'element music
              'associated-context sync
              'origin loc))

(define-ly-syntax (lyric-combine parser location voice music)
  (lyric-combine-music voice music location))

(define-ly-syntax (add-lyrics parser location music addlyrics-list)
  (let* ((existing-voice-name (get-first-context-id 'Voice music))
         (voice-name (if (string? existing-voice-name)
                         existing-voice-name
                         (get-next-unique-voice-name)))
         (voice (if (string? existing-voice-name)
                    (music)
                    (make-music 'ContextSpeccedMusic
                                'element music
                                'context-type 'Voice
                                'context-id voice-name
                                'origin (ly:music-property music 'origin))))
         (lyricstos (map (lambda (mus)
                           (let* ((loc (ly:music-property mus 'origin))
                                  (lyr (lyric-combine-music voice-name mus loc)))
                             (make-music 'ContextSpeccedMusic
                                         'create-new #t
                                         'context-type 'Lyrics
                                         'element lyr
                                         'origin loc)))
                         addlyrics-list)))
    (make-simultaneous-music (cons voice lyricstos))))
