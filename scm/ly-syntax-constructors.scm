;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2006--2022 Erik Sandberg <mandolaerik@gmail.com>
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

(define-module (lily ly-syntax-constructors)
  #:use-module (lily)
  #:use-module (srfi srfi-1)
  #:use-module (lily display-lily))

(define-public (music-function-call-error fun m)
  (let* ((sigcar (car (ly:music-function-signature fun)))
         (pred? (if (pair? sigcar) (car sigcar) sigcar)))
    (ly:parser-error
     (format #f (G_ "~a function cannot return ~a")
             (type-name pred?)
             (value->lily-string m))
     (*location*))
    (and (pair? sigcar)
         (if (ly:music? (cdr sigcar))
             (ly:music-deep-copy (cdr sigcar) (*location*))
             (cdr sigcar)))))

;; Music function: Apply function and check return value.
;; args are in reverse order.
;;
;; If args is not a proper list, an error has been flagged earlier
;; and no fallback value had been available.  In this case,
;; we don't call the function but rather return the general
;; fallback.
(define-public (music-function fun args)
  (let* ((sigcar (car (ly:music-function-signature fun)))
         (pred? (if (pair? sigcar) (car sigcar) sigcar))
         (good (list? args))
         (m (and good (apply (ly:music-function-extract fun) (reverse! args)))))
    (if good
        (if (pred? m)
            (if (ly:music? m) (ly:set-origin! m) m)
            (music-function-call-error fun m))
        (and (pair? sigcar)
             (if (ly:music? (cdr sigcar))
                 (ly:music-deep-copy (cdr sigcar) (*location*))
                 (cdr sigcar))))))

(define-public (argument-error n pred arg)
  (ly:parser-error
   (format #f
           (G_ "wrong type for argument ~a.  Expecting ~a, found ~s")
           n (type-name pred) (music->make-music arg))
   (*location*)))

;; Used for chaining several music functions together.  `final'
;; contains the last argument and still needs typechecking.
(define (music-function-chain call final)
  (let* ((fun (car call))
         (siglast (last (ly:music-function-signature fun)))
         (pred? (if (pair? siglast) (car siglast) siglast)))
    (if (pred? final)
        (music-function fun (cons final (cdr call)))
        (begin
          (argument-error (length call) pred? final)
          ;; call music function just for the error return value
          (music-function fun #f)))))

(define-public (partial-music-function call-list)
  (let* ((good (every list? call-list))
         (sig (ly:music-function-signature (caar call-list)))
         (headsig (ly:music-function-signature (car (last call-list)))))
    (and good
         (ly:make-music-function
          (cons (car headsig) (list-tail sig (length (car call-list))))
          (lambda rest
            ;; Every time we use music-function, it destructively
            ;; reverses its list of arguments.  Changing the calling
            ;; convention would be non-trivial since we do error
            ;; propagation to the reversed argument list by making it
            ;; a non-proper list.  So we just create a fresh copy of
            ;; all argument lists for each call.  We also want to
            ;; avoid reusing any music expressions without copying and
            ;; want to let them point to the location of the music
            ;; function call rather than its definition.
            (let ((call-list (ly:music-deep-copy call-list (*location*))))
              (fold music-function-chain
                    (music-function (caar call-list)
                                    (reverse! rest (cdar call-list)))
                    (cdr call-list))))))))

(define-public (partial-text-script partial-markup)
  (ly:make-music-function
   (cons (cons ly:event? #f)
         (cons* ly:dir? markup-function? (markup-command-signature partial-markup)))
   (lambda (direction partial-markup . rest)
     (make-music 'TextScriptEvent
                 'text (cons partial-markup rest)
                 (if (zero? direction) '()
                     (list (cons 'direction direction)))))))

(define-public (create-script item)
  (cond ((ly:event? item) (ly:set-origin! item))
        ((markup? item) (ly:set-origin! (make-music 'TextScriptEvent 'text item)))
        (else
         (ly:parser-error (G_ "not an articulation") (*location*))
         *unspecified*)))

;; We use define-syntax-function here with a slightly fishy "fallback
;; return value" which is not actually music but #f.  The default
;; expression of define-event-function would have a music fallback
;; triggering "Parsed object should be dead" warnings for music
;; objects outside of the current parser session/module.  The called
;; functions always deliver music and are used from the parser in a
;; manner where only the last argument is provided from outside the
;; parser, and its predicate "scheme?" is always true.  So the
;; fallback value will never get used and its improper type is no
;; issue.

(define-public create-script-function
  (define-syntax-function ly:event? (dir item) (ly:dir? scheme?)
    (let ((res (create-script item)))
      (if (ly:event? res)
          (begin
            (if (not (zero? dir))
                (set! (ly:music-property res 'direction) dir))
            res)
          (make-music 'PostEvents)))))

(define-public (void-music)
  (ly:set-origin! (make-music 'Music)))

(define-public (sequential-alternative-music mlist)
  (ly:set-origin! (make-music
                   'SequentialAlternativeMusic
                   'elements mlist)))

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

(define-public repeat
  (define-syntax-function ly:music?
    (type num body)
    (string? index? ly:music?)
    (ly:set-origin! (make-repeat type num body '()))))

(define-public (repeat-alt type num body alts)
  (ly:set-origin! (make-repeat type num body alts)))

(define (script-to-mmrest-text music)
  "Extract @code{'direction} and @code{'text} from @var{music}, and transform
into a @code{MultiMeasureTextEvent}."

  (cond
   ((music-is-of-type? music 'text-script-event)
    (make-music 'MultiMeasureTextEvent music))
   ((music-is-of-type? music 'articulation-event)
    (make-music 'MultiMeasureArticulationEvent music))
   (else music)))

(define-public (multi-measure-rest duration articulations)
  (ly:set-origin! (make-music 'MultiMeasureRestMusic
                              'articulations (map script-to-mmrest-text articulations)
                              'duration duration)))

(define-public (repetition-chord duration articulations)
  (ly:set-origin! (make-music 'EventChord
                              'duration duration
                              'elements articulations)))

(define-public (context-create type id ops mus)
  (let ((csm (context-spec-music mus type id ops)))
    (set! (ly:music-property csm 'create-new) #t)
    (ly:set-origin! csm)))

(define-public (context-find-or-create type id ops mus)
  (ly:set-origin! (context-spec-music mus type id ops)))

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

(define-public (partial-markup commands)
  ;; Like composed-markup-list, except that the result is a single
  ;; markup command that can be applied to one markup
  (define (compose rest)
    (fold
     (lambda (cmd prev) (append cmd (list prev)))
     (append (car commands) rest)
     (cdr commands)))
  (let ((chain (lambda (layout props . rest)
                 (interpret-markup layout props (compose rest)))))
    (set! (markup-command-signature chain)
          (list-tail
           (markup-command-signature (caar commands))
           (length (cdar commands))))
    chain))

;; See create-script-function for the rationale of using
;; define-syntax-function instead of define-music-function here.
(define-public property-set
  (define-syntax-function ly:music?
    (context property value) (symbol? symbol? scheme?)
    (context-spec-music
     (ly:set-origin!
      (make-music 'PropertySet
                  'symbol property
                  'value value))
     context)))

(define-public (property-unset context property)
  (ly:set-origin! (context-spec-music
                   (ly:set-origin!
                    (make-music 'PropertyUnset
                                'symbol property))
                   context)))

(define-public property-override
  (define-syntax-function ly:music?
    (context path value) (symbol? symbol-list? scheme?)
    (context-spec-music
     (ly:set-origin!
      (make-music 'OverrideProperty
                  'symbol (car path)
                  'grob-property-path (cdr path)
                  'grob-value value
                  'pop-first #t))
     context)))

(define-public (property-revert context path)
  (ly:set-origin! (context-spec-music
                   (ly:set-origin!
                    (make-music 'RevertProperty
                                'symbol (car path)
                                'grob-property-path (cdr path)))
                   context)))

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

(define (make-lyric-combine sync sync-type music)
  ;; CompletizeExtenderEvent is added following the last lyric in MUSIC
  ;; to signal to the Extender_engraver that any pending extender should
  ;; be completed if the lyrics end before the associated voice.
  (append! (ly:music-property music 'elements)
           (list (make-music 'CompletizeExtenderEvent)))
  (make-music 'LyricCombineMusic
              'element music
              'associated-context sync
              'associated-context-type sync-type))

(define-public (lyric-combine sync sync-type music)
  (ly:set-origin! (make-lyric-combine sync sync-type music)))

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
                     (lambda (mus+mods)
                       (let* ((mus (car mus+mods))
                              (mods (cdr mus+mods))
                              (loc (ly:music-property mus 'origin))
                              (el (make-lyric-combine voice-name voice-type mus)))
                         (ly:set-origin! el loc)
                         (ly:set-origin! (make-music 'ContextSpeccedMusic
                                                     'create-new #t
                                                     'context-type 'Lyrics
                                                     'property-operations mods
                                                     'element el)
                                         loc)
                         ))
                     addlyrics-list)))
    (make-simultaneous-music (cons voice lyricstos))))
