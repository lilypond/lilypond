;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2020--2026 Thomas Morley <thomasmorley65@gmail.com>
;;;;                          Jean Abou Samra <jean@abou-samra.fr>
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

(define (finger-key-glide mus-arts il rl glides)
  "Return a list of sublists, where each sublist's first entry is the name of
the grob where the @code{FingerGlideSpanner} starts: @code{Fingering},
@code{StringNumber} or @code{StrokeFinger}.  The second entry is @code{id},
@code{text}, @code{digit}, etc., which serves as a key later on.
The third entry is the corrsponding @code{finger-glide-event}.

We first work on simple events here in order to ensure the correct order,
Thus we finally need to exchange the simple event with the correct
stream-event.

@var{mus-arts} is supposed to be a list of articulations derived from
@code{music-cause} of a @code{note-event}.
@var{il} is an intermediate list.  It is started with a
@code{finger-glide-event} filled with the relevant finger-grob and its name.
Then it is moved to @var{rl} as a sublist. @var{rl} is the final result and
return value."

  (let* ((finger-glide?
          (lambda (ev)
            (eq? (ly:prob-property ev 'name) 'FingerGlideEvent)))
         (fingering?
          (lambda (ev)
            (eq? (ly:prob-property ev 'name) 'FingeringEvent)))
         (string-number?
          (lambda (ev)
            (eq? (ly:prob-property ev 'name) 'StringNumberEvent)))
         (stroke-finger?
          (lambda (ev)
            (eq? (ly:prob-property ev 'name) 'StrokeFingerEvent))))
    (if (null? mus-arts)
        rl
        (cond
         ((finger-glide? (car mus-arts))
          (let ((glide-others
                 ;; We need to get finger-glide-stream-events, which
                 ;; (car mus-arts) is not.
                 ;; Replace (car mus-arts) with the relevant stream-event,
                 ;; looking at the 'origin of (car mus-arts) and 'origin
                 ;; of the 'music-cause of stream-events in `glide`.
                 ;;
                 ;; Thus we split `glide` into two list, the first only
                 ;; takes the current finger-glide-stream-event, the
                 ;; second the others.
                 ;;
                 ;; TODO The direct call of equal? is always #t, why do
                 ;;      we have to compare 'origin?
                 (call-with-values
                     (lambda ()
                       (partition
                        (lambda (gl)
                          (equal?
                           (ly:prob-property (car mus-arts) 'origin)
                           (ly:prob-property
                            (ly:event-property gl 'music-cause)
                            'origin)))
                        glides))
                   (lambda (a b) (list (car a) b)))))
            (finger-key-glide
             (cdr mus-arts)
             (cons (car glide-others) il)
             rl
             (cadr glide-others))))
         ((fingering? (car mus-arts))
          (if (null? il)
              (finger-key-glide (cdr mus-arts) il rl glides)
              (let ((digit (ly:prob-property (car mus-arts) 'digit #f))
                    (text (ly:prob-property (car mus-arts) 'text #f))
                    (id (ly:prob-property (car mus-arts) 'id #f)))
                (finger-key-glide (cdr mus-arts)
                                  '()
                                  (cons (cons* 'Fingering (or id digit text) il) rl)
                                  glides))))
         ((string-number? (car mus-arts))
          (if (null? il)
              (finger-key-glide (cdr mus-arts) il rl glides)
              (let ((string-number
                     (ly:prob-property (car mus-arts) 'string-number #f))
                    (id (ly:prob-property (car mus-arts) 'id #f)))
                (finger-key-glide (cdr mus-arts)
                                  '()
                                  (cons (cons* 'StringNumber (or id string-number) il) rl)
                                  glides))))
         ((stroke-finger? (car mus-arts))
          (if (null? il)
              (finger-key-glide (cdr mus-arts) il rl glides)
              (let ((stroke-finger-digit
                     (ly:prob-property
                      (car mus-arts)
                      'stroke-finger-digit
                      #f))
                    (stroke-finger-text
                     (ly:prob-property
                      (car mus-arts)
                      'stroke-finger-text
                      #f))
                    (id (ly:prob-property (car mus-arts) 'id #f)))
                (finger-key-glide
                 (cdr mus-arts)
                 '()
                 (cons
                  (cons*
                   'StrokeFinger
                   (or id stroke-finger-digit stroke-finger-text)
                   il)
                  rl)
                 glides))))
         (else (finger-key-glide (cdr mus-arts) il rl glides))))))

(define-public Finger_glide_engraver
  (lambda (context)
    (let (;; Maps fingering numbers to fingering glide events
          ;;      string numbers to string-number glide events
          ;;      stroke fingers to stroke-finger glide events
          (finger-glide-event '())
          (string-number-glide-event '())
          (stroke-finger-glide-event '())
          ;; Maps fingering numbers to fingering glide grobs
          ;;      string numbers to string-number glide grobs
          ;;      stroke fingers to stroke-finger glide grobs
          (finger-glide-grobs '())
          (string-number-glide-grobs '())
          (stroke-finger-glide-grobs '()))
      (make-engraver
       (listeners
        ((note-event this-engraver event)
         (let* ((event-arts (ly:event-property event 'articulations))
                (music-cause (ly:event-property event 'music-cause))
                (music-arts (ly:prob-property music-cause 'articulations))
                (glide-events '()))

           ;; Find FingerGlideEvents and accumulate them in glide-events here.
           ;; NB These are stream-events.
           (for-each
            (lambda (art-ev)
              (when (memq 'finger-glide-event (ly:event-property art-ev 'class))
                (set! glide-events (cons art-ev glide-events))))
            event-arts)

           ;; Fill the lists finger-glide-event, string-number-glide-event, and
           ;; stroke-finger-glide-event with the id, digit, text etc and the
           ;; starting finger-glide-event.
           (for-each
            (lambda (ls)
              (when (eq? (car ls) 'Fingering)
                (set! finger-glide-event
                      (acons (cadr ls) (last ls) finger-glide-event)))
              (when (eq? (car ls) 'StringNumber)
                (set! string-number-glide-event
                      (acons (cadr ls) (last ls) string-number-glide-event)))
              (when (eq? (car ls) 'StrokeFinger)
                (set! stroke-finger-glide-event
                      (acons (cadr ls) (last ls) stroke-finger-glide-event))))
            (finger-key-glide music-arts '() '() glide-events)))))
       (acknowledgers
        ((stroke-finger-interface this-engraver grob source-engraver)
         (let* ((cause (ly:grob-property grob 'cause))
                (stroke-finger-digit
                 (ly:prob-property cause 'stroke-finger-digit #f))
                (stroke-finger-text
                 (ly:prob-property cause 'stroke-finger-text #f))
                (id (ly:prob-property cause 'id #f))
                (stroke-finger-glide-evt
                 (assoc-get (or id stroke-finger-digit stroke-finger-text)
                            stroke-finger-glide-event))
                (new-glide-grob
                 (if stroke-finger-glide-evt
                     (ly:engraver-make-grob
                      this-engraver
                      'FingerGlideSpanner
                      stroke-finger-glide-evt)
                     #f)))
           ;; Set right bound, select the grob via its 'id or
           ;; stroke-finger-digit/text from `stroke-finger-glide-grobs`,
           ;; 'id is preferred.
           (let* ((relevant-grob
                   (assoc-get (or id stroke-finger-digit stroke-finger-text)
                              stroke-finger-glide-grobs)))
             (when relevant-grob
               (ly:spanner-set-bound! relevant-grob RIGHT grob)
               (ly:engraver-announce-end-grob this-engraver relevant-grob grob)
               (set! stroke-finger-glide-grobs
                     (assoc-remove!
                      stroke-finger-glide-grobs
                      (or id stroke-finger-digit stroke-finger-text)))))
           ;; Set left bound and store the 'id or stroke-finger-digit/text with
           ;; the created grob as a pair in local `stroke-finger-glide-grobs`,
           ;; 'id is preferred.
           (when new-glide-grob
             (set! stroke-finger-glide-grobs
                   (acons (or id stroke-finger-digit stroke-finger-text)
                          new-glide-grob
                          stroke-finger-glide-grobs))
             (ly:spanner-set-bound! new-glide-grob LEFT grob)
             (set! stroke-finger-glide-event
                   (assoc-remove!
                    stroke-finger-glide-event
                    (or id stroke-finger-digit stroke-finger-text))))))
        ((string-number-interface this-engraver grob source-engraver)
         (let* ((cause (ly:grob-property grob 'cause))
                (string-number (ly:prob-property cause 'string-number #f))
                (id (ly:prob-property cause 'id #f))
                (string-number-glide-evt
                 (assoc-get (or id string-number) string-number-glide-event))
                (new-glide-grob
                 (if string-number-glide-evt
                     (ly:engraver-make-grob
                      this-engraver
                      'FingerGlideSpanner
                      string-number-glide-evt)
                     #f)))
           ;; Set right bound, select the grob via its 'id or string-number from
           ;; `string-number-glide-grobs`, 'id is preferred.
           (let* ((relevant-grob
                   (assoc-get (or id string-number)
                              string-number-glide-grobs)))
             (when relevant-grob
               (ly:spanner-set-bound! relevant-grob RIGHT grob)
               (ly:engraver-announce-end-grob this-engraver relevant-grob grob)
               (set! string-number-glide-grobs
                     (assv-remove! string-number-glide-grobs
                                   (or id string-number)))))
           ;; Set left bound and store the 'id or string-number with the created
           ;; grob as a pair in local `string-number-glide-grobs`, 'id is
           ;; preferred.
           (when new-glide-grob
             (set! string-number-glide-grobs
                   (acons (or id string-number) new-glide-grob
                          string-number-glide-grobs))
             (ly:spanner-set-bound! new-glide-grob LEFT grob)
             (set! string-number-glide-event
                   (assv-remove! string-number-glide-event
                                 (or id string-number))))))
        ((finger-interface this-engraver grob source-engraver)
         (let* ((cause (ly:grob-property grob 'cause))
                (digit (ly:prob-property cause 'digit #f))
                (text (ly:prob-property cause 'text #f))
                (id (ly:prob-property cause 'id #f))
                (digit-glide-evt
                 (assoc-get (or id digit text) finger-glide-event))
                (new-glide-grob
                 (if digit-glide-evt
                     (ly:engraver-make-grob
                      this-engraver
                      'FingerGlideSpanner
                      digit-glide-evt)
                     #f)))
           ;; Set right bound, select the grob via its 'id, digit or text from
           ;; `finger-glide-grobs`, 'id is preferred.
           (let* ((relevant-grob
                   (assoc-get (or id digit text) finger-glide-grobs)))
             (when relevant-grob
               (ly:spanner-set-bound! relevant-grob RIGHT grob)
               (ly:engraver-announce-end-grob this-engraver relevant-grob grob)
               (set! finger-glide-grobs
                     (assoc-remove! finger-glide-grobs (or id digit text)))))
           ;; Set left bound and store the 'id. digit or text with the created
           ;; grob as a pair in local `finger-glide-grobs`, 'id is preferred.
           (when new-glide-grob
             (set! finger-glide-grobs
                   (acons (or id digit text) new-glide-grob finger-glide-grobs))
             (ly:spanner-set-bound! new-glide-grob LEFT grob)
             (set! finger-glide-event
                   (assoc-remove! finger-glide-event (or id digit text)))))))
       ((finalize this-engraver)
        ;; Warn for a created grob without right bound, suicide the grob.
        (for-each
         (lambda (grob-entry)
           (ly:event-warning
            (event-cause (cdr grob-entry))
            (G_ "unterminated finger glide spanner"))
           (ly:grob-suicide! (cdr grob-entry)))
         (append
          finger-glide-grobs
          string-number-glide-grobs
          stroke-finger-glide-grobs)))))))

(ly:register-translator
 Finger_glide_engraver 'Finger_glide_engraver
 '((grobs-created . (FingerGlideSpanner))
   (events-accepted . (note-event))
   (properties-read . ())
   (properties-written . ())
   (description . "\
Engraver to print a line between two @code{Fingering}, @code{StringNumber} or
@code{StrokeFinger} grobs.")))
