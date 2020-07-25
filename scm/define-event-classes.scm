;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2005--2020 Erik Sandberg <mandolaerik@gmail.com>
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


(use-modules (srfi srfi-1))

;; Event class hierarchy. Each line is on the form (Parent . (List of children))
(define event-classes
  '((() . (StreamEvent))
    (StreamEvent .
                 (RemoveContext
                  ChangeParent Override Revert UnsetProperty SetProperty
                  music-event OldMusicEvent CreateContext Prepare
                  OneTimeStep Finish GraceChange))
    (music-event . (annotate-output-event
                    footnote-event arpeggio-event breathing-event
                    extender-event span-event rhythmic-event dynamic-event
                    break-event label-event percent-event key-change-event
                    string-number-event stroke-finger-event tie-event
                    part-combine-event
                    beam-forbid-event script-event tempo-change-event
                    tremolo-event bend-after-event fingering-event
                    glissando-event harmonic-event hyphen-event
                    laissez-vibrer-event mark-event multi-measure-text-event
                    multi-measure-articulation-event
                    note-grouping-event pes-or-flexa-event repeat-tie-event
                    spacing-section-event layout-instruction-event
                    time-signature-event
                    completize-extender-event break-span-event alternative-event
                    vowel-transition-event
                    duration-line-event))

    (layout-instruction-event . (apply-output-event))
    (script-event . (articulation-event text-script-event))
    (part-combine-event . (solo-one-event solo-two-event unisono-event))
    (break-event . (line-break-event page-break-event page-turn-event))
    (dynamic-event . (absolute-dynamic-event))
    (span-event . (span-dynamic-event
                   beam-event episema-event ligature-event
                   measure-spanner-event
                   measure-counter-event pedal-event
                   phrasing-slur-event slur-event
                   staff-span-event text-span-event
                   trill-span-event tremolo-span-event
                   tuplet-span-event))
    (span-dynamic-event . (decrescendo-event crescendo-event))
    (break-span-event . (break-dynamic-span-event))
    (pedal-event . (sostenuto-event sustain-event una-corda-event))
    (rhythmic-event . (lyric-event
                       melodic-event multi-measure-rest-event
                       double-percent-event percent-event
                       repeat-slash-event rest-event
                       skip-event bass-figure-event))
    (melodic-event . (cluster-note-event note-event))
    (() . (Announcement))
    (Announcement . (AnnounceNewContext))
    ))

(define-public (event-class-cons class parent classlist)
  (let ((lineage (assq parent classlist)))
    (if (not lineage)
        (begin
          (if (not (null? parent))
              (ly:warning (_ "unknown parent class `~a'") parent))
          (set! lineage '())))
    (if (symbol? class)
        (acons class lineage classlist)
        (fold (lambda (elt alist)
                (acons elt lineage alist))
              classlist class))))

(define all-event-classes
  (fold (lambda (elt classlist)
          (event-class-cons (cdr elt) (car elt) classlist))
        '() event-classes))

;; Maps event-class to a list of ancestors (inclusive)
(define ancestor-lookup (make-hash-table (length all-event-classes)))

(define (ancestor-lookup-initialize)
  (hash-clear! ancestor-lookup)
  (for-each (lambda (ent) (hashq-set! ancestor-lookup (car ent) ent))
            all-event-classes))

(ancestor-lookup-initialize)
(call-after-session ancestor-lookup-initialize)

;; Each class will be defined as
;; (class parent grandparent .. )
;; so that (eq? (cdr class) parent) holds.

(define-public (define-event-class class parent)
  "Defines a new event @code{class} derived from @code{parent}, a
previously defined event class."
  (let ((parentclass (ly:make-event-class parent)))
    (cond
     ((ly:make-event-class class)
      (ly:error (_ "Cannot redefine event class `~S'") class))
     ((not parentclass)
      (ly:error (_ "Undefined parent event class `~S'") parentclass))
     (else
      (hashq-set! ancestor-lookup
                  class
                  (cons class parentclass))))
    *unspecified*))

;; TODO: Allow entering more complex classes, by taking unions.
(define-public (ly:make-event-class leaf)
  (hashq-ref ancestor-lookup leaf))

(define-public (ly:in-event-class? ev cl)
  "Does event @var{ev} belong to event class @var{cl}?"
  (memq cl (ly:event-property ev 'class)))

;; does this exist in guile already?
(define (map-tree f t)
  (cond
   ((list? t)
    (map (lambda (x) (map-tree f x)) t))
   ((pair? t)
    (cons (map-tree f (car t)) (map-tree f (cdr t))))
   (else (f t))))

;; expand each non-leaf subtree to (root . children), recursively
(define (expand-event-tree root)
  (let ((children (assq root event-classes)))
    (if children
        (cons root (map expand-event-tree (cdr children)))
        root)))

;; produce neater representation of music event tree.
;; TODO: switch to this representation for the event-classes list?
(define music-event-tree (expand-event-tree 'music-event))
(define (sort-tree t)
  (define (stringify el)
    (if (symbol? el)
        (symbol->string el)
        (symbol->string (first el))))
  (if (list? t)
      (sort (map (lambda (el)
                   (if (list? el)
                       (cons (car el) (sort-tree (cdr el)))
                       el))
                 t)
            (lambda (a b) (string<? (stringify a) (stringify b))))
      t))

;;(use-modules (ice-9 pretty-print))
;;(pretty-print (cons (car music-event-tree) (sort-tree (cdr music-event-tree))))

(defmacro-public make-stream-event (expr)
  (ly:stream-event::undump (primitive-eval (list 'quasiquote expr))))

(define* (simplify e)
  (cond
   ;; Special case for lists reduces stack consumption.
   ((list? e) (map simplify e))
   ((pair? e) (cons (simplify (car e))
                    (simplify (cdr e))))
   ((ly:stream-event? e)
    (list 'unquote (list 'make-stream-event (simplify (ly:stream-event::dump e)))))
   ((ly:music? e)
    (list 'unquote (music->make-music e)))
   ((ly:moment? e)
    (list 'unquote `(ly:make-moment
                     ,(ly:moment-main-numerator e)
                     ,(ly:moment-main-denominator e)
                     . ,(if (zero? (ly:moment-grace-numerator e))
                            '()
                            (list (ly:moment-grace-numerator e)
                                  (ly:moment-grace-denominator e))))))
   ((ly:duration? e)
    (list 'unquote `(ly:make-duration
                     ,(ly:duration-log e)
                     ,(ly:duration-dot-count e)
                     ,(ly:duration-scale e))))
   ((ly:pitch? e)
    (list 'unquote `(ly:make-pitch
                     ,(ly:pitch-octave e)
                     ,(ly:pitch-notename e)
                     ,(ly:pitch-alteration e))))
   ((ly:input-location? e)
    (list 'unquote '(ly:dummy-input-location)))
   (#t e)))

(define-public (ly:simplify-scheme e)
  (list 'quasiquote (simplify e)))
