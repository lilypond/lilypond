;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2005--2022 Erik Sandberg <mandolaerik@gmail.com>
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
    (music-event . (ad-hoc-jump-event
                    annotate-output-event
                    footnote-event arpeggio-event
                    bar-event
                    breathing-event
                    caesura-event
                    coda-mark-event
                    dal-segno-event
                    extender-event span-event rhythmic-event dynamic-event
                    break-event label-event percent-event key-change-event
                    string-number-event stroke-finger-event tie-event
                    part-combine-event ottava-event
                    beam-forbid-event script-event tempo-change-event
                    tremolo-event bend-after-event fingering-event
                    glissando-event harmonic-event hyphen-event
                    laissez-vibrer-event mark-event text-mark-event
                    multi-measure-text-event
                    multi-measure-articulation-event
                    note-grouping-event pes-or-flexa-event repeat-tie-event
                    section-event section-label-event
                    segno-mark-event
                    spacing-section-event layout-instruction-event
                    time-signature-event
                    completize-extender-event break-span-event alternative-event
                    volta-repeat-end-event
                    volta-repeat-start-event
                    vowel-transition-event
                    duration-line-event fine-event))

    (layout-instruction-event . (apply-output-event))
    (mark-event . (ad-hoc-mark-event rehearsal-mark-event))
    (script-event . (articulation-event text-script-event))
    (part-combine-event . (solo-one-event solo-two-event unisono-event))
    (break-event . (line-break-event page-break-event page-turn-event))
    (dynamic-event . (absolute-dynamic-event))
    (span-event . (bend-span-event
                   beam-event episema-event finger-glide-event
                   staff-highlight-event ligature-event
                   measure-spanner-event
                   measure-counter-event pedal-event
                   phrasing-slur-event slur-event
                   span-dynamic-event
                   staff-span-event text-span-event
                   trill-span-event tremolo-span-event
                   tuplet-span-event volta-span-event))
    (span-dynamic-event . (decrescendo-event crescendo-event))
    (break-span-event . (break-dynamic-span-event))
    (pedal-event . (sostenuto-event sustain-event una-corda-event))
    (rhythmic-event . (lyric-event
                       melodic-event general-rest-event
                       double-percent-event percent-event
                       repeat-slash-event
                       skip-event bass-figure-event))
    (general-rest-event . (rest-event multi-measure-rest-event))
    (melodic-event . (cluster-note-event note-event))
    (() . (Announcement))
    (Announcement . (AnnounceNewContext))
    ))

(define-public (event-class-cons class parent classlist)
  (let ((lineage (assq parent classlist)))
    (if (not lineage)
        (begin
          (if (not (null? parent))
              (ly:warning (G_ "unknown parent class `~a'") parent))
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
      (ly:error (G_ "Cannot redefine event class `~S'") class))
     ((not parentclass)
      (ly:error (G_ "Undefined parent event class `~S'") parentclass))
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
