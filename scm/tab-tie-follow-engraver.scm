;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2017--2026 Thomas Morley <thomasmorley65@gmail.com>
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

(define Tab_tie_follow_engraver
  (lambda (context)
    (let ((ties '())
          (repeat-ties '())
          (tab-note-heads '())
          (left-bound-tab-nhds '())
          (spanners '()))

      (make-engraver
       (acknowledgers
        ((semi-tie-interface this-engraver grob source-engraver)
         (when (positive? (ly:grob-property grob 'head-direction))
           (set! repeat-ties (cons grob repeat-ties))))
        ((spanner-interface this-engraver grob source-engraver)
         (when (grob::has-interface grob 'tie-interface)
           (set! ties (cons grob ties)))
         (when (or (grob::has-interface grob 'slur-interface)
                   (grob::has-interface grob 'glissando-interface))
           (set! spanners (cons grob spanners))))
        ((tab-note-head-interface this-engraver grob source-engraver)
         ;; Only sort TabNoteHead grobs if `tabFullNotation` is not used.
         ;; If `tabFullNotation` is used unset 'after-line-breaking, in order
         ;; to avoid printing transparent and parenthesized.
         (if (ly:context-property context 'tabFullNotation #f)
             (ly:grob-set-property! grob 'after-line-breaking '())
             (begin
               ;; In-chord repeat ties and those at stand-alone note heads are
               ;; in tab-note-head's 'articulations, thus affect tab-note-head
               ;; here, setting the `repeat-tied` sub-property.
               ;; Otherwise tab-note-head may be part of an event-chord with
               ;; \repeatTie in 'elements, thus store and accumulate them and
               ;; check for RepeatTie later
               (let* ((cause (ly:grob-property grob 'cause))
                      (repeat-tie?
                       (event-has-articulation? 'repeat-tie-event cause)))
                 (when repeat-tie?
                   (ly:grob-set-nested-property!
                    grob '(details tied-properties repeat-tied) #t)))
               (set! tab-note-heads (cons grob tab-note-heads))
               ;; accumulate tab-note-heads where a Tie ends
               ;; in `left-bound-tab-nhds`
               (when (pair? ties)
                 (for-each
                  (lambda (tie)
                    (when (equal? grob (ly:spanner-bound tie RIGHT))
                      (set! left-bound-tab-nhds
                            (cons grob left-bound-tab-nhds))))
                  ties))))))

       ((stop-translation-timestep this-engraver)
       ;;;; RepeatTies, attached to an event-chord or stand-alone TabNoteHead.
        ;;
        ;; TODO Stand-alone TabNoteHead are already done in `acknowledgers`,
        ;; should we filter for them?
        ;;
        ;; Equal lengths means, \repeatTie was applied to an
        ;; event-chord or a stand-alone note-head. Set the `repeat-tied`
        ;; sub-property.
        (when (and (pair? repeat-ties) (pair? tab-note-heads)
                   (= (length repeat-ties) (length tab-note-heads)))
          (for-each
           (lambda (tnhd)
             (ly:grob-set-nested-property!
              tnhd '(details tied-properties repeat-tied) #t))
           tab-note-heads))

       ;;;; Ties
        ;;
        ;; Set the `tied` subproperty for TabNoteHeads ending a Tie.
        (when (pair? left-bound-tab-nhds)
          (for-each
           (lambda (grob)
             (ly:grob-set-nested-property! grob
                                           '(details tied-properties tied) #t))
           left-bound-tab-nhds))
        ;; `ties` can not be cleared at this stage, because a Tie may be broken,
        ;; and we are interested in the *final* right bound TabNoteHead.

       ;;;; Slur and Glissando
        ;;
        ;; Set the `span-start` property for TabNoteHeads starting a Slur or
        ;; Glissando
        ;; NB Slur is bound by note-column, Glissando by note-head
        (when (pair? spanners)
          (for-each
           (lambda (sp)
             (let* ((left-bound (ly:spanner-bound sp LEFT))
                    (left-bounds-array
                     (ly:grob-object left-bound 'note-heads #f)))
               (for-each
                (lambda (g)
                  (when (memv g left-bound-tab-nhds)
                    (ly:grob-set-property! g 'span-start #t)))
                (if left-bounds-array
                    (ly:grob-array->list left-bounds-array)
                    (list left-bound)))))
           spanners))

        (set! left-bound-tab-nhds '())
        (set! spanners '())
        (set! tab-note-heads '())
        (set! repeat-ties '()))

       ((finalize trans)
        (set! ties '()))))))

(ly:register-translator
 Tab_tie_follow_engraver 'Tab_tie_follow_engraver
 '((grobs-created . ())
   (events-accepted . ())
   (properties-read . (tabFullNotation))
   (properties-written . ())
   (description . "Adjust @code{TabNoteHead} properties when the
@code{TabNoteHead} holds a @code{RepeatTie}, when a @code{Tie} ends and when a
@code{Slur} or @code{Glissando} starts at a tied @code{TabNoteHead}.")))
