;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2006--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

(define-module (lily paper-system))

(use-modules (lily)
             (srfi srfi-1)
             (ice-9 optargs))

(define-public (paper-system-title? system)
  (equal? #t (ly:prob-property system 'is-title)
          ))

(define (system-stencil system-grob main-stencil)
  (let* ((padding (ly:grob-property system-grob 'in-note-padding #f))
         (in-notes (if padding (ly:grob-property system-grob 'in-note-stencil) empty-stencil))
         (in-notes (if in-notes in-notes empty-stencil))
         (direction (if padding (ly:grob-property system-grob 'in-note-direction) UP)))
    (if padding
        (ly:stencil-combine-at-edge main-stencil Y direction in-notes padding)
        main-stencil)))

(define-public (paper-system-stencil system)
  "Add stencils for notes to the main stencil, returning the result."
  (let ((main-stencil (ly:prob-property system 'stencil))
        (system-grob (ly:prob-property system 'system-grob)))
    (if (ly:grob? system-grob)
        (system-stencil system-grob main-stencil)
        main-stencil)))

(define-public (paper-system-layout system)
  (let*
      ((g (paper-system-system-grob system)))

    (if (ly:grob? g)
        (ly:grob-layout  g)
        #f)))

(define-public (paper-system-system-grob paper-system)
  (ly:prob-property paper-system 'system-grob))

(define-public (paper-system-extent system axis)
  (ly:stencil-extent (paper-system-stencil system) axis))

(define-public (paper-system-staff-extents ps)
  (ly:prob-property ps 'staff-refpoint-extent '(0 . 0)))

(define-public (paper-system-annotate-last system layout)
  (let*
      ((bottomspace (ly:prob-property system 'bottom-space))
       (y-extent (paper-system-extent system Y))
       (x-extent (paper-system-extent system X))
       (stencil (ly:prob-property system 'stencil))

       (arrow (if (number? bottomspace)
                  (annotate-y-interval layout
                                       "bottom-space"
                                       (cons (- (car y-extent) bottomspace)
                                             (car y-extent))
                                       #t)
                  #f)))

    (if arrow
        (set! stencil
              (ly:stencil-add stencil arrow)))

    (set! (ly:prob-property system 'stencil)
          stencil)
    ))


(define*-public (annotate-padding system-Y system-X Y-ext X-ext
                                  next-system-Y next-system-X next-Y-ext next-X-ext
                                  layout horizon-padding padding #:key (base-color blue))
  "Y-ext and next-Y-ext are either skyline-pairs or extents."
  (let* ((eps 0.001)
         (skyline (and (ly:skyline-pair? Y-ext)
                       (car Y-ext)))
         (next-skyline (and (ly:skyline-pair? next-Y-ext)
                            (cdr next-Y-ext)))
         (annotation-X (cond
                        ((and skyline next-skyline)
                         (-
                          (ly:skyline-touching-point skyline next-skyline horizon-padding)
                          horizon-padding))
                        (skyline
                         (ly:skyline-max-height-position skyline))
                        (next-skyline
                         (ly:skyline-max-height-position next-skyline))
                        (else
                         (max (cdr X-ext)
                              (cdr next-X-ext)))))
         (annotation-Y (if skyline
                           (ly:skyline-height skyline annotation-X)
                           (car Y-ext)))
         (next-annotation-Y (if next-skyline
                                (- (+ (ly:skyline-height next-skyline
                                                         (- (+ annotation-X system-X)
                                                            next-system-X))
                                      next-system-Y)
                                   system-Y)
                                (cdr next-Y-ext)))
         (padding-blocks (>= next-annotation-Y (- annotation-Y padding eps)))
         (contrast-color (append (cdr base-color) (list (car base-color))))
         (color (if padding-blocks contrast-color base-color))
         (annotation (ly:stencil-translate-axis
                      (annotate-y-interval
                       layout
                       "padding"
                       `(,(- annotation-Y padding). ,annotation-Y)
                       #t
                       #:color color)
                      annotation-X X)))
    (if (> padding 0.0)
        annotation
        empty-stencil)))


(define-public (paper-system-annotate system next-system layout)
  "Add arrows and texts to indicate which lengths are set."

  (let* ((grob (ly:prob-property system 'system-grob))
         (paper-height (ly:output-def-lookup layout 'paper-height))
         (bottom-margin (ly:output-def-lookup layout 'bottom-margin))
         (top-margin (ly:output-def-lookup layout 'top-margin))
         (spaceable-staves (if (ly:grob? grob) (ly:system::get-spaceable-staves grob) '()))
         (all-staves (if (ly:grob? grob) (ly:system::get-staves grob) '()))
         (spaceable-staff-annotate
          (lambda (before-staff after-staff)
            (let ((before-Y (ly:grob-relative-coordinate before-staff grob Y))
                  (after-Y (ly:grob-relative-coordinate after-staff grob Y)))
              (annotate-spacing-spec
               layout
               ;; FIXME: Improve `ly:get-spacing-spec' to return the
               ;; name of the used `XXX-XXX-spacing' property, if
               ;; possible.  Right now we have to use the empty
               ;; string.
               ""
               (ly:get-spacing-spec before-staff after-staff)
               before-Y
               after-Y))))

         (staff-padding-annotate
          (lambda (before-staff after-staff)
            (let ((before-Y (ly:grob-relative-coordinate before-staff grob Y))
                  (before-X (ly:grob-relative-coordinate before-staff grob X))
                  (before-X-ext (ly:grob-extent before-staff before-staff X))
                  (after-Y (ly:grob-relative-coordinate after-staff grob Y))
                  (after-X (ly:grob-relative-coordinate after-staff grob X))
                  (after-X-ext (ly:grob-extent after-staff after-staff X))
                  (skylines (ly:grob-property before-staff 'vertical-skylines))
                  (after-skylines (ly:grob-property after-staff 'vertical-skylines))
                  (padding (assoc-get 'padding
                                      (ly:get-spacing-spec before-staff after-staff)
                                      0.0))
                  (horizon-padding (ly:grob-property before-staff
                                                     'skyline-horizontal-padding
                                                     0.0)))
              (ly:stencil-translate
               (annotate-padding
                before-Y before-X skylines before-X-ext
                after-Y after-X after-skylines after-X-ext
                layout horizon-padding padding)
               (cons before-X before-Y)))))

         (staff-annotations (if (< 1 (length spaceable-staves))
                                (map spaceable-staff-annotate
                                     (drop-right spaceable-staves 1)
                                     (drop spaceable-staves 1))
                                '()))
         (staff-padding-annotations (if (< 1 (length all-staves))
                                        (map staff-padding-annotate
                                             (drop-right all-staves 1)
                                             (drop all-staves 1))
                                        '()))
         (estimate-extent (if (ly:grob? grob)
                              (annotate-y-interval layout
                                                   "extent-estimate"
                                                   (ly:grob-property grob 'pure-Y-extent)
                                                   #f)
                              #f))

         (spacing-spec-sym (cond ((and next-system
                                       (paper-system-title? system)
                                       (paper-system-title? next-system))
                                  'markup-markup-spacing)
                                 ((paper-system-title? system)
                                  'markup-system-spacing)
                                 ((and next-system
                                       (paper-system-title? next-system))
                                  'score-markup-spacing)
                                 ((not next-system)
                                  'last-bottom-spacing)
                                 ((ly:prob-property system 'last-in-score #f)
                                  'score-system-spacing)
                                 (else
                                  'system-system-spacing)))
         (spacing-spec (ly:output-def-lookup layout spacing-spec-sym))
         (last-staff-Y (car (paper-system-staff-extents system)))
         (system-Y (ly:prob-property system 'Y-offset 0.0))
         (system-X (ly:prob-property system 'X-offset 0.0))
         (next-system-Y (and next-system
                             (ly:prob-property next-system 'Y-offset 0.0)))
         (next-system-X (and next-system
                             (ly:prob-property next-system 'X-offset 0.0)))
         (first-staff-next-system-Y (if next-system
                                        (- (+ (cdr (paper-system-staff-extents next-system))
                                              system-Y)
                                           next-system-Y)
                                        (+ system-Y top-margin bottom-margin (- paper-height))))

         (skyline (or
                   (ly:prob-property system 'vertical-skylines #f)
                   (paper-system-extent system Y)))
         (next-skyline (and next-system
                            (or
                             (ly:prob-property next-system 'vertical-skylines #f)
                             (paper-system-extent next-system Y))))
         (horizon-padding (and
                           (ly:grob? grob)
                           (ly:grob-property grob 'skyline-horizontal-padding 0)))
         (padding-annotation (if (skyline-pair-and-non-empty? next-system)
                                 (annotate-padding
                                  (- system-Y) system-X skyline (paper-system-extent system X)
                                  (- next-system-Y) next-system-X next-skyline (paper-system-extent next-system X)
                                  layout
                                  horizon-padding
                                  (assoc-get 'padding spacing-spec 0.0)
                                  #:base-color blue)
                                 empty-stencil))

         (system-annotation (annotate-spacing-spec
                             layout
                             (symbol->string spacing-spec-sym)
                             spacing-spec
                             last-staff-Y
                             first-staff-next-system-Y))
         (annotations (ly:stencil-add
                       padding-annotation
                       (stack-stencils Y DOWN 0.0 staff-padding-annotations)
                       (stack-stencils Y DOWN 0.0 (append staff-annotations (list system-annotation))))))

    (if estimate-extent
        (set! annotations
              (stack-stencils X RIGHT 5.5
                              (list annotations
                                    estimate-extent))))

    (if (not (null? annotations))
        (set! (ly:prob-property system 'stencil)
              (ly:stencil-add
               (ly:prob-property system 'stencil)
               (ly:make-stencil
                (ly:stencil-expr annotations)
                (ly:stencil-extent empty-stencil X)
                (ly:stencil-extent empty-stencil Y)))))
    (ly:prob-property system 'stencil)))
