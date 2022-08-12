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

(define-module (lily page))

(use-modules (lily)
             (lily paper-system)
             (srfi srfi-1))


(define (annotate? layout)
  (eq? #t (ly:output-def-lookup layout 'annotate-spacing)))

(define-public (make-page paper-book page-number is-bookpart-last-page)
  "A page is a prob (Property object). This is because the non-music
layout elements (title, header, footer) are markups and must be interpreted in context
of layout settings just like markups inside the music."
  ;; FIXME: the page breaker sometimes passes page-number as 0.  Is
  ;; that expected?
  (let* ((paper (ly:paper-book-paper paper-book))
         (page (ly:make-prob
                'page
                (layout->page-init paper)
                'paper-book paper-book
                'page-number page-number
                'is-last-bookpart (ly:output-def-lookup paper 'is-last-bookpart)
                'is-bookpart-last-page is-bookpart-last-page))
         (header-proc (ly:output-def-lookup paper 'make-header))
         (head-stencil (if (procedure? header-proc)
                           (header-proc page)
                           empty-stencil))
         (footer-proc (ly:output-def-lookup paper 'make-footer))
         (foot-stencil (if (procedure? footer-proc)
                           (footer-proc page)
                           empty-stencil)))
    (page-set-property! page 'head-stencil head-stencil)
    (page-set-property! page 'foot-stencil foot-stencil)
    page))

(define-public page-property ly:prob-property)
(define-public page-set-property! ly:prob-set-property!)

(define (page-translate-systems page)
  (for-each

   (lambda (sys off)
     (if (not (number? (ly:prob-property sys 'Y-offset)))
         (ly:prob-set-property! sys 'Y-offset off)))

   (page-property page 'lines)
   (page-property page 'configuration)))

(define (annotate-top-space first-system layout header-stencil stencil)
  "Add an annotation at the top to STENCIL and return new stencil."
  (let* ((top-margin (ly:output-def-lookup layout 'top-margin))
         (sym (if (paper-system-title? first-system)
                  'top-markup-spacing
                  'top-system-spacing))
         (spacing-spec (ly:output-def-lookup layout sym))
         (X-offset (ly:prob-property first-system 'X-offset 5))
         (header-extent (ly:stencil-extent header-stencil Y)))

    (set! stencil
          (ly:stencil-add stencil
                          (ly:stencil-translate-axis
                           (annotate-spacing-spec layout
                                                  (symbol->string sym)
                                                  spacing-spec
                                                  (- top-margin)
                                                  (car header-extent)
                                                  #:base-color red)
                           X-offset X)))
    stencil))


(define (annotate-page layout stencil)
  "add annotations to a stencil, and return result"
  (let ((top-margin (ly:output-def-lookup layout 'top-margin))
        (paper-height (ly:output-def-lookup layout 'paper-height))
        (bottom-margin (ly:output-def-lookup layout 'bottom-margin))
        (add-stencil (lambda (y)
                       (set! stencil
                             (ly:stencil-add stencil
                                             (ly:stencil-translate-axis y 6 X))))))
    (add-stencil
     (ly:stencil-translate-axis
      (annotate-y-interval layout "paper-height"
                           (cons (- paper-height) 0)
                           #t)
      1 X))
    (add-stencil
     (ly:stencil-translate-axis
      (annotate-y-interval layout "top-margin"
                           (cons (- top-margin) 0)
                           #t)
      2 X))
    (add-stencil
     (ly:stencil-translate-axis
      (annotate-y-interval layout "bottom-margin"
                           (cons (- paper-height) (- bottom-margin paper-height))
                           #t)
      2 X))
    stencil))

(define (annotate-space-left page)
  (let*
      ((paper-book (page-property page 'paper-book))
       (layout (ly:paper-book-paper paper-book))
       (arrow (annotate-y-interval layout
                                   "space left"
                                   (cons (- 0.0
                                            (page-property page 'bottom-edge)
                                            (let ((foot (page-property page 'foot-stencil)))
                                              (if (and (ly:stencil? foot)
                                                       (not (ly:stencil-empty? foot)))
                                                  (car (ly:stencil-extent foot Y))
                                                  0.0)))
                                         (page-property page  'bottom-system-edge))
                                   #t)))

    (set! arrow (ly:stencil-translate-axis arrow 8 X))

    arrow))

(define-public (layout->page-init layout)
  "Alist of settings for page layout"
  (let*
      ((paper-height (ly:output-def-lookup layout 'paper-height))
       (paper-width (ly:output-def-lookup layout 'paper-width))
       (left-margin (ly:output-def-lookup layout 'left-margin))
       (right-margin (ly:output-def-lookup layout 'right-margin))
       (bottom-edge (- paper-height
                       (ly:output-def-lookup layout 'bottom-margin)) )
       (top-margin (ly:output-def-lookup layout 'top-margin))
       )

    `((paper-height . ,paper-height)
      (paper-width . ,paper-width)
      (left-margin . ,left-margin)
      (right-margin . ,right-margin)
      (top-margin . ,top-margin)
      (bottom-edge . ,bottom-edge)
      )))

(define (make-page-stencil page)
  "Construct a stencil representing the page from PAGE."

  (page-translate-systems page)
  (let*
      ((paper-book (page-property page 'paper-book))
       (prop (lambda (sym) (page-property page sym)))
       (layout (ly:paper-book-paper paper-book))
       (lines (page-property page 'lines))
       (number (page-property page 'page-number))

       ;; TODO: naming paper-height/paper-width not analogous to TeX.

       (system-xoffset (ly:output-def-lookup layout 'horizontal-shift 0.0))
       (system-separator-markup (ly:output-def-lookup layout 'system-separator-markup))
       (system-separator-stencil (if (markup? system-separator-markup)
                                     (interpret-markup layout
                                                       (layout-extract-page-properties layout)
                                                       system-separator-markup)
                                     #f))

       (page-stencil empty-stencil)

       (last-system #f)
       (last-y 0.0)
       (add-to-page (lambda (stencil x y)
                      (set! page-stencil
                            (ly:stencil-add page-stencil
                                            (ly:stencil-translate stencil
                                                                  (cons
                                                                   (+ system-xoffset x)
                                                                   (- 0 y (prop 'top-margin)))

                                                                  )))))
       (add-system
        (lambda (system)
          (let* ((stencil (paper-system-stencil system))
                 (extra-offset (ly:prob-property system 'extra-offset '(0 . 0)))
                 (x (+ (ly:prob-property system 'X-offset 0.0)
                       (car extra-offset)))
                 (y (+ (ly:prob-property system 'Y-offset 0.0)
                       (cdr extra-offset)))
                 (is-title (paper-system-title?
                            system)))
            (add-to-page stencil x y)
            (if (and (ly:stencil? system-separator-stencil)
                     last-system
                     (not (paper-system-title? system))
                     (not (paper-system-title? last-system)))
                (add-to-page
                 system-separator-stencil
                 0
                 (average (- last-y
                             (car (paper-system-staff-extents last-system)))
                          (- y
                             (cdr (paper-system-staff-extents system))))))
            (set! last-system system)
            (set! last-y y))))
       (head (prop 'head-stencil))
       (foot (prop 'foot-stencil))
       )

    (if (and
         (ly:stencil? head)
         (not (ly:stencil-empty? head)))
        (begin
          ;; Ensure that the top of the header just touches the top margin.
          (set! head (ly:stencil-translate-axis head
                                                (- 0 (cdr (ly:stencil-extent head Y)) (prop 'top-margin)) Y))
          (set! page-stencil (ly:stencil-add page-stencil head))))

    (if (and
         (annotate? layout)
         (pair? lines))

        (begin
          (set! page-stencil (annotate-top-space (car lines) layout head page-stencil))

          (for-each (lambda (sys next-sys)
                      (paper-system-annotate sys next-sys layout))
                    lines
                    (append (cdr lines) (list #f)))
          (paper-system-annotate-last (car (last-pair lines)) layout)))

    (for-each add-system lines)


    (ly:prob-set-property! page 'bottom-system-edge
                           (car (ly:stencil-extent page-stencil Y)))
    (ly:prob-set-property! page 'space-left
                           (+ (prop 'bottom-edge)
                              (prop 'bottom-system-edge)
                              (if (and (ly:stencil? foot)
                                       (not (ly:stencil-empty? foot)))
                                  (car (ly:stencil-extent foot Y))
                                  0.0)))

    (if (annotate? layout)
        (set! page-stencil
              (ly:stencil-add page-stencil
                              (annotate-space-left page))))

    (if (and (ly:stencil? foot)
             (not (ly:stencil-empty? foot)))
        (set! page-stencil
              (ly:stencil-add
               page-stencil
               (ly:stencil-translate
                foot
                (cons 0
                      (+ (- (prop 'bottom-edge))
                         (- (car (ly:stencil-extent foot Y)))))))))

    (if (ly:output-def-lookup layout 'two-sided #f)
        (set! page-stencil
              (ly:stencil-translate page-stencil
                                    (cons (prop (if (even? number)
                                                    'left-margin
                                                    'right-margin))
                                          0)))
        (set! page-stencil
              (ly:stencil-translate page-stencil (cons (prop 'left-margin) 0))))

    ;; annotation.
    (if (annotate? layout)
        (set! page-stencil (annotate-page layout page-stencil)))

    (if (ly:get-option 'use-paper-size-for-page)
        (set! page-stencil (ly:make-stencil
                            (ly:stencil-expr page-stencil)
                            (cons 0 (ly:output-def-lookup layout 'paper-width))
                            (cons (- (ly:output-def-lookup layout 'paper-height)) 0))
              ))
    page-stencil))

(define-public (page-stencil page)
  (if (not (ly:stencil? (page-property page 'stencil)))

      ;; todo: make tweakable.
      ;; via property + callbacks.

      (page-set-property! page 'stencil (make-page-stencil page)))
  (page-property page 'stencil))

(define-public (calc-printable-height page)
  "Printable area for music and titles."
  (let*
      ((paper-book (page-property page 'paper-book))
       (layout (ly:paper-book-paper paper-book))
       (h (- (ly:output-def-lookup layout 'paper-height)
             (ly:output-def-lookup layout 'top-margin)
             (ly:output-def-lookup layout 'bottom-margin)))

       (head (page-property page 'head-stencil))
       (foot (page-property page 'foot-stencil))
       (available
        (- h (if (ly:stencil? head)
                 (interval-length (ly:stencil-extent head Y))
                 0)
           (if (ly:stencil? foot)
               (interval-length (ly:stencil-extent foot Y))
               0))))
    available))
