;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2010--2022 Mike Solomon <mikesol@stanfordalumni.org>
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

;; Constants

(define CENTRAL-COLUMN-HOLE-PLACEMENTS '((one . (0.0 . 6.5))
                                         (two . (0.0 . 5.5))
                                         (three . (0.0 . 4.5))
                                         (four . (0.0 . 3.0))
                                         (five . (0.0 . 2.0))
                                         (six . (0.0 . 1.0))))

(define CENTRAL-COLUMN-HOLE-LIST (map car CENTRAL-COLUMN-HOLE-PLACEMENTS))
(define CENTRAL-COLUMN-HOLE-H-LIST (cons 'h CENTRAL-COLUMN-HOLE-LIST))

;; Utility functions

(use-modules (ice-9 optargs))

(define (return-1 x) 1.0)

(define (make-spreadsheet parameter-list)
  "Makes a spreadsheet function with columns of parameter-list.
   This function can then be filled with rows.
   For example:
   @code{guile> ((make-spreadsheet '(foo bar)) '((1 2) (3 4) (5 6)))}
   @code{(((foo . 1) (bar . 2)) ((foo . 3) (bar . 4)) ((foo . 5) (bar . 6)))}"
  (lambda (ls)
    (map (lambda (list-to-translate)
           (map (lambda (name element)
                  `(,name . ,element))
                parameter-list
                list-to-translate))
         ls)))

(define (get-spreadsheet-column column spreadsheet)
  "Gets all the values in @code{column} form @code{spreadsheet}
   made by @code{make-spreadsheet}.
   For example:
   @code{guile> (get-spreadsheet-column 'bar ((make-spreadsheet '(foo bar)) '((1 2) (3 4) (5 6))))}
   @code{(2 4 6)}"
  (map (lambda (row) (assoc-get column row)) spreadsheet))

(define (make-named-spreadsheet parameter-list)
  "Makes a named spreadsheet function with columns of parameter-list.
   This function can then be filled with named rows
   For example:
   @code{guile> ((make-named-spreadsheet '(foo bar)) '((x . (1 2)) (y . (3 4)) (z . (5 6))))}
   @code{((x (foo . 1) (bar . 2)) (y (foo . 3) (bar . 4)) (z (foo . 5) (bar . 6)))}"
  (lambda (ls)
    (map (lambda (list-to-translate)
           `(,(list-ref list-to-translate 0)
             . ,(map (lambda (name element)
                       `(,name . ,element))
                     parameter-list
                     (list-tail list-to-translate 1))))
         ls)))

(define (get-named-spreadsheet-column column spreadsheet)
  "Gets all the values in @code{column} form @code{spreadsheet}
   made by @code{make-named-spreadsheet}.
   For example:
   @code{guile> (get-spreadsheet-column 'bar ((make-named-spreadsheet '(foo bar)) '((x . (1 2)) (y . (3 4)) (z . (5 6)))))}
   @code{((x . 2) (y . 4) (z . 6))}"
  (map
   (lambda (row) (cons (car row) (assoc-get column (cdr row))))
   spreadsheet))

(define make-key-alist
  (make-named-spreadsheet '(name offset graphical textual)))

(define (simple-stencil-alist stencil offset)
  "A stencil alist that contains one and only one stencil.
   Shorthand used repeatedly in various instruments."
  `((stencils . (,stencil))
    (offset . ,offset)
    (textual?  . #f)
    (xy-scale-function . (,return-1 . ,return-1))))

(define (make-central-column-hole-addresses keys)
  "Takes @code{keys} and ascribes them to the central column."
  (map
   (lambda (key) `(central-column . ,key))
   keys))

(define (make-key-symbols hand)
  "Takes @code{hand} and ascribes @code{key} to it."
  (lambda (keys)
    (map (lambda (key) `(,hand . ,key))
         keys)))

(define make-left-hand-key-addresses (make-key-symbols 'left-hand))

(define make-right-hand-key-addresses (make-key-symbols 'right-hand))

;; Flute assembly instructions

(define flute-change-points
  ((make-named-spreadsheet '(piccolo flute flute-b-extension))
   `((bottom-group-key-names
      . (((x
           . ((offset . (-0.45 . -1.05))
              (stencil . ,piccolo-rh-x-key-stencil)
              (text? . ("X" . #f))
              (complexity . trill))))
         ((cis
           . ((offset . (0.0 . 0.0))
              (stencil . ,flute-rh-cis-key-stencil)
              (text? . ("C" . 1))
              (complexity . trill)))
          (c
           . ((offset . (0.3 . 0.0))
              (stencil . ,flute-rh-c-key-stencil)
              (text? . ("C" . #f))
              (complexity . trill)))
          (gz
           . ((offset . (0.0 . -1.2))
              (stencil . ,flute-rh-gz-key-stencil)
              (text? . ("gz" . #f))
              (complexity . trill))))
         ((cis
           . ((offset . (0.0 . 0.0))
              (stencil . ,flute-rh-cis-key-stencil)
              (text? . ("C" . 1))
              (complexity . trill)))
          (c
           . ((offset . (0.3 . 0.0))
              (stencil . ,flute-rh-c-key-stencil)
              (text? . ("C" . #f))
              (complexity . trill)))
          (b
           . ((offset . (1.15 . 0.0))
              (stencil . ,flute-rh-b-key-stencil)
              (text? . ("B" . #f))
              (complexity . trill)))
          (gz
           . ((offset . (0.0 . -1.2))
              (stencil . ,flute-rh-gz-key-stencil)
              (text? . ("gz" . #f))
              (complexity . trill))))))
     (bottom-group-graphical-stencil
      . (((right-hand . ees) (right-hand . x))
         ,(make-right-hand-key-addresses '(ees cis c gz))
         ,(make-right-hand-key-addresses '(ees cis c b gz))))
     (bottom-group-graphical-draw-instruction
      . (((right-hand . ees))
         ,(make-right-hand-key-addresses '(ees cis c))
         ,(make-right-hand-key-addresses '(ees cis c b))))
     (bottom-group-special-key-instruction
      . ((,rich-group-draw-rule ((right-hand . x)) ((right-hand . ees)))
         (,rich-group-draw-rule ((right-hand . gz))
                                ,(make-right-hand-key-addresses
                                  '(ees cis c)))
         (,rich-group-draw-rule ((right-hand . gz))
                                ,(make-right-hand-key-addresses
                                  '(ees cis c b)))))
     (bottom-group-text-stencil
      . (,(make-right-hand-key-addresses '(bes d dis ees x))
         ,(make-right-hand-key-addresses '(bes d dis ees cis c gz))
         ,(make-right-hand-key-addresses '(bes d dis ees cis c b gz)))))))

(define (generate-flute-family-entry flute-name)
  (let*
      ((change-points
        (get-named-spreadsheet-column
         flute-name
         flute-change-points)))
    `(,flute-name
      . ((keys
          . ((hidden
              . ((midline
                  . ((offset . (0.0 . 0.0))
                     (stencil . ,midline-stencil)
                     (text? . #f)
                     (complexity . basic)))))
             (central-column
              . ((one
                  . ((offset . ,(assoc-get 'one CENTRAL-COLUMN-HOLE-PLACEMENTS))
                     (stencil . ,ring-column-circle-stencil)
                     (text? . #f)
                     (complexity . ring)))
                 (two
                  . ((offset . ,(assoc-get 'two CENTRAL-COLUMN-HOLE-PLACEMENTS))
                     (stencil . ,ring-column-circle-stencil)
                     (text? . #f)
                     (complexity . ring)))
                 (three
                  . ((offset . ,(assoc-get 'three CENTRAL-COLUMN-HOLE-PLACEMENTS))
                     (stencil . ,ring-column-circle-stencil)
                     (text? . #f)
                     (complexity . ring)))
                 (four
                  . ((offset . ,(assoc-get 'four CENTRAL-COLUMN-HOLE-PLACEMENTS))
                     (stencil . ,ring-column-circle-stencil)
                     (text? . #f)
                     (complexity . ring)))
                 (five
                  . ((offset . ,(assoc-get 'five CENTRAL-COLUMN-HOLE-PLACEMENTS))
                     (stencil . ,ring-column-circle-stencil)
                     (text? . #f)
                     (complexity . ring)))
                 (six
                  . ((offset . ,(assoc-get 'six CENTRAL-COLUMN-HOLE-PLACEMENTS))
                     (stencil . ,ring-column-circle-stencil)
                     (text? . #f)
                     (complexity . ring)))))
             (left-hand
              . ((bes
                  . ((offset . (0.5 . 1.8))
                     (stencil . ,flute-lh-bes-key-stencil)
                     (text? . ("B" . 0))
                     (complexity . trill)))
                 (b
                  . ((offset . (0.0 . 0.0))
                     (stencil . ,flute-lh-b-key-stencil)
                     (text? . ("B" . #f))
                     (complexity . trill)))
                 (gis
                  . ((offset . (0.0 . 0.0))
                     (stencil . ,flute-lh-gis-key-stencil)
                     (text? . ("G" . 1))
                     (complexity . trill)))))
             (right-hand
              . ,(append `((bes
                            . ((offset . (0.0 . 0.0))
                               (stencil . ,flute-rh-bes-key-stencil)
                               (text? . ("B" . 0))
                               (complexity . trill)))
                           (d
                            . ((offset . (0.0 . 0.0))
                               (stencil . ,flute-rh-d-key-stencil)
                               (text? . ("D" . #f))
                               (complexity . trill)))
                           (dis
                            . ((offset . (0.0 . 0.0))
                               (stencil . ,flute-rh-dis-key-stencil)
                               (text? . ("D" . 1))
                               (complexity . trill)))
                           (ees
                            . ((offset . (1.5 . 1.3))
                               (stencil . ,flute-rh-ees-key-stencil)
                               (text? . ("E" . 0))
                               (complexity . trill))))
                         (assoc-get 'bottom-group-key-names change-points)))))
         (graphical-commands
          . ((stencil-alist
              . ((stencils
                  . (,(simple-stencil-alist '(hidden . midline) '(0.0 . 3.75))
                     ((stencils
                       . ,(make-central-column-hole-addresses
                           CENTRAL-COLUMN-HOLE-LIST))
                      (xy-scale-function . (,identity . ,identity))
                      (textual? . #f)
                      (offset . (0.0 . 0.0)))
                     ((stencils . ((left-hand . bes) (left-hand . b)))
                      (xy-scale-function . (,return-1 . ,return-1))
                      (textual? . #f)
                      (offset . (-1.5 . 6.5)))
                     ,(simple-stencil-alist '(left-hand . gis) '(1.0 . 4.0))
                     ,(simple-stencil-alist '(right-hand . bes)  '(-1.75 . 3.05))
                     ,(simple-stencil-alist '(right-hand . d)  '(-1.0 . 2.5))
                     ,(simple-stencil-alist '(right-hand . dis)  '(-1.0 . 1.5))
                     ((stencils
                       . ,(assoc-get 'bottom-group-graphical-stencil
                                     change-points))
                      (xy-scale-function . (,return-1 . ,return-1))
                      (textual? . #f)
                      (offset . (0.0 . -0.6)))))
                 (xy-scale-function . (,identity . ,identity))
                 (textual? . #f)
                 (offset . (0.0 . 0.0))))
             (draw-instructions
              . ((,apply-group-draw-rule-series
                  (((left-hand . bes) (left-hand . b))
                   ,(assoc-get 'bottom-group-graphical-draw-instruction
                               change-points)))
                 ,(assoc-get 'bottom-group-special-key-instruction
                             change-points)
                 (,group-automate-rule
                  ,(make-central-column-hole-addresses CENTRAL-COLUMN-HOLE-LIST))
                 (,group-automate-rule ((hidden . midline)))))
             (extra-offset-instructions
              . ((,uniform-extra-offset-rule (0.0 . 0.0))))))
         (text-commands
          . ((stencil-alist
              . ((stencils
                  . (,(simple-stencil-alist '(hidden . midline) '(0.0 . 3.75))
                     ((stencils
                       . ,(make-central-column-hole-addresses
                           CENTRAL-COLUMN-HOLE-LIST))
                      (xy-scale-function . (,identity . ,identity))
                      (textual? . #f)
                      (offset . (0.0 . 0.0)))
                     ((stencils . ,(make-left-hand-key-addresses '(bes b gis)))
                      (textual? . ,lh-woodwind-text-stencil)
                      (offset . (1.5 . 3.75)))
                     ((stencils . ,(assoc-get 'bottom-group-text-stencil
                                              change-points))
                      (textual? . ,rh-woodwind-text-stencil)
                      (offset . (-1.25 . 0.0)))))
                 (xy-scale-function . (,identity . ,identity))
                 (textual? . #f)
                 (offset . (0.0 . 0.0))))
             (draw-instructions
              . ((,apply-group-draw-rule-series
                  (,(make-left-hand-key-addresses '(bes b gis))
                   ,(assoc-get 'bottom-group-text-stencil change-points)))
                 (,group-automate-rule
                  ,(make-central-column-hole-addresses CENTRAL-COLUMN-HOLE-LIST))
                 (,group-automate-rule ((hidden . midline)))))
             (extra-offset-instructions
              . ((,uniform-extra-offset-rule (0.0 . 0.0))))))))))

;;; Tin whistle assembly instructions

(define tin-whistle-change-points
  ((make-named-spreadsheet '(tin-whistle)) '()))

(define (generate-tin-whistle-family-entry tin-whistle-name)
  (let*
      ((change-points
        (get-named-spreadsheet-column tin-whistle-name tin-whistle-change-points)))
    `(,tin-whistle-name
      . ((keys
          . ((hidden
              . ((midline
                  . ((offset . (0.0 . 0.0))
                     (stencil . ,midline-stencil)
                     (text? . #f)
                     (complexity . basic)))))
             (central-column
              . ((one
                  . ((offset . ,(assoc-get 'one CENTRAL-COLUMN-HOLE-PLACEMENTS))
                     (stencil . ,column-circle-stencil)
                     (text? . #f)
                     (complexity . covered)))
                 (two
                  . ((offset . ,(assoc-get 'two CENTRAL-COLUMN-HOLE-PLACEMENTS))
                     (stencil . ,column-circle-stencil)
                     (text? . #f)
                     (complexity . covered)))
                 (three
                  . ((offset . ,(assoc-get 'three CENTRAL-COLUMN-HOLE-PLACEMENTS))
                     (stencil . ,column-circle-stencil)
                     (text? . #f)
                     (complexity . covered)))
                 (four
                  . ((offset . ,(assoc-get 'four CENTRAL-COLUMN-HOLE-PLACEMENTS))
                     (stencil . ,column-circle-stencil)
                     (text? . #f)
                     (complexity . covered)))
                 (five
                  . ((offset . ,(assoc-get 'five CENTRAL-COLUMN-HOLE-PLACEMENTS))
                     (stencil . ,column-circle-stencil)
                     (text? . #f)
                     (complexity . covered)))
                 (six
                  . ((offset . ,(assoc-get 'six CENTRAL-COLUMN-HOLE-PLACEMENTS))
                     (stencil . ,column-circle-stencil)
                     (text? . #f)
                     (complexity . covered)))))
             (left-hand . ())
             (right-hand . ())))
         (graphical-commands
          . ((stencil-alist
              . ((stencils
                  . (,(simple-stencil-alist '(hidden . midline) '(0.0 . 3.75))
                     ((stencils
                       . ,(make-central-column-hole-addresses
                           CENTRAL-COLUMN-HOLE-LIST))
                      (xy-scale-function . (,identity . ,identity))
                      (textual? . #f)
                      (offset . (0.0 . 0.0)))))
                 (xy-scale-function . (,identity . ,identity))
                 (textual? . #f)
                 (offset . (0.0 . 0.0))))
             (draw-instructions
              . ((,group-automate-rule
                  ,(make-central-column-hole-addresses CENTRAL-COLUMN-HOLE-LIST))
                 (,group-automate-rule ((hidden . midline)))))
             (extra-offset-instructions
              . ((,uniform-extra-offset-rule (0.0 . 0.0))))))
         (text-commands
          . ((stencil-alist
              . ((stencils .
                           (,(simple-stencil-alist '(hidden . midline) '(0.0 . 3.75))
                            ((stencils
                              . ,(make-central-column-hole-addresses
                                  CENTRAL-COLUMN-HOLE-LIST))
                             (xy-scale-function . (,identity . ,identity))
                             (textual? . #f)
                             (offset . (0.0 . 0.0)))))
                 (xy-scale-function . (,identity . ,identity))
                 (textual? . #f)
                 (offset . (0.0 . 0.0))))
             (draw-instructions
              . ((,group-automate-rule
                  ,(make-central-column-hole-addresses CENTRAL-COLUMN-HOLE-LIST))
                 (,group-automate-rule ((hidden . midline)))))
             (extra-offset-instructions
              . ((,uniform-extra-offset-rule (0.0 . 0.0))))))))))

;;; Oboe assembly instructions

(define oboe-change-points
  ((make-named-spreadsheet '(oboe)) '()))

(define (generate-oboe-family-entry oboe-name)
  (let*
      ((change-points
        (get-named-spreadsheet-column oboe-name oboe-change-points)))
    `(,oboe-name
      . ((keys
          . ((hidden
              . ((midline
                  . ((offset . (0.0 . 0.0))
                     (stencil . ,midline-stencil)
                     (text? . #f)
                     (complexity . basic)))))
             (central-column
              . ((one
                  . ((offset . ,(assoc-get 'one CENTRAL-COLUMN-HOLE-PLACEMENTS))
                     (stencil . ,ring-column-circle-stencil)
                     (text? . #f)
                     (complexity . ring)))
                 (two
                  . ((offset . ,(assoc-get 'two CENTRAL-COLUMN-HOLE-PLACEMENTS))
                     (stencil . ,ring-column-circle-stencil)
                     (text? . #f)
                     (complexity . ring)))
                 (three
                  . ((offset . ,(assoc-get 'three CENTRAL-COLUMN-HOLE-PLACEMENTS))
                     (stencil . ,ring-column-circle-stencil)
                     (text? . #f)
                     (complexity . ring)))
                 (four
                  . ((offset . ,(assoc-get 'four CENTRAL-COLUMN-HOLE-PLACEMENTS))
                     (stencil . ,ring-column-circle-stencil)
                     (text? . #f)
                     (complexity . ring)))
                 (five
                  . ((offset . ,(assoc-get 'five CENTRAL-COLUMN-HOLE-PLACEMENTS))
                     (stencil . ,ring-column-circle-stencil)
                     (text? . #f)
                     (complexity . ring)))
                 (six
                  . ((offset . ,(assoc-get 'six CENTRAL-COLUMN-HOLE-PLACEMENTS))
                     (stencil . ,ring-column-circle-stencil)
                     (text? . #f)
                     (complexity . ring)))
                 (h
                  . ((offset . (0.0 . 6.25))
                     (stencil . ,(variable-column-circle-stencil 0.4))
                     (text? . #f)
                     (complexity . trill)))))
             (left-hand
              . ((I
                  . ((offset . (0.0 . 0.0))
                     (stencil . ,oboe-lh-I-key-stencil)
                     (text? . ("I" . #f))
                     (complexity . trill)))
                 (III
                  . ((offset . (0.0 . 2.6))
                     (stencil . ,oboe-lh-III-key-stencil)
                     (text? . ("III" . #f))
                     (complexity . trill)))
                 (II
                  . ((offset . (0.0 . 0.0))
                     (stencil . ,oboe-lh-II-key-stencil)
                     (text? . ("II" . #f))
                     (complexity . trill)))
                 (b
                  . ((offset . (0.0 . 0.0))
                     (stencil . ,oboe-lh-b-key-stencil)
                     (text? . ("B" . #f))
                     (complexity . trill)))
                 (d
                  . ((offset . (0.0 . 0.0))
                     (stencil . ,oboe-lh-d-key-stencil)
                     (text? . ("D" . #f))
                     (complexity . trill)))
                 (cis
                  . ((offset . (0.0 . 0.0))
                     (stencil . ,oboe-lh-cis-key-stencil)
                     (text? . ("C" . 1))
                     (complexity . trill)))
                 (gis
                  . ((offset . (-0.85 . 0.2))
                     (stencil . ,oboe-lh-gis-key-stencil)
                     (text? . ("G" . 1))
                     (complexity . trill)))
                 (ees
                  . ((offset . (2.05 . -3.65))
                     (stencil . ,oboe-lh-ees-key-stencil)
                     (text? . ("E" . 0))
                     (complexity . trill)))
                 (low-b
                  . ((offset . (3.6 . 0.5))
                     (stencil . ,oboe-lh-low-b-key-stencil)
                     (text? . ("b" . #f))
                     (complexity . trill)))
                 (bes
                  . ((offset . (2.25 . -4.15))
                     (stencil . ,oboe-lh-bes-key-stencil)
                     (text? . ("B" . 0))
                     (complexity . trill)))
                 (f
                  . ((offset . (2.15 . -3.85))
                     (stencil . ,oboe-lh-f-key-stencil)
                     (text? . ("F" . #f))
                     (complexity . trill)))))
             (right-hand
              . ((a
                  . ((offset . (1.5 . 1.2))
                     (stencil . ,oboe-rh-a-key-stencil)
                     (text? . ("A" . #f))
                     (complexity . trill)))
                 (gis
                  . ((offset . (0.0 . 0.0))
                     (stencil . ,oboe-rh-gis-key-stencil)
                     (text? . ("G" . 1))
                     (complexity . trill)))
                 (d
                  . ((offset . (0.0 . 0.0))
                     (stencil . ,oboe-rh-d-key-stencil)
                     (text? . ("D" . #f))
                     (complexity . trill)))
                 (f
                  . ((offset . (0.0 . 0.0))
                     (stencil . ,oboe-rh-f-key-stencil)
                     (text? . ("F" . #f))
                     (complexity . trill)))
                 (banana
                  . ((offset . (0.0 . 0.0))
                     (stencil . ,oboe-rh-banana-key-stencil)
                     (text? . ("ban" . #f))
                     (complexity . trill)))
                 (c
                  . ((offset . (0.0 . 0.0))
                     (stencil . ,oboe-rh-c-key-stencil)
                     (text? . ("C" . #f))
                     (complexity . trill)))
                 (cis
                  . ((offset . (3.8 . -0.6))
                     (stencil . ,oboe-rh-cis-key-stencil)
                     (text? . ("C" . 1))
                     (complexity . trill)))
                 (ees
                  . ((offset . (0.0 . -1.8))
                     (stencil . ,oboe-rh-ees-key-stencil)
                     (text? . ("E" . 0))
                     (complexity . trill)))))))
         (graphical-commands
          . ((stencil-alist
              . ((stencils
                  . (,(simple-stencil-alist '(hidden . midline) '(0.0 . 3.75))
                     ((stencils
                       . ,(make-central-column-hole-addresses
                           CENTRAL-COLUMN-HOLE-H-LIST))
                      (xy-scale-function . (,identity . ,identity))
                      (textual? . #f)
                      (offset . (0.0 . 0.0)))
                     ((stencils . ((left-hand . I) (left-hand . III)))
                      (xy-scale-function . (,return-1 . ,return-1))
                      (textual? . #f)
                      (offset . (-2.5 . 6.5)))
                     ,(simple-stencil-alist '(left-hand . II) '(2.5 . 6.0))
                     ,(simple-stencil-alist '(left-hand . b) '(-1.35 . 6.0))
                     ,(simple-stencil-alist '(left-hand . d) '(1.0 . 6.0))
                     ,(simple-stencil-alist '(left-hand . cis) '(1.0 . 5.0))
                     ((stencils
                       . ,(make-left-hand-key-addresses '(gis bes low-b ees f)))
                      (xy-scale-function . (,return-1 . ,return-1))
                      (textual? . #f)
                      (offset . (0.0 . 3.9)))
                     ((stencils .
                                ,(make-right-hand-key-addresses '(a gis)))
                      (xy-scale-function . (,return-1 . ,return-1))
                      (textual? . #f)
                      (offset . (-3.5 . 3.5)))
                     ,(simple-stencil-alist '(right-hand . d) '(1.0 . 2.5))
                     ,(simple-stencil-alist '(right-hand . f)  '(-1.0 . 1.5))
                     ,(simple-stencil-alist '(right-hand . banana)  '(1.7 . 1.0))
                     ((stencils . ,(make-right-hand-key-addresses '(c cis ees)))
                      (xy-scale-function . (,return-1 . ,return-1))
                      (textual? . #f)
                      (offset . (-3.4 . 0.3)))))
                 (xy-scale-function . (,identity . ,identity))
                 (textual? . #f)
                 (offset . (0.0 . 0.0))))
             (draw-instructions
              . ((,apply-group-draw-rule-series
                  (((right-hand . a) (right-hand . gis))
                   ,(make-left-hand-key-addresses '(gis bes low-b ees))
                   ,(make-right-hand-key-addresses '(cis c ees))))
                 (,rich-group-draw-rule
                  ((left-hand . III))
                  ((left-hand . I)))
                 (,rich-group-draw-rule
                  ((left-hand . f))
                  ,(make-left-hand-key-addresses '(gis bes low-b ees)))
                 (,group-automate-rule
                  ,(make-central-column-hole-addresses CENTRAL-COLUMN-HOLE-LIST))
                 (,group-automate-rule ((hidden . midline)))))
             (extra-offset-instructions
              . ((,rich-group-extra-offset-rule
                  ((central-column . h)) ((central-column . one)) (0.0 . 0.8))
                 (,uniform-extra-offset-rule (0.0 . 0.0))))))
         (text-commands
          . ((stencil-alist
              . ((stencils .
                           (,(simple-stencil-alist '(hidden . midline) '(0.0 . 3.75))
                            ((stencils
                              . ,(make-central-column-hole-addresses
                                  CENTRAL-COLUMN-HOLE-H-LIST))
                             (xy-scale-function . (,identity . ,identity))
                             (textual? . #f)
                             (offset . (0.0 . 0.0)))
                            ((stencils . ,(make-left-hand-key-addresses '(III I)))
                             (textual? . ,lh-woodwind-text-stencil)
                             (offset . (-2.8 . 7.0)))
                            ((stencils . ,(make-left-hand-key-addresses '(II)))
                             (textual? . ,lh-woodwind-text-stencil)
                             (offset . (2.2 . 7.0)))
                            ((stencils
                              .  ,(make-left-hand-key-addresses
                                   '(b d cis gis ees low-b bes f)))
                             (textual? . ,lh-woodwind-text-stencil)
                             (offset . (1.5 . 3.75)))
                            ((stencils
                              . ,(make-right-hand-key-addresses
                                  '(a gis d f banana c cis ees)))
                             (textual? . ,rh-woodwind-text-stencil)
                             (offset . (-1.25 . 0.0)))))
                 (xy-scale-function . (,identity . ,identity))
                 (textual? . #f)
                 (offset . (0.0 . 0.0))))
             (draw-instructions
              . ((,apply-group-draw-rule-series
                  (,(make-left-hand-key-addresses '(b d cis gis ees low-b bes f))
                   ,(make-left-hand-key-addresses '(III I))
                   ,(make-right-hand-key-addresses '(a gis d f banana c cis ees))))
                 (,group-automate-rule
                  ,(make-central-column-hole-addresses CENTRAL-COLUMN-HOLE-LIST))
                 (,group-automate-rule ((hidden . midline)))))
             (extra-offset-instructions
              . ((,rich-group-extra-offset-rule
                  ((central-column . h))
                  ((central-column . one))
                  (0.0 . 0.8))
                 (,uniform-extra-offset-rule (0.0 . 0.0))))))))))

;; Clarinet assembly instructions

(define clarinet-change-points
  ((make-named-spreadsheet '(clarinet bass-clarinet low-bass-clarinet))
   `((bottom-group-key-names
      . (()
         ((ees
           . ((offset . (0.0 . ,(* 0 (+ 0.75 CL-RH-HAIR))))
              (stencil . ,bass-clarinet-rh-ees-key-stencil)
              (text? . ("E" . 0))
              (complexity . trill))))
         ((ees
           . ((offset . (0.0 . ,(* 0 (+ 0.75 CL-RH-HAIR))))
              (stencil . ,low-bass-clarinet-rh-ees-key-stencil)
              (text? . ("E" . 0))
              (complexity . trill)))
          (d
           . ((offset . (,(+ 1.5 CL-RH-HAIR) . ,(* -1 (+ 0.75 CL-RH-HAIR))))
              (stencil . ,clarinet-rh-d-key-stencil)
              (text? . ("d" . #f))
              (complexity . trill)))
          (low-cis
           . ((offset . (0.0 . 1.4))
              (stencil . ,clarinet-rh-low-cis-key-stencil)
              (text? . ("c" . 1))
              (complexity . trill)))
          (low-d
           . ((offset . (0.0 . 2.4))
              (stencil . ,clarinet-rh-low-d-key-stencil)
              (text? . ("d" . #f))
              (complexity . trill)))
          (low-c
           . ((offset . (0.0 . 0.0))
              (stencil . ,clarinet-rh-low-c-key-stencil)
              (text? . ("c" . #f))
              (complexity . trill))))))
     (left-extra-key-names
      . (()
         ()
         ((d
           . ((offset . (4.0 . -0.8))
              (stencil . ,clarinet-lh-d-key-stencil)
              (text? . ("D" . #f))
              (complexity . trill))))))
     (right-thumb-group
      . (()
         ()
         (((stencils
            . ,(make-right-hand-key-addresses '(low-c low-cis)))
           (xy-scale-function . (,return-1 . ,return-1))
           (textual? . #f)
           (offset . (-1.3 . 4.0))))))
     (low-left-hand-key-addresses
      . (,(make-left-hand-key-addresses '(cis f e fis))
         ,(make-left-hand-key-addresses '(cis f e fis))
         ,(make-left-hand-key-addresses '(cis f e fis d))))
     (all-left-hand-key-addresses
      . (,(make-left-hand-key-addresses '(a gis ees cis f e fis))
         ,(make-left-hand-key-addresses '(a gis ees cis f e fis))
         ,(make-left-hand-key-addresses '(a gis ees cis f e fis d))))
     (low-key-group
      . (()
         ()
         (,(make-right-hand-key-addresses '(low-c low-cis)))))
     (low-rich-draw-rules
      . (()
         ()
         ((,rich-group-draw-rule
           ((left-hand . d))
           ,(make-left-hand-key-addresses '(cis f e fis)))
          (,rich-group-draw-rule
           ((right-hand . low-d))
           ((right-hand . low-cis) (right-hand . low-c))))))
     (low-extra-offset-rule
      . (()
         ()
         ((,rich-group-extra-offset-rule
           ,(make-right-hand-key-addresses '(low-c low-d low-cis))
           ,(make-right-hand-key-addresses '(one two three four))
           (-0.5 . -0.7)))))
     (bottom-right-group-key-addresses
      . (,(make-right-hand-key-addresses '(fis e f gis))
         ,(make-right-hand-key-addresses '(fis e ees gis f))
         ,(make-right-hand-key-addresses '(fis e ees gis f d))))
     (right-hand-key-addresses
      . (,(make-right-hand-key-addresses '(fis e f gis))
         ,(make-right-hand-key-addresses '(fis e ees gis f))
         ,(make-right-hand-key-addresses
           '(low-d low-cis low-c fis e ees gis f d)))))))

(define (generate-clarinet-family-entry clarinet-name)
  (let*
      ((change-points
        (get-named-spreadsheet-column clarinet-name clarinet-change-points)))
    `(,clarinet-name
      . ((keys
          . ((hidden
              . ((midline
                  . ((offset . (0.0 . 0.0))
                     (stencil . ,midline-stencil)
                     (text? . #f)
                     (complexity . basic)))))
             (central-column
              . ((one
                  . ((offset . ,(assoc-get 'one CENTRAL-COLUMN-HOLE-PLACEMENTS))
                     (stencil . ,column-circle-stencil)
                     (text? . #f)
                     (complexity . covered)))
                 (two
                  . ((offset . ,(assoc-get 'two CENTRAL-COLUMN-HOLE-PLACEMENTS))
                     (stencil . ,column-circle-stencil)
                     (text? . #f)
                     (complexity . covered)))
                 (three
                  . ((offset . ,(assoc-get 'three CENTRAL-COLUMN-HOLE-PLACEMENTS))
                     (stencil . ,column-circle-stencil)
                     (text? . #f)
                     (complexity . covered)))
                 (four
                  . ((offset . ,(assoc-get 'four CENTRAL-COLUMN-HOLE-PLACEMENTS))
                     (stencil . ,column-circle-stencil)
                     (text? . #f)
                     (complexity . covered)))
                 (five
                  . ((offset . ,(assoc-get 'five CENTRAL-COLUMN-HOLE-PLACEMENTS))
                     (stencil . ,column-circle-stencil)
                     (text? . #f)
                     (complexity . covered)))
                 (six
                  . ((offset . ,(assoc-get 'six CENTRAL-COLUMN-HOLE-PLACEMENTS))
                     (stencil . ,column-circle-stencil)
                     (text? . #f)
                     (complexity . covered)))
                 (h
                  . ((offset . (0.0 . 6.25))
                     (stencil . ,(variable-column-circle-stencil 0.4))
                     (text? . #f)
                     (complexity . covered)))))
             (left-hand
              . ,(append `((thumb
                            . ((offset . (0.0 . 0.0))
                               (stencil . ,clarinet-lh-thumb-key-stencil)
                               (text? . #f)
                               (complexity . trill)))
                           (R
                            . ((offset . (1.0 . 1.0))
                               (stencil . ,clarinet-lh-R-key-stencil)
                               (text? . #f)
                               (complexity . trill)))
                           (a
                            . ((offset . (0.0 . 0.0))
                               (stencil . ,clarinet-lh-a-key-stencil)
                               (text? . ("A" . #f))
                               (complexity . trill)))
                           (gis
                            . ((offset . (0.8 . 1.0))
                               (stencil . ,clarinet-lh-gis-key-stencil)
                               (text? . ("G" . 1))
                               (complexity . trill)))
                           (ees
                            . ((offset . (0.0 . 0.0))
                               (stencil . ,clarinet-lh-ees-key-stencil)
                               (text? . ("E" . 0))
                               (complexity . trill)))
                           (cis
                            . ((offset . (-0.85 . 0.2))
                               (stencil . ,clarinet-lh-cis-key-stencil)
                               (text? . ("C" . 1))
                               (complexity . trill)))
                           (f
                            . ((offset . (3.6 . 0.5))
                               (stencil . ,clarinet-lh-f-key-stencil)
                               (text? . ("F" . #f))
                               (complexity . trill)))
                           (e
                            . ((offset . (2.05 . -3.65))
                               (stencil . ,clarinet-lh-e-key-stencil)
                               (text? . ("E" . #f))
                               (complexity . trill)))
                           (fis
                            . ((offset . (2.25 . -4.15))
                               (stencil . ,clarinet-lh-fis-key-stencil)
                               (text? . ("F" . 1))
                               (complexity . trill))))
                         (assoc-get 'left-extra-key-names change-points)))
             (right-hand
              . ,(append `((one
                            . ((offset . (0.0 . 0.75))
                               (stencil . ,clarinet-rh-one-key-stencil)
                               (text? . "1")
                               (complexity . trill)))
                           (two
                            . ((offset . (0.0 . 0.25))
                               (stencil . ,clarinet-rh-two-key-stencil)
                               (text? . "2")
                               (complexity . trill)))
                           (three
                            . ((offset . (0.0 . -0.25))
                               (stencil . ,clarinet-rh-three-key-stencil)
                               (text? . "3")
                               (complexity . trill)))
                           (four
                            . ((offset . (0.0 . -0.75))
                               (stencil . ,clarinet-rh-four-key-stencil)
                               (text? . "4")
                               (complexity . trill)))
                           (b
                            . ((offset . (0.0 . 0.0))
                               (stencil . ,clarinet-rh-b-key-stencil)
                               (text? . ("B" . #f))
                               (complexity . trill)))
                           (fis
                            . ((offset . (0.0 . ,(* 4 (+ 0.75 CL-RH-HAIR))))
                               (stencil . ,clarinet-rh-fis-key-stencil)
                               (text? . ("F" . 1))
                               (complexity . trill)))
                           (gis
                            . ((offset . (,(+ 1.5 CL-RH-HAIR)
                                          . ,(* 3 (+ 0.75 CL-RH-HAIR))))
                               (stencil . ,clarinet-rh-gis-key-stencil)
                               (text? . ("G" . 1))
                               (complexity . trill)))
                           (e
                            . ((offset . (0.0 . ,(* 2 (+ 0.75 CL-RH-HAIR))))
                               (stencil . ,clarinet-rh-e-key-stencil)
                               (text? . ("E" . #f))
                               (complexity . trill)))
                           (f
                            . ((offset . (,(+ 1.5 CL-RH-HAIR)
                                          . ,(* 1 (+ 0.75 CL-RH-HAIR))))
                               (stencil . ,clarinet-rh-f-key-stencil)
                               (text? . ("F" . #f))
                               (complexity . trill))))
                         (assoc-get 'bottom-group-key-names change-points)))))
         (graphical-commands
          . ((stencil-alist
              . ((stencils
                  . ,(append (assoc-get 'right-thumb-group change-points)
                             `(,(simple-stencil-alist '(hidden . midline)
                                                      '(0.0 . 3.75))
                               ((stencils
                                 . ,(make-central-column-hole-addresses
                                     CENTRAL-COLUMN-HOLE-H-LIST))
                                (xy-scale-function . (,identity . ,identity))
                                (textual? . #f)
                                (offset . (0.0 . 0.0)))
                               ((stencils
                                 . ,(make-left-hand-key-addresses '(thumb R)))
                                (xy-scale-function . (,identity . ,identity))
                                (textual? . #f)
                                (offset . (-2.5 . 6.5)))
                               ((stencils
                                 . ((left-hand . a) (left-hand . gis)))
                                (xy-scale-function . (,return-1 . ,return-1))
                                (textual? . #f)
                                (offset . (0.0 . 7.5)))
                               ,(simple-stencil-alist '(left-hand . ees)
                                                      '(1.0 . 5.0))
                               ((stencils
                                 . ,(make-left-hand-key-addresses '(cis f e fis)))
                                (xy-scale-function . (,return-1 . ,return-1))
                                (textual? . #f)
                                (offset . (0.0 . 3.9)))
                               ((stencils
                                 . ,(make-right-hand-key-addresses
                                     '(one two three four)))
                                (xy-scale-function . (,return-1 . ,return-1))
                                (textual? . #f)
                                (offset . (-1.25 . 3.75)))
                               ,(simple-stencil-alist '(right-hand . b)
                                                      '(-1.0 . 1.5))
                               ((stencils
                                 . ,(assoc-get 'bottom-right-group-key-addresses
                                               change-points))
                                (xy-scale-function . (,return-1 . ,return-1))
                                (textual? . #f)
                                (offset . (-4.0 . -0.75))))))
                 (xy-scale-function . (,identity . ,identity))
                 (textual? . #f)
                 (offset . (0.0 . 0.0))))
             (draw-instructions
              . ,(append (assoc-get 'low-rich-draw-rules change-points)
                         `((,apply-group-draw-rule-series
                            ,(append (assoc-get 'low-key-group change-points)
                                     `(((left-hand . a) (left-hand . gis))
                                       ,(make-right-hand-key-addresses
                                         '(one two three four))
                                       ,(assoc-get 'low-left-hand-key-addresses
                                                   change-points)
                                       ,(assoc-get 'right-hand-key-addresses
                                                   change-points))))
                           (,rich-group-draw-rule
                            ((left-hand . R))
                            ((left-hand . thumb)))
                           (,group-automate-rule
                            ,(make-central-column-hole-addresses
                              CENTRAL-COLUMN-HOLE-LIST))
                           (,group-automate-rule ((hidden . midline))))))
             (extra-offset-instructions
              . ,(append (assoc-get 'low-extra-offset-rule change-points)
                         `((,rich-group-extra-offset-rule
                            ((central-column . h))
                            ((central-column . one)
                             (left-hand . a)
                             (left-hand . gis))
                            (0.0 . 0.8))
                           (,uniform-extra-offset-rule (0.0 . 0.0)))))))
         (text-commands
          . ((stencil-alist
              . ((stencils
                  . (,(simple-stencil-alist '(hidden . midline) '(0.0 . 3.75))
                     ((stencils
                       . ,(make-central-column-hole-addresses
                           CENTRAL-COLUMN-HOLE-LIST))
                      (xy-scale-function . (,identity . ,identity))
                      (textual? . #f)
                      (offset . (0.0 . 0.0)))
                     ((stencils . ((left-hand . thumb) (left-hand . R)))
                      (xy-scale-function . (,identity . ,identity))
                      (textual? . #f)
                      (offset . (-2.5 . 6.5)))
                     ((stencils
                       . ,(assoc-get 'all-left-hand-key-addresses change-points))
                      (textual? . ,lh-woodwind-text-stencil)
                      (offset . (1.5 . 3.75)))
                     ((stencils
                       . ,(make-right-hand-key-addresses '(one two three four)))
                      (textual? . ,number-column-stencil)
                      (offset . (-1.25 . 3.75)))
                     ((stencils . ,(assoc-get 'right-hand-key-addresses
                                              change-points))
                      (textual? . ,rh-woodwind-text-stencil)
                      (offset . (-1.25 . 0.0)))))
                 (xy-scale-function . (,identity . ,identity))
                 (textual? . #f)
                 (offset . (0.0 . 0.0))))
             (draw-instructions
              . ((,apply-group-draw-rule-series
                  (,(assoc-get 'all-left-hand-key-addresses change-points)
                   ,(make-right-hand-key-addresses '(one two three four))
                   ,(assoc-get 'right-hand-key-addresses change-points)))
                 (,group-automate-rule
                  ,(make-central-column-hole-addresses
                    CENTRAL-COLUMN-HOLE-LIST))
                 (,group-automate-rule ((hidden . midline)))))
             (extra-offset-instructions
              . ((,rich-group-extra-offset-rule
                  ((central-column . h))
                  ((central-column . one) (left-hand . a) (left-hand . gis))
                  (0.0 . 0.8))
                 (,uniform-extra-offset-rule (0.0 . 0.0))))))))))

;; Saxophone assembly instructions

(define (saxophone-name-passerelle name)
  (cond ((eqv? name 'saxophone) 'saxophone)
        ((eqv? name 'soprano-saxophone) 'saxophone)
        ((eqv? name 'alto-saxophone) 'saxophone)
        ((eqv? name 'tenor-saxophone) 'saxophone)
        ((eqv? name 'baritone-saxophone) 'baritone-saxophone)))

(define saxophone-change-points
  ((make-named-spreadsheet '(saxophone baritone-saxophone))
   `((low-a-key-definition
      . (()
         ((low-a
           . ((offset . (0.0 . 0.0))
              (stencil . ,saxophone-lh-low-a-key-stencil)
              (text? . #f)
              (complexity . trill))))))
     (low-a-key-group
      . (()
         (,(simple-stencil-alist '(left-hand . low-a) '(-5.0 . 7.0)))))
     (low-a-presence
      . (()
         ((left-hand . low-a)))))))

(define (generate-saxophone-family-entry saxophone-name)
  (let*
      ((change-points
        (get-named-spreadsheet-column
         (saxophone-name-passerelle saxophone-name) saxophone-change-points)))
    `(,saxophone-name
      . ((keys
          . ((hidden
              . ((midline
                  . ((offset . (0.0 . 0.0))
                     (stencil . ,midline-stencil)
                     (text? . #f)
                     (complexity . basic)))))
             (central-column
              . ((one
                  . ((offset . ,(assoc-get 'one CENTRAL-COLUMN-HOLE-PLACEMENTS))
                     (stencil . ,column-circle-stencil)
                     (text? . #f)
                     (complexity . trill)))
                 (two
                  . ((offset . ,(assoc-get 'two CENTRAL-COLUMN-HOLE-PLACEMENTS))
                     (stencil . ,column-circle-stencil)
                     (text? . #f)
                     (complexity . trill)))
                 (three
                  . ((offset . ,(assoc-get 'three CENTRAL-COLUMN-HOLE-PLACEMENTS))
                     (stencil . ,column-circle-stencil)
                     (text? . #f)
                     (complexity . trill)))
                 (four
                  . ((offset . ,(assoc-get 'four CENTRAL-COLUMN-HOLE-PLACEMENTS))
                     (stencil . ,column-circle-stencil)
                     (text? . #f)
                     (complexity . trill)))
                 (five
                  . ((offset . ,(assoc-get 'five CENTRAL-COLUMN-HOLE-PLACEMENTS))
                     (stencil . ,column-circle-stencil)
                     (text? . #f)
                     (complexity . trill)))
                 (six
                  . ((offset . ,(assoc-get 'six CENTRAL-COLUMN-HOLE-PLACEMENTS))
                     (stencil . ,column-circle-stencil)
                     (text? . #f)
                     (complexity . trill)))))
             (left-hand
              . ,(append (assoc-get 'low-a-key-definition change-points)
                         `((T
                            . ((offset . (0.0 . 0.0))
                               (stencil . ,saxophone-lh-T-key-stencil)
                               (text? . ("T" . #f))
                               (complexity . trill)))
                           (ees
                            . ((offset . (0.4 . 1.6))
                               (stencil . ,saxophone-lh-ees-key-stencil)
                               (text? . ("E" . 0))
                               (complexity . trill)))
                           (d
                            . ((offset . (1.5 . 0.5))
                               (stencil . ,saxophone-lh-d-key-stencil)
                               (text? . ("D" . #f))
                               (complexity . trill)))
                           (f
                            . ((offset . (0.0 . 0.0))
                               (stencil . ,saxophone-lh-f-key-stencil)
                               (text? . ("F" . #f))
                               (complexity . trill)))
                           (front-f
                            . ((offset . (0.0 . 0.0))
                               (stencil . ,saxophone-lh-front-f-key-stencil)
                               (text? . ("f" . #f))
                               (complexity . trill)))
                           (bes
                            . ((offset . (0.0 . 0.0))
                               (stencil . ,saxophone-lh-bes-key-stencil)
                               (text? . ("B" . 0))
                               (complexity . trill)))
                           (gis
                            . ((offset . (0.0 . 1.1))
                               (stencil . ,saxophone-lh-gis-key-stencil)
                               (text? . ("G" . 1))
                               (complexity . trill)))
                           (cis
                            . ((offset . (2.4 . 0.0))
                               (stencil . ,saxophone-lh-cis-key-stencil)
                               (text? . ("C" . 1))
                               (complexity . trill)))
                           (b
                            . ((offset . (0.0 . 0.0))
                               (stencil . ,saxophone-lh-b-key-stencil)
                               (text? . ("B" . #f))
                               (complexity . trill)))
                           (low-bes
                            . ((offset . (0.0 . -0.2))
                               (stencil . ,saxophone-lh-low-bes-key-stencil)
                               (text? . ("b" . 0))
                               (complexity . trill))))))
             (right-hand
              . ((e
                  . ((offset . (0.0 . 2.0))
                     (stencil . ,saxophone-rh-e-key-stencil)
                     (text? . ("E" . #f))
                     (complexity . trill)))
                 (c
                  . ((offset . (0.0 . 0.9))
                     (stencil . ,saxophone-rh-c-key-stencil)
                     (text? . ("C" . #f))
                     (complexity . trill)))
                 (bes
                  . ((offset . (0.0 . 0.0))
                     (stencil . ,saxophone-rh-bes-key-stencil)
                     (text? . ("B" . 0))
                     (complexity . trill)))
                 (high-fis
                  . ((offset . (0.0 . 0.0))
                     (stencil . ,saxophone-rh-high-fis-key-stencil)
                     (text? . ("hF" . 1))
                     (complexity . trill)))
                 (fis
                  . ((offset . (0.0 . 0.0))
                     (stencil . ,saxophone-rh-fis-key-stencil)
                     (text? . ("F" . 1))
                     (complexity . trill)))
                 (ees
                  . ((offset . (0.0 . 0.7))
                     (stencil . ,saxophone-rh-ees-key-stencil)
                     (text? . ("E" . 0))
                     (complexity . trill)))
                 (low-c
                  . ((offset . (-1.2 . -0.1))
                     (stencil . ,saxophone-rh-low-c-key-stencil)
                     (text? . ("c" . #f))
                     (complexity . trill)))))))
         (graphical-commands
          . ((stencil-alist
              . ((stencils
                  . ,(append (assoc-get 'low-a-key-group change-points)
                             `(,(simple-stencil-alist '(hidden . midline)
                                                      '(0.0 . 3.75))
                               ((stencils
                                 . ,(make-central-column-hole-addresses
                                     CENTRAL-COLUMN-HOLE-LIST))
                                (xy-scale-function . (,identity . ,identity))
                                (textual? . #f)
                                (offset . (0.0 . 0.0)))
                               ((stencils
                                 . ,(make-left-hand-key-addresses '(ees d f)))
                                (xy-scale-function . (,return-1 . ,return-1))
                                (textual? . #f)
                                (offset . (1.5 . 6.8)))
                               ,(simple-stencil-alist '(left-hand . front-f)
                                                      '(0.0 . 7.35))
                               ,(simple-stencil-alist '(left-hand . T)
                                                      '(-2.2 . 6.5))
                               ,(simple-stencil-alist '(left-hand . bes)
                                                      '(0.0 . 6.2))
                               ((stencils
                                 . ,(make-left-hand-key-addresses
                                     '(gis cis b low-bes)))
                                (xy-scale-function . (,return-1 . ,return-1))
                                (textual? . #f)
                                (offset . (1.2 . 3.5)))
                               ((stencils
                                 . ,(make-right-hand-key-addresses '(e c bes)))
                                (xy-scale-function . (,return-1 . ,return-1))
                                (textual? . #f)
                                (offset . (-2.3 . 3.4)))
                               ,(simple-stencil-alist '(right-hand . high-fis)
                                                      '(-1.8 . 2.5))
                               ,(simple-stencil-alist '(right-hand . fis)
                                                      '(-1.5 . 1.5))
                               ((stencils
                                 . ,(make-right-hand-key-addresses '(ees low-c)))
                                (xy-scale-function . (,return-1 . ,return-1))
                                (textual? . #f)
                                (offset . (-2.0 . 0.3))))))
                 (xy-scale-function . (,identity . ,identity))
                 (textual? . #f)
                 (offset . (0.0 . 0.0))))
             (draw-instructions
              . ((,apply-group-draw-rule-series
                  (,(make-left-hand-key-addresses '(ees d f))
                   ,(make-left-hand-key-addresses '(gis cis b low-bes))
                   ,(make-right-hand-key-addresses '(e c bes))
                   ,(make-right-hand-key-addresses '(ees low-c))))
                 (,group-automate-rule
                  ,(make-central-column-hole-addresses CENTRAL-COLUMN-HOLE-LIST))
                 (,group-automate-rule ((hidden . midline)))))
             (extra-offset-instructions
              . ((,rich-group-extra-offset-rule
                  ((left-hand . bes))
                  ,(append (assoc-get 'low-a-presence change-points)
                           '((central-column . one)
                             (left-hand . front-f)
                             (left-hand . T)
                             (left-hand . ees)
                             (left-hand . d)
                             (left-hand . f)))
                  (0.0 . 1.0))
                 (,uniform-extra-offset-rule (0.0 . 0.0))))))
         (text-commands
          . ((stencil-alist
              . ((stencils
                  . ,(append (assoc-get 'low-a-key-group change-points)
                             `(,(simple-stencil-alist '(hidden . midline)
                                                      '(0.0 . 3.75))
                               ((stencils
                                 . ,(make-central-column-hole-addresses
                                     CENTRAL-COLUMN-HOLE-LIST))
                                (xy-scale-function . (,identity . ,identity))
                                (textual? . #f)
                                (offset . (0.0 . 0.0)))
                               ,(simple-stencil-alist '(left-hand . T)
                                                      '(-1.0 . 7.0))
                               ((stencils
                                 . ,(make-left-hand-key-addresses
                                     '(ees d f front-f bes gis cis b low-bes)))
                                (textual? . ,lh-woodwind-text-stencil)
                                (offset . (1.5 . 3.75)))
                               ((stencils
                                 . ,(make-right-hand-key-addresses
                                     '(e c bes high-fis fis ees low-c)))
                                (textual? . ,rh-woodwind-text-stencil)
                                (offset . (-1.25 . 0.0))))))
                 (xy-scale-function . (,identity . ,identity))
                 (textual? . #f)
                 (offset . (0.0 . 0.0))))
             (draw-instructions
              . ((,apply-group-draw-rule-series
                  (,(make-left-hand-key-addresses
                     '(ees d f front-f bes gis cis b low-bes))
                   ,(make-right-hand-key-addresses
                     '(e c bes high-fis fis ees low-c))))
                 (,group-automate-rule
                  ,(make-central-column-hole-addresses
                    CENTRAL-COLUMN-HOLE-LIST))
                 (,group-automate-rule ((hidden . midline)))))
             (extra-offset-instructions
              . ((,uniform-extra-offset-rule (0.0 . 0.0))))))))))

;; Bassoon assembly instructions

(define bassoon-change-points
  ((make-named-spreadsheet '(bassoon contrabassoon))
   `((left-hand-additional-keys .
                                (((a .
                                     ((offset . (0.0 . -0.3))
                                      (stencil . ,bassoon-lh-a-flick-key-stencil)
                                      (text? . ("A" . #f))
                                      (complexity . trill)))
                                  (w .
                                     ((offset . (0.0 . 0.0))
                                      (stencil . ,bassoon-lh-whisper-key-stencil)
                                      (text? . ("w" . #f))
                                      (complexity . trill))))
                                 ()))
     (right-hand-additional-keys .
                                 (((cis .
                                        ((offset . (0.0 . 0.0))
                                         (stencil . ,bassoon-rh-cis-key-stencil)
                                         (text? . ("C" . 1))
                                         (complexity . trill)))
                                   (thumb-gis .
                                              ((offset . (0.0 . 0.0))
                                               (stencil . ,bassoon-rh-thumb-gis-key-stencil)
                                               (text? . ("G" . 1))
                                               (complexity . trill))))
                                  ()))
     (left-hand-flick-group .
                            (((left-hand . d) (left-hand . c) (left-hand . a))
                             ((left-hand . d) (left-hand . c))))
     (left-hand-thumb-group .
                            (((left-hand . w) (left-hand . thumb-cis))
                             ((left-hand . thumb-cis))))
     (cis-offset-instruction .
                             (((,rich-group-extra-offset-rule
                                ((right-hand . cis))
                                ,(append
                                  '((hidden . midline) (hidden . long-midline))
                                  (make-central-column-hole-addresses '(three two one))
                                  (make-left-hand-key-addresses
                                   '(low-b low-bes low-c low-d d a c w thumb-cis
                                           lh-ees high-ees high-e cis ees)))
                                (0.0 . 0.9)))
                              ()))
     (right-hand-lower-thumb-group .
                                   (((right-hand . thumb-gis) (right-hand . thumb-fis))
                                    ((right-hand . thumb-fis))))
     (right-hand-cis-key .
                         ((,(simple-stencil-alist '(right-hand . cis) '(-2.3 . 3.22)))
                          ()))
     (back-left-hand-key-addresses .
                                   ((low-b low-bes low-c low-d d a c w thumb-cis)
                                    (low-b low-bes low-c low-d d c thumb-cis)))
     (front-right-hand-key-addresses .
                                     ((cis bes fis f gis) (bes fis f gis)))
     (back-right-hand-key-addresses .
                                    ((thumb-bes thumb-gis thumb-e thumb-fis)
                                     (thumb-bes thumb-e thumb-fis))))))

(define (generate-bassoon-family-entry bassoon-name)
  (let*
      ((change-points
        (get-named-spreadsheet-column bassoon-name bassoon-change-points)))
    `(,bassoon-name
      . ((keys
          . ((hidden
              . ((midline
                  .  ((offset . (0.0 . 0.0))
                      (stencil . ,midline-stencil)
                      (text? . #f)
                      (complexity . basic)))
                 (long-midline
                  . ((offset . (0.0 . 0.0))
                     (stencil . ,long-midline-stencil)
                     (text? . #f)
                     (complexity . basic)))))
             (central-column
              . ((one
                  . ((offset . ,(assoc-get 'one CENTRAL-COLUMN-HOLE-PLACEMENTS))
                     (stencil . ,ring-column-circle-stencil)
                     (text? . #f)
                     (complexity . trill)))
                 (two
                  . ((offset . ,(assoc-get 'two CENTRAL-COLUMN-HOLE-PLACEMENTS))
                     (stencil . ,ring-column-circle-stencil)
                     (text? . #f)
                     (complexity . ring)))
                 (three
                  . ((offset . ,(assoc-get 'three CENTRAL-COLUMN-HOLE-PLACEMENTS))
                     (stencil . ,ring-column-circle-stencil)
                     (text? . #f)
                     (complexity . ring)))
                 (four
                  . ((offset . ,(assoc-get 'four CENTRAL-COLUMN-HOLE-PLACEMENTS))
                     (stencil . ,ring-column-circle-stencil)
                     (text? . #f)
                     (complexity . ring)))
                 (five
                  . ((offset . ,(assoc-get 'five CENTRAL-COLUMN-HOLE-PLACEMENTS))
                     (stencil . ,ring-column-circle-stencil)
                     (text? . #f)
                     (complexity . ring)))
                 (six
                  . ((offset . ,(assoc-get 'six CENTRAL-COLUMN-HOLE-PLACEMENTS))
                     (stencil . ,bassoon-cc-six-key-stencil)
                     (text? . #f)
                     (complexity . ring)))))
             (left-hand
              . ,(append (assoc-get 'left-hand-additional-keys
                                    change-points)
                         `((high-e
                            . ((offset . (0.0 . 0.0))
                               (stencil . ,bassoon-lh-he-key-stencil)
                               (text? . ("hE" . #f))
                               (complexity . trill)))
                           (high-ees
                            . ((offset . (0.0 . 0.0))
                               (stencil . ,bassoon-lh-hees-key-stencil)
                               (text? . ("hE" . 0))
                               (complexity . trill)))
                           (lh-ees
                            . ((offset . (0.0 . 0.0))
                               (stencil . ,bassoon-lh-lhees-key-stencil)
                               (text? . ("lE" . 0))
                               (complexity . trill)))
                           (ees
                            . ((offset . (-1.0 . 1.0))
                               (stencil . ,bassoon-lh-ees-key-stencil)
                               (text? . ("E" . 0))
                               (complexity . trill)))
                           (cis
                            . ((offset . (0.0 . 0.0))
                               (stencil . ,bassoon-lh-cis-key-stencil)
                               (text? . ("C" . 1))
                               (complexity . trill)))
                           (low-bes
                            . ((offset . (0.0 . 0.0))
                               (stencil . ,bassoon-lh-lbes-key-stencil)
                               (text? . ("b" . 0))
                               (complexity . trill)))
                           (low-b
                            . ((offset . (-1.0 . -0.7))
                               (stencil . ,bassoon-lh-lb-key-stencil)
                               (text? . ("b" . #f))
                               (complexity . trill)))
                           (low-c
                            . ((offset . (0.0 . 0.0))
                               (stencil . ,bassoon-lh-lc-key-stencil)
                               (text? . ("c" . #f))
                               (complexity . trill)))
                           (low-d
                            . ((offset . (0.0 . 0.0))
                               (stencil . ,bassoon-lh-ld-key-stencil)
                               (text? . ("d" . #f))
                               (complexity . trill)))
                           (d
                            . ((offset . (-1.5 . 2.0))
                               (stencil . ,bassoon-lh-d-flick-key-stencil)
                               (text? . ("D" . #f))
                               (complexity . trill)))
                           (c
                            . ((offset . (-0.8 . 1.1))
                               (stencil . ,bassoon-lh-c-flick-key-stencil)
                               (text? . ("C" . #f))
                               (complexity . trill)))
                           (thumb-cis
                            . ((offset . (2.0 . -1.0))
                               (stencil . ,bassoon-lh-thumb-cis-key-stencil)
                               (text? . ("C" . 1))
                               (complexity . trill))))))
             (right-hand
              . ,(append (assoc-get 'right-hand-additional-keys
                                    change-points)
                         `((bes
                            . ((offset . (0.0 . 0.7))
                               (stencil . ,bassoon-rh-bes-key-stencil)
                               (text? . ("B" . 0))
                               (complexity . trill)))
                           (f
                            . ((offset . (-2.2 . 4.35))
                               (stencil . ,bassoon-rh-f-key-stencil)
                               (text? . ("F" . #f))
                               (complexity . trill)))
                           (fis
                            . ((offset . (1.5 . 1.0))
                               (stencil . ,bassoon-rh-fis-key-stencil)
                               (text? . ("F" . 1))
                               (complexity . trill)))
                           (gis
                            . ((offset . (0.0 . -0.15))
                               (stencil . ,bassoon-rh-gis-key-stencil)
                               (text? . ("G" . 1))
                               (complexity . trill)))
                           (thumb-bes
                            . ((offset . (0.0 . 0.0))
                               (stencil . ,bassoon-rh-thumb-bes-key-stencil)
                               (text? . ("B" . 0))
                               (complexity . trill)))
                           (thumb-e
                            . ((offset . (1.75 . 0.4))
                               (stencil . ,bassoon-rh-thumb-e-key-stencil)
                               (text? . ("E" . #f))
                               (complexity . trill)))
                           (thumb-fis
                            . ((offset . (-1.0 . 1.6))
                               (stencil . ,bassoon-rh-thumb-fis-key-stencil)
                               (text? . ("F" . 1))
                               (complexity . trill))))))))
         (graphical-commands
          . ((stencil-alist
              . ((stencils
                  . ,(append (assoc-get 'right-hand-cis-key change-points)
                             `(,(simple-stencil-alist '(hidden . midline)
                                                      '(0.0 . 3.75))
                               ,(simple-stencil-alist '(hidden . long-midline)
                                                      '(0.0 . 3.80))
                               ((stencils
                                 . ,(make-central-column-hole-addresses
                                     CENTRAL-COLUMN-HOLE-LIST))
                                (xy-scale-function . (,identity . ,identity))
                                (textual? . #f)
                                (offset . (0.0 . 0.0)))
                               ,(simple-stencil-alist '(left-hand . high-e)
                                                      '(-1.0 . 7.0))
                               ,(simple-stencil-alist '(left-hand . high-ees)
                                                      '(-1.0 . 6.0))
                               ,(simple-stencil-alist '(left-hand . lh-ees)
                                                      '(-1.0 . 5.0))
                               ((stencils
                                 . ((left-hand . ees) (left-hand . cis)))
                                (xy-scale-function . (,return-1 . ,return-1))
                                (textual? . #f)
                                (offset . (3.0 . 3.75)))
                               ((stencils
                                 . (((stencils
                                      . ((left-hand . low-b)
                                         (left-hand . low-bes)))
                                     (xy-scale-function
                                      . (,return-1 . ,return-1))
                                     (textual? . #f)
                                     (offset . (-2.0 . 9.0)))
                                    ((stencils
                                      . ,(assoc-get 'left-hand-flick-group
                                                    change-points))
                                     (xy-scale-function
                                      . (,return-1 . ,return-1))
                                     (textual? . #f)
                                     (offset . (3.0 . 7.0)))
                                    ,(simple-stencil-alist '(left-hand . low-c)
                                                           '(-1.0 . 4.5))
                                    ,(simple-stencil-alist '(left-hand . low-d)
                                                           '(-1.0 . 0.1))
                                    ((stencils
                                      . ,(assoc-get 'left-hand-thumb-group
                                                    change-points))
                                     (xy-scale-function
                                      . (,return-1 . ,return-1))
                                     (textual? . #f)
                                     (offset . (1.5 . -0.6)))))
                                (xy-scale-function . (,return-1 . ,return-1))
                                (textual? . #f)
                                (offset . (-5.5 . 4.7)))
                               ,(simple-stencil-alist '(right-hand . bes)
                                                      '(1.0 . 1.2))
                               ((stencils
                                 . ,(make-right-hand-key-addresses '(gis f fis)))
                                (xy-scale-function . (,return-1 . ,return-1))
                                (textual? . #f)
                                (offset . (2.0 . -1.25)))
                               ((stencils
                                 . (((stencils
                                      . ((right-hand . thumb-bes)
                                         (right-hand . thumb-e)))
                                     (xy-scale-function
                                      . (,return-1 . ,return-1))
                                     (textual? . #f)
                                     (offset . (-1.22 . 5.25)))
                                    ((stencils
                                      . ,(assoc-get 'right-hand-lower-thumb-group
                                                    change-points))
                                     (xy-scale-function
                                      . (,return-1 . ,return-1))
                                     (textual? . #f)
                                     (offset . (0.0 . 0.0)))))
                                (xy-scale-function
                                 . (,return-1 . ,return-1))
                                (textual? . #f)
                                (offset . (-5.0 . 0.0))))))
                 (xy-scale-function . (,identity . ,identity))
                 (textual? . #f)
                 (offset . (0.0 . 0.0))))
             (draw-instructions
              . ((,apply-group-draw-rule-series
                  (,(make-left-hand-key-addresses '(ees cis))
                   ,(make-left-hand-key-addresses
                     (assoc-get 'back-left-hand-key-addresses change-points))
                   ,(make-right-hand-key-addresses '(f fis gis))
                   ,(make-right-hand-key-addresses
                     (assoc-get 'back-right-hand-key-addresses change-points))))
                 (,group-automate-rule
                  ,(make-central-column-hole-addresses
                    CENTRAL-COLUMN-HOLE-LIST))
                 (,bassoon-midline-rule
                  ,(append
                    (make-left-hand-key-addresses
                     (assoc-get 'back-left-hand-key-addresses change-points))
                    (make-right-hand-key-addresses
                     (assoc-get 'back-right-hand-key-addresses
                                change-points))))))
             (extra-offset-instructions
              . ,(append
                  (assoc-get 'cis-offset-instruction change-points)
                  `((,uniform-extra-offset-rule (0.0 . 0.0)))))))
         (text-commands
          . ((stencil-alist
              . ((stencils
                  . (,(simple-stencil-alist '(hidden . midline) '(0.0 . 3.75))
                     ((stencils
                       . ,(make-central-column-hole-addresses
                           CENTRAL-COLUMN-HOLE-LIST))
                      (xy-scale-function . (,identity . ,identity))
                      (textual? . #f)
                      (offset . (0.0 . 0.0)))
                     ((stencils
                       . ,(make-left-hand-key-addresses
                           '(high-e high-ees lh-ees ees cis)))
                      (textual? . ,lh-woodwind-text-stencil)
                      (offset . (1.5 . 3.75)))
                     ((stencils
                       . ,(make-left-hand-key-addresses
                           (assoc-get 'back-left-hand-key-addresses
                                      change-points)))
                      (textual? . ,rh-woodwind-text-stencil)
                      (offset . (-1.25 . 3.75)))
                     ((stencils
                       . ,(make-right-hand-key-addresses
                           (assoc-get 'front-right-hand-key-addresses
                                      change-points)))
                      (textual? . ,lh-woodwind-text-stencil)
                      (offset . (1.5 . 0.0)))
                     ((stencils .
                                ,(make-right-hand-key-addresses
                                  (assoc-get 'back-right-hand-key-addresses
                                             change-points)))
                      (textual? . ,rh-woodwind-text-stencil)
                      (offset . (-1.25 . 0.0)))))
                 (xy-scale-function . (,identity . ,identity))
                 (textual? . #f)
                 (offset . (0.0 . 0.0))))
             (draw-instructions
              . ((,apply-group-draw-rule-series
                  (,(make-left-hand-key-addresses
                     (assoc-get 'back-left-hand-key-addresses change-points))
                   ,(make-right-hand-key-addresses
                     (assoc-get 'front-right-hand-key-addresses change-points))
                   ,(make-right-hand-key-addresses
                     (assoc-get 'back-right-hand-key-addresses change-points))
                   ,(make-left-hand-key-addresses '(high-e high-ees lh-ees ees cis))))
                 (,group-automate-rule
                  ,(make-central-column-hole-addresses
                    CENTRAL-COLUMN-HOLE-LIST))
                 (,group-automate-rule ((hidden . midline)))))
             (extra-offset-instructions
              . ((,uniform-extra-offset-rule (0.0 . 0.0))))))))))

;; Assembly functions

;; Scans a bank for name.
;; for example, '(left-hand . bes) will return bes in the left-hand
;; of a given bank
(define (get-key name bank)
  (assoc-get (cdr name) (assoc-get (car name) bank)))

(define (translate-key-instruction key-instruction)
  (let*
      ((key-name (car key-instruction))
       (key-complexity (assoc-get 'complexity (cdr key-instruction))))
    (cond
     ((eqv? key-complexity 'basic)
      `((,key-name . ,(assoc-get 'F HOLE-FILL-LIST))))
     ((eqv? key-complexity 'trill)
      (make-symbol-alist key-name #t #f))
     ((eqv? key-complexity 'covered)
      (make-symbol-alist key-name #f #f))
     ((eqv? key-complexity 'ring)
      (make-symbol-alist key-name #f #t)))))

(define (update-possb-list input-key possibility-list canonic-list)
  (if (null? possibility-list)
      (ly:error (G_ "woodwind markup error - invalid key or hole requested"))
      (if
       (assoc-get input-key (cdar possibility-list))
       (append
        `(((,(caaar possibility-list) .
            ,(assoc-get input-key (cdar possibility-list))) .
            ,(assoc-get (caar possibility-list) canonic-list)))
        (alist-delete (caar possibility-list) canonic-list))
       (update-possb-list input-key (cdr possibility-list) canonic-list))))

(define (key-crawler input-list possibility-list)
  (if (null? input-list)
      (map car possibility-list)
      (key-crawler
       (cdr input-list)
       (update-possb-list
        (car input-list)
        possibility-list
        possibility-list))))

(define (translate-draw-instructions input-alist key-name-alist)
  (append-map (lambda (short long)
                (let*
                    ((key-instructions
                      (map (lambda (instr)
                             `(((,long . ,(car instr)) . 0)
                               . ,(translate-key-instruction instr)))
                           (assoc-get long key-name-alist))))
                  (key-crawler (assoc-get short input-alist) key-instructions)))
              '(hd cc lh rh)
              '(hidden central-column left-hand right-hand)))

(define (uniform-draw-instructions key-name-alist)
  (append-map (lambda (long)
                (map (lambda (key-instructions)
                       `((,long . ,(car key-instructions)) . 1))
                     (assoc-get long key-name-alist)))
              '(hidden central-column left-hand right-hand)))

(define (list-all-possible-keys key-name-alist)
  (map (lambda (short long)
         `(,short
           . ,(map (lambda (key-instructions)
                     (car key-instructions))
                   (assoc-get long key-name-alist))))
       '(cc lh rh)
       '(central-column left-hand right-hand)))

(define (list-all-possible-keys-verbose key-name-alist)
  (map (lambda (short long)
         `(,short
           . ,(map (lambda (key-instructions)
                     `(,(car key-instructions)
                       . ,(map (lambda (x)
                                 (car x))
                               (translate-key-instruction key-instructions))))
                   (assoc-get long key-name-alist))))
       '(cc lh rh)
       '(central-column left-hand right-hand)))

(define woodwind-data-assembly-instructions
  `((,generate-flute-family-entry . piccolo)
    (,generate-flute-family-entry . flute)
    (,generate-flute-family-entry . flute-b-extension)
    (,generate-tin-whistle-family-entry . tin-whistle)
    (,generate-oboe-family-entry . oboe)
    (,generate-clarinet-family-entry . clarinet)
    (,generate-clarinet-family-entry . bass-clarinet)
    (,generate-clarinet-family-entry . low-bass-clarinet)
    (,generate-saxophone-family-entry . saxophone)
    (,generate-saxophone-family-entry . soprano-saxophone)
    (,generate-saxophone-family-entry . alto-saxophone)
    (,generate-saxophone-family-entry . tenor-saxophone)
    (,generate-saxophone-family-entry . baritone-saxophone)
    (,generate-bassoon-family-entry . bassoon)
    (,generate-bassoon-family-entry . contrabassoon)))

(define-public woodwind-instrument-list
  (map cdr woodwind-data-assembly-instructions))

(define woodwind-data-alist
  (map (lambda (instruction)
         ((car instruction) (cdr instruction)))
       woodwind-data-assembly-instructions))

;;; The brains of the markup function: takes drawing and offset information
;;; about a key region and calls the appropriate stencils to draw the region.

(define
  (assemble-stencils
   stencil-alist
   key-bank
   draw-instructions
   extra-offset-instructions
   radius
   thick
   xy-stretch
   layout
   props)
  (apply
   ly:stencil-add
   (map (lambda (node)
          (ly:stencil-translate
           (if (pair? (cdr node))
               (if (assoc-get 'textual? node)
                   ((assoc-get 'textual? node) (map (lambda (key)
                                                      (assoc-get 'text? key))
                                                    (map (lambda (instr)
                                                           (get-key
                                                            instr
                                                            key-bank))
                                                         (assoc-get 'stencils node)))
                    radius
                    (map (lambda (key)
                           (assoc-get
                            key
                            draw-instructions))
                         (assoc-get 'stencils
                                    node))
                    layout
                    props)
                   (assemble-stencils
                    node
                    key-bank
                    draw-instructions
                    extra-offset-instructions
                    radius
                    thick
                    (pair-map (assoc-get 'xy-scale-function stencil-alist)
                              xy-stretch)
                    layout
                    props))
               (if (= 0 (assoc-get node draw-instructions))
                   empty-stencil
                   ((assoc-get 'stencil (get-key node key-bank))
                    radius
                    thick
                    (assoc-get node draw-instructions)
                    layout
                    props)))
           (coord-scale
            (coord-translate
             (coord-scale
              (assoc-get
               'offset
               (if (pair? (cdr node))
                   node
                   (get-key node key-bank)))
              (pair-map
               (assoc-get 'xy-scale-function stencil-alist)
               xy-stretch))
             (if
              (assoc-get node extra-offset-instructions)
              (assoc-get node extra-offset-instructions)
              '(0.0 . 0.0)))
            radius)))
        (assoc-get 'stencils stencil-alist))))

(define*-public (print-keys instrument #:optional (port (current-output-port)))
  (format port "\nPrinting keys for: ~a\n" instrument)
  (let ((chosen-instrument (assoc-get instrument woodwind-data-alist)))
    (do ((key-list
          (list-all-possible-keys (assoc-get 'keys chosen-instrument))
          (cdr key-list)))
        ((null? key-list))
      (format port "~a\n   ~a\n" (caar key-list) (cdar key-list)))))

(define-public (get-woodwind-key-list instrument)
  (list-all-possible-keys-verbose
   (assoc-get
    'keys
    (assoc-get instrument woodwind-data-alist))))

(define*-public (print-keys-verbose instrument
                                    #:optional (port (current-output-port)))
  (format port "\nPrinting keys in verbose mode for: ~a\n" instrument)
  (do ((key-list (get-woodwind-key-list instrument)
                 (cdr key-list)))
      ((null? key-list))
    (format port "~a\n" (caar key-list))
    (for-each
     (lambda (x)
       (format port "   possibilities for ~a:\n      ~a\n" (car x) (cdr x)))
     (cdar key-list))))

(define-markup-command
  (woodwind-diagram layout props instrument user-draw-commands)
  (symbol? list?)
  #:category instrument-specific-markup ; markup category
  #:properties ((size 1)
                (thickness 0.1)
                (graphical #t)
                (font-size 0)
                (woodwind-diagram-details '()))
  "Make a woodwind-instrument diagram.  For example, say

@example
\\markup \\woodwind-diagram
  #'oboe #'((lh . (d ees)) (cc . (five3qT1q)) (rh . (gis)))
@end example

@noindent
for an oboe with the left-hand d key, left-hand ees key,
and right-hand gis key depressed while the five-hole of
the central column effectuates a trill between 1/4 and 3/4 closed.

The following instruments are supported:
@itemize @minus

@item
piccolo

@item
flute

@item
oboe

@item
clarinet

@item
bass-clarinet

@item
saxophone

@item
bassoon

@item
contrabassoon

@end itemize

To see all of the callable keys for a given instrument,
include the function @code{(print-keys 'instrument)}
in your .ly file, where instrument is the instrument
whose keys you want to print.

Certain keys allow for special configurations.  The entire gamut of
configurations possible is as follows:

@itemize @minus

@item
1q (1/4 covered)

@item
1h (1/2 covered)

@item
3q (3/4 covered)

@item
R (ring depressed)

@item
F (fully covered; the default if no state put)

@end itemize

Additionally, these configurations can be used in trills.  So, for example,
@code{three3qTR} effectuates a trill between 3/4 full and ring depressed
on the three hole.  As another example, @code{threeRT} effectuates a trill
between R and open, whereas @code{threeTR} effectuates a trill between open
and shut.  To see all of the possibilities for all of the keys of a given
instrument, invoke @code{(print-keys-verbose 'instrument)}.

Lastly, substituting an empty list for the pressed-key alist will result in
a diagram with all of the keys drawn but none filled, for example:

@example
\\markup \\woodwind-diagram #'oboe #'()
@end example"
  (let*  ((radius size)
          (thick (* size thickness))
          (display-graphic graphical)
          (xy-stretch `(1.0 . 2.5))
          (chosen-instrument (assoc-get instrument woodwind-data-alist))
          (chosen-instrument
           (if (not chosen-instrument)
               (ly:error (G_ "~a is not a valid woodwind instrument.")
                         instrument)
               chosen-instrument))
          (stencil-info
           (assoc-get
            (if display-graphic 'graphical-commands 'text-commands)
            chosen-instrument))
          (pressed-info
           (if (null? user-draw-commands)
               (uniform-draw-instructions (assoc-get 'keys chosen-instrument))
               (translate-draw-instructions
                (append '((hd . ())) user-draw-commands)
                (assoc-get 'keys chosen-instrument))))
          (draw-info
           (function-chain
            pressed-info
            (assoc-get 'draw-instructions stencil-info)))
          (extra-offset-info
           (function-chain
            pressed-info
            (assoc-get 'extra-offset-instructions stencil-info))))
    (assemble-stencils
     (assoc-get 'stencil-alist stencil-info)
     (assoc-get 'keys chosen-instrument)
     draw-info
     extra-offset-info
     radius
     thick
     xy-stretch
     layout
     props)))
