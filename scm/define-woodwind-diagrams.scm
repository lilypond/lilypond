;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2010--2022 Mike Solomon <mikesol@stanfordalumni.org>
;;;;    Clarinet drawings copied from diagrams created by
;;;;    Gilles Thibault <gilles.thibault@free.fr>
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

(define HOLE-FILL-LIST '((R . 3) (1q . 5) (1h . 7) (3q . 11) (F . 13)))

;; Utility functions

(define-public (symbol-concatenate . names)
  "Like @code{string-concatenate}, but for symbols."
  (string->symbol (string-concatenate (map symbol->string names))))

(define-public (function-chain arg function-list)
  "Apply a list of functions in @var{function-list} to @var{arg}.
Each element of @var{function-list} is structured @code{(cons function
'(arg2 arg3 ...))}.  If function takes arguments besides @var{arg}, they
are provided in @var{function-list}.  Example:

@example
(function-chain 1 `((,+ 1) (,- 2) (,+ 3) (,/)))
   @result{} 1/3
@end example"
  (fold
   (lambda (fun arg) (apply (car fun) arg (cdr fun)))
   arg
   function-list))

(define (assoc-keys alist)
  "Gets the keys of an alist."
  (map car alist))

(define (assoc-values alist)
  "Gets the values of an alist."
  (map cdr alist))

(define (get-slope-offset p1 p2)
  "Gets the slope and offset for p1 and p2.
   For example:
   @code{(get-slope-offset '(1 . 2) '(3 . -5.1))}
   @code{(-3.55 . 5.55)}"
  (let*
      ((slope (/ (- (cdr p1) (cdr p2)) (- (car p1) (car p2))))
       (offset (- (cdr p1) (* slope (car p1)))))
    `(,slope . ,offset)))

(define (is-square? x input-list)
  "Returns true if x is the square of a value in input-list."
  (pair? (memv (inexact->exact (sqrt x)) input-list)))

(define (true-entry? input-list)
  "Is there a true entry in @code{input-list}?"
  (any identity input-list))

(define (entry-greater-than-x? input-list x)
  "Is there an entry greater than @code{x} in @code{input-list}?"
  (member x input-list <))

(define (n-true-entries input-list)
  "Returns number of true entries in @code{input-list}."
  (count identity input-list))

(define (bezier-head-for-stencil bezier cut-point)
  "Prepares a split-bezier to be used in a connected path stencil."
  (list-tail (flatten-list (ly:bezier-extract bezier 0 cut-point)) 2))

;; Translators for keys

;; Translates a "normal" key (open, closed, trill)
(define (key-fill-translate fill)
  (cond
   ((= fill 1) #f)
   ((= fill 2) #f)
   ((= fill (expt (assoc-get 'F HOLE-FILL-LIST) 2)) 0.5)
   ((= fill (assoc-get 'F HOLE-FILL-LIST)) #t)))

;; Similar to above, but trans vs opaque doesn't matter
(define (text-fill-translate fill)
  (cond
   ((< fill 3) 1.0)
   ((= fill (expt (assoc-get 'F HOLE-FILL-LIST) 2)) 0.5)
   ((= fill (assoc-get 'F HOLE-FILL-LIST)) 0.0)))

;; Emits a list for the central-column-hole maker
;; (not-full?, 1-quarter-full?, 1-half-full?, 3-quarters-full?, full?)
;; Multiple values, such as (#t #f #f #t #f), mean a trill between
;; not-full and 3-quarters-full
(define (process-fill-value fill)
  (let* ((avals (list-tail (assoc-values HOLE-FILL-LIST) 1)))
    (append `(,(or (< fill 3) (is-square? fill avals)))
            (map (lambda (x) (= 0 (remainder fill x))) avals))))

;; Color a stencil gray
(define (gray-colorize stencil)
  (apply ly:stencil-in-color stencil (x11-color 'grey)))

;; A connected path stencil that is surrounded by proc
(define (rich-path-stencil ls x-stretch y-stretch proc)
  (lambda (radius thick fill layout props)
    (let*
        ((fill-translate (key-fill-translate fill))
         (gray? (eqv? fill-translate 0.5)))
      (ly:stencil-add
       ((if gray? gray-colorize identity)
        (proc
         (make-connected-path-stencil
          ls
          thick
          (* x-stretch radius)
          (* y-stretch radius)
          #f
          (if gray? #t fill-translate))))
       (if (not gray?)
           empty-stencil
           ((rich-path-stencil ls x-stretch y-stretch proc)
            radius
            thick
            1
            layout
            props))))))

;; A connected path stencil without a surrounding proc
(define (standard-path-stencil ls x-stretch y-stretch)
  (rich-path-stencil ls x-stretch y-stretch identity))

;; An ellipse stencil that is surrounded by a proc
(define (rich-pe-stencil x-stretch y-stretch start end proc)
  (lambda (radius thick fill layout props)
    (let*
        ((fill-translate (key-fill-translate fill))
         (gray? (eqv? fill-translate 0.5)))
      (ly:stencil-add
       ((if gray? gray-colorize identity)
        (proc
         (make-partial-ellipse-stencil
          (* x-stretch radius)
          (* y-stretch radius)
          start
          end
          thick
          #t
          (if gray? #t fill-translate))))
       (if (not gray?)
           empty-stencil
           ((rich-pe-stencil x-stretch y-stretch start end proc)
            radius
            thick
            1
            layout
            props))))))

(define (rich-e-stencil x-stretch y-stretch proc)
  (lambda (radius thick fill layout props)
    (let*
        ((fill-translate (key-fill-translate fill))
         (gray? (eqv? fill-translate 0.5)))
      (ly:stencil-add
       ((if gray? gray-colorize identity)
        (proc
         (make-ellipse-stencil
          (* x-stretch radius)
          (* y-stretch radius)
          thick
          (if gray? #t fill-translate))))
       (if (not gray?)
           empty-stencil
           ((rich-e-stencil x-stretch y-stretch proc)
            radius
            thick
            1
            layout
            props))))))

;; An ellipse stencil without a surrounding proc
(define (standard-e-stencil x-stretch y-stretch)
  (rich-e-stencil x-stretch y-stretch identity))

;; Translates all possible representations of symbol.
;; If simple? then the only representations are open, closed, and trill.
;; Otherwise, there can be various levels of "closure" on the holes
;; ring? allows for a ring around the holes as well
(define (make-symbol-alist symbol simple? ring?)
  (delete `(,(symbol-concatenate symbol 'T 'F) .
            ,(expt (assoc-get 'F HOLE-FILL-LIST) 2))
          `((,symbol . ,(assoc-get 'F HOLE-FILL-LIST))
            (,(symbol-concatenate symbol 'T) .
             ,(expt (assoc-get 'F HOLE-FILL-LIST) 2))
            ,@(if simple?
                  '()
                  (append-map
                   (lambda (x)
                     `((,(symbol-concatenate symbol (car x) 'T)
                        . ,(expt (cdr x) 2))
                       (,(symbol-concatenate symbol 'T (car x))
                        . ,(* (cdr x) (assoc-get 'F HOLE-FILL-LIST)))
                       (,(symbol-concatenate symbol (car x))
                        . ,(cdr x))
                       ,@(append-map
                          (lambda (y)
                            (map (lambda (a b)
                                   `(,(symbol-concatenate symbol
                                                          (car a)
                                                          'T
                                                          (car b))
                                     . ,(* (cdr a) (cdr b))))
                                 `(,x ,y) `(,y ,x)))
                          (cdr (member x HOLE-FILL-LIST)))))
                   (if ring? HOLE-FILL-LIST (cdr HOLE-FILL-LIST)))))))

;;; Commands for text layout

;; Distinguish a markup if (= trigger 0.5) - either with a circle or in grey
(define-markup-command
  (conditional-trill-markup layout props trigger in-markup)
  (number? markup?)
  (let* ((wwd-details (chain-assoc-get 'woodwind-diagram-details props '()))
         (text-trill-circled (assoc-get 'text-trill-circled wwd-details #t)))
    (interpret-markup layout props
                      (if (eqv? trigger 0.5)
                          (if text-trill-circled
                              (make-circle-markup in-markup)
                              (make-with-color-markup grey in-markup))
                          in-markup))))

;; Makes a list of named-keys
(define (make-name-keylist input-list key-list font-size)
  (map (lambda (x y)
         (if (< x 1)
             (make-conditional-trill-markup-markup
              x
              (make-concat-markup
               (list
                (make-abs-fontsize-markup font-size (car y))
                (if (and (< x 1) (cdr y))
                    (make-abs-fontsize-markup
                     font-size
                     (make-raise-markup
                      (/ font-size 12)
                      (make-fontsize-markup
                       -2
                       (if (eqv? (cdr y) 1)
                           (make-sharp-markup)
                           (make-flat-markup)))))
                    (make-null-markup)))))
             (make-null-markup)))
       input-list key-list))

;; Makes a list of number-keys
(define (make-number-keylist input-list key-list font-size)
  (map (lambda (x y)
         (if (< x 1)
             (make-conditional-trill-markup-markup
              x
              (make-abs-fontsize-markup font-size (make-number-markup y)))
             (make-null-markup)))
       input-list
       key-list))

;; Creates a named-key list with a certain alignment
(define (aligned-text-stencil-function dir hv)
  (lambda (key-name-list radius fill-list layout props)
    (interpret-markup
     layout
     props
     (make-general-align-markup
      X
      dir
      ((if hv make-concat-markup make-center-column-markup)
       (make-name-keylist
        (map text-fill-translate fill-list)
        key-name-list
        (* 12 radius
           (magstep
            (chain-assoc-get 'font-size props 0)))))))))

(define number-column-stencil
  (lambda (key-name-list radius fill-list layout props)
    (interpret-markup
     layout
     props
     (make-general-align-markup
      Y
      CENTER
      (make-general-align-markup
       X
       RIGHT
       (make-override-markup
        '(baseline-skip . 0)
        (make-column-markup
         (make-number-keylist
          (map text-fill-translate fill-list)
          key-name-list
          (* radius 8
             (magstep
              (chain-assoc-get 'font-size props 0)))))))))))

;; Utility function for the left-hand keys
(define lh-woodwind-text-stencil
  (aligned-text-stencil-function LEFT #t))

;; Utility function for the right-hand keys
(define rh-woodwind-text-stencil
  (aligned-text-stencil-function RIGHT #t))

(define octave-woodwind-text-stencil
  (aligned-text-stencil-function CENTER #f))

;;; Draw rules

(define (rich-group-draw-rule alist target-part change-part)
  (if
   (entry-greater-than-x?
    (map (lambda (key) (assoc-get key alist)) target-part) 3)
   (map-selected-alist-keys (lambda (x) (if (= x 0) 1 x)) change-part alist)
   alist))

(define (bassoon-midline-rule alist target-part)
  (if
   (entry-greater-than-x?
    (map (lambda (key) (assoc-get key alist)) target-part) 0)
   (map-selected-alist-keys (lambda (x) 1) '((hidden . long-midline)) alist)
   (map-selected-alist-keys (lambda (x) 1) '((hidden . midline)) alist)))

(define (group-draw-rule alist target-part)
  (rich-group-draw-rule alist target-part target-part))

(define (group-automate-rule alist change-part)
  (map-selected-alist-keys (lambda (x) (if (= x 0) 1 x)) change-part alist))

(define (apply-group-draw-rule-series alist target-part-list)
  (if (null? target-part-list)
      alist
      (apply-group-draw-rule-series
       (group-draw-rule alist (car target-part-list))
       (cdr target-part-list))))

;; Extra-offset rules

(define (rich-group-extra-offset-rule alist target-part change-part eos)
  (if
   (entry-greater-than-x?
    (map (lambda (key) (assoc-get key alist)) target-part) 0)
   (map-selected-alist-keys (lambda (x) eos) change-part alist)
   alist))

(define (group-extra-offset-rule alist target-part eos)
  (rich-group-extra-offset-rule alist target-part target-part eos))

(define (uniform-extra-offset-rule alist eos)
  (map-selected-alist-keys
   (lambda (x) (if (pair? x) x eos))
   (assoc-keys alist)
   alist))

;;; General drawing commands

;; Used all the time for a dividing line
(define (midline-stencil radius thick fill layout props)
  (make-line-stencil (* thick 2) (* -0.80 radius) 0 (* 0.80 radius) 0))

(define (long-midline-stencil radius thick fill layout props)
  (make-line-stencil (* thick 2) (* -5.75 radius) 0 (* 0.75 radius) 0))

;; Used all the time for a small, between-hole key
(define little-elliptical-key-stencil (standard-e-stencil 0.75 0.2))

;; Used for several upper keys in the clarinet and sax
(define (upper-key-stencil tailw tailh bodyw bodyh)
  (let*
      ((xmove (lambda (x) (+ tailw (+ 0.2 (* bodyw (- x 0.2))))))
       (ymove (lambda (x) (+ (- tailh) (+ -0.05 (* bodyh (+ x 0.05)))))))
    (standard-path-stencil
     `((,(xmove 0.7)
        ,(ymove -0.2)
        ,(xmove 1.0)
        ,(ymove -1.0)
        ,(xmove 0.5)
        ,(ymove -1.0))
       (,(xmove 0.2)
        ,(ymove -1.0)
        ,(xmove 0.2)
        ,(ymove -0.2)
        ,(xmove 0.3)
        ,(ymove -0.1))
       (,(+ 0.2 tailw)
        ,(- -0.05 tailh)
        ,(+ 0.1 (/ tailw 2))
        ,(- -0.025 (/ tailh 2))
        0.0
        0.0))
     1.0
     1.0)))

;; Utility function for the column-hole maker.
;; Returns the left and right degrees for the drawing of a given
;; fill level (1-quarter, 1-half, etc...)
(define (degree-first-true fill-list left? reverse?)
  (define (dfl-crawler fill-list os-list left?)
    (if (car fill-list)
        ((if left? car cdr) (car os-list))
        (dfl-crawler (cdr fill-list) (cdr os-list) left?)))
  (dfl-crawler
   ((if reverse? reverse identity) fill-list)
   ((if reverse? reverse identity)
    '((0 . 0) (215 . 325) (180 . 0) (145 . 35) (90 . 90)))
   left?))

;; Gets the position of the first (or last if reverse?) element of a list.
(define (position-true-endpoint in-list reverse?)
  (define (pte-crawler in-list n)
    (if (car in-list)
        n
        (pte-crawler (cdr in-list) (+ n 1))))
  ((if reverse? - +)
   (if reverse? (length in-list) 0)
   (pte-crawler ((if reverse? reverse identity) in-list) 0)))

;; Huge, kind-of-ugly maker of a circle in a column.
;; I think this is the clearest way to write it, though...

(define (column-circle-stencil radius thick fill layout props)
  (let* ((fill-list (process-fill-value fill))
         (wwd-details (chain-assoc-get 'woodwind-diagram-details props '()))
         (fill-angle (assoc-get 'fill-angle wwd-details 0)))
    (cond
     ((and
       (list-ref fill-list 0)
       (not (true-entry? (list-tail fill-list 1)))) ; is it empty?
      ((standard-e-stencil 1.0 1.0) radius thick fill layout props))
     ((and
       (list-ref fill-list 4)
       (not (true-entry? (list-head fill-list 4)))) ; is it full?
      ((standard-e-stencil 1.0 1.0) radius thick fill layout props))
     ((and
       (list-ref fill-list 0)
       (list-ref fill-list 4)) ; is it a trill between empty and full?
      ((standard-e-stencil 1.0 1.0) radius thick fill layout props))
     (else  ;If none of these, it is partially full.
      (ly:stencil-add
       ((rich-pe-stencil 1.0 1.0 0 360 identity)
        radius
        thick
        (if (list-ref fill-list 4)
            (expt (assoc-get 'F HOLE-FILL-LIST) 2)
            1)
        layout
        props)
       ((rich-pe-stencil
         1.0
         1.0
         (+ fill-angle (degree-first-true fill-list #t #t))
         (+ fill-angle (degree-first-true fill-list #f #t))
         identity)
        radius
        thick
        (if
         (= 2 (n-true-entries fill-list)) ; trill between empty and partially full
         (expt (assoc-get 'F HOLE-FILL-LIST) 2)
         (assoc-get 'F HOLE-FILL-LIST))
        layout
        props)
       (if
        (= 2 (n-true-entries (list-tail fill-list 1))) ; trill between two partially full states
        ((rich-pe-stencil
          1.0
          1.0
          (+ fill-angle (degree-first-true fill-list #t #f))
          (+ fill-angle (degree-first-true fill-list #f #f))
          identity)
         radius
         thick
         (assoc-get 'F HOLE-FILL-LIST)
         layout
         props)
        empty-stencil))))))

(define (variable-column-circle-stencil scaler)
  (lambda (radius thick fill layout props)
    (column-circle-stencil (* radius scaler) thick fill layout props)))

;; A stencil for ring-column circles that combines two of the above
(define (ring-column-circle-stencil radius thick fill layout props)
  (if (= 0 (remainder fill (assoc-get 'R HOLE-FILL-LIST)))
      (ly:stencil-add
       ((if
         (= fill (expt (assoc-get 'R HOLE-FILL-LIST) 2))
         gray-colorize
         identity)
        ((standard-e-stencil
          (+ (- 1.0 (* 2 (/ thick radius))) (/ thick radius 2))
          (+ (- 1.0 (* 2 (/ thick radius))) (/ thick radius 2)))
         radius
         (* 4 thick)
         1
         layout
         props))
       ((standard-e-stencil 1.0 1.0) radius thick 1 layout props)
       (column-circle-stencil
        (* radius (+ (- 1.0 (* 4 (/ thick radius))) (/ thick radius 2)))
        thick
        (*
         (if (= 0 (remainder fill (assoc-get 'F HOLE-FILL-LIST)))
             (assoc-get 'F HOLE-FILL-LIST)
             1)
         (if (= fill (expt (assoc-get 'R HOLE-FILL-LIST) 2))
             (/ fill (expt (assoc-get 'R HOLE-FILL-LIST) 2))
             (/ fill (assoc-get 'R HOLE-FILL-LIST))))
        layout
        props))
      (column-circle-stencil radius thick fill layout props)))

;;; Flute family stencils

(define flute-lh-b-key-stencil
  (standard-path-stencil
   '((0 1.3)
     (0 1.625 -0.125 1.75 -0.25 1.75)
     (-0.55 1.75 -0.55 0.95 -0.25 0.7)
     (0 0.4 0 0.125 0 0))
   2
   1.55))

(define flute-lh-bes-key-stencil
  (standard-path-stencil
   '((0 1.3)
     (0 1.625 -0.125 1.75 -0.25 1.75)
     (-0.55 1.75 -0.55 0.95 -0.25 0.7)
     (0 0.4 0 0.125 0 0))
   2.0
   1.3))

(define (flute-lh-gis-rh-bes-key-stencil deg)
  (rich-path-stencil
   '((0.1 0.1 0.2 0.4 0.3 0.6)
     (0.3 1.0 0.8 1.0 0.8 0.7)
     (0.8 0.3 0.5 0.3 0 0))
   1.0
   1.0
   (lambda (stencil) (ly:stencil-rotate stencil deg 0 0))))

(define flute-lh-gis-key-stencil (flute-lh-gis-rh-bes-key-stencil 0))

(define flute-rh-bes-key-stencil (flute-lh-gis-rh-bes-key-stencil 200))

(define flute-rh-d-key-stencil little-elliptical-key-stencil)

(define flute-rh-dis-key-stencil little-elliptical-key-stencil)

(define flute-rh-ees-key-stencil
  (standard-path-stencil
   '((0.8 0) (1.1 0 1.1 0.75 0.7 0.75) (0.5 0.75) (0.15 0.75 0.1 0.2 0 0))
   -2.38
   1.4))

(define (piccolo-rh-x-key-stencil radius thick fill layout props)
  (interpret-markup
   layout
   props
   (make-general-align-markup
    Y
    DOWN
    (make-concat-markup
     (make-name-keylist
      `(,(text-fill-translate fill))
      '(("X" . #f))
      (* 9 radius
         (magstep
          (chain-assoc-get 'font-size props 0))))))))

(define flute-lower-row-stretch 1.4)

(define flute-rh-cis-key-stencil
  (standard-path-stencil
   '((0 0.75) (-0.8 0.75 -0.8 0 0 0))
   flute-lower-row-stretch
   flute-lower-row-stretch))

(define flute-rh-c-key-stencil
  (standard-path-stencil
   '((0 0.75) (0.4 0.75) (0.4 0) (0 0))
   flute-lower-row-stretch
   flute-lower-row-stretch))

(define flute-rh-b-key-stencil
  (standard-path-stencil
   '((0 0.75) (0.25 0.75) (0.25 0) (0 0))
   flute-lower-row-stretch
   flute-lower-row-stretch))

(define flute-rh-gz-key-stencil
  (rich-path-stencil
   '((0.1 0.1 0.4 0.2 0.6 0.3)
     (1.0 0.3 1.0 0.8 0.7 0.8)
     (0.3 0.8 0.3 0.5 0 0))
   flute-lower-row-stretch
   flute-lower-row-stretch
   (lambda (stencil) (ly:stencil-rotate stencil 160 0 0))))

;;; Shared oboe/clarinet stencils

(define (oboe-lh-gis-lh-low-b-key-stencil gis?)
  (let*
      ((x 1.2)
       (y 0.4)
       (scaling-factor 1.7)
       (up-part
        (ly:bezier-extract
         `((0.0 . 0.0) (0.0 . ,y) (,x . ,y) (,x . 0.0))
         0
         0.8))
       (down-part
        (ly:bezier-extract
         `((,x . 0.0) (,x . ,(- y)) (0.0 . ,(- y)) (0.0 . 0.0))
         0.2
         1)))
    (if gis?
        (standard-path-stencil
         (append
          (append
           `((0.25 ,(/ y -2) 0.75 ,(/ y -2) 1.0 0.0))
           (map (lambda (l)
                  (flatten-list
                   (map (lambda (x)
                          (coord-translate
                           (coord-rotated x (cons y (* 2 0.25)))
                           '(1.0 . 0)))
                        l)))
                `(((0 . ,y) (,x . ,y) (,x . 0))
                  ((,x . ,(- y)) (0 . ,(- y)) (0 . 0)))))
          `((0.75 ,(/ y -2) 0.25 ,(/ y -2) 0.0 0.0)))
         scaling-factor
         scaling-factor)
        (standard-path-stencil
         (map (lambda (l)
                (flatten-list
                 (map (lambda (x)
                        (coord-rotated x (cons y (* 2 0.25))))
                      l)))
              `(,(list-tail up-part 1)
                ,(list-head down-part 1)
                ,(list-tail down-part 1)))
         (- scaling-factor)
         (- scaling-factor)))))

(define oboe-lh-gis-key-stencil (oboe-lh-gis-lh-low-b-key-stencil #t))

(define oboe-lh-low-b-key-stencil (oboe-lh-gis-lh-low-b-key-stencil #f))

(define (oboe-lh-ees-lh-bes-key-stencil ees?)
  (standard-path-stencil
   `((0 1.5)
     (0 1.625 -0.125 1.75 -0.25 1.75)
     (-0.5 1.75 -0.5 0.816 -0.25 0.5)
     (0 0.25 0 0.125 0 0)
     (0 ,(if ees? -0.6 -0.3)))
   (* (if ees? -1.0 1.0) -1.8)
   1.8))

(define oboe-lh-ees-key-stencil (oboe-lh-ees-lh-bes-key-stencil #t))

(define oboe-lh-bes-key-stencil (oboe-lh-ees-lh-bes-key-stencil #f))

;;; Oboe family stencils

(define (oboe-lh-octave-key-stencil long?)
  (let* ((h (if long? 1.4 1.2)))
    (standard-path-stencil
     `((-0.4 0 -0.4 1.0 -0.1 1.0)
       (-0.1 ,h)
       (0.1 ,h)
       (0.1 1.0)
       (0.4 1.0 0.4 0 0 0))
     2.0
     2.0)))

(define oboe-lh-I-key-stencil (oboe-lh-octave-key-stencil #f))

(define oboe-lh-II-key-stencil (oboe-lh-octave-key-stencil #f))

(define oboe-lh-III-key-stencil (oboe-lh-octave-key-stencil #t))

(define oboe-lh-b-key-stencil (standard-e-stencil 0.6 0.8))

(define oboe-lh-d-key-stencil little-elliptical-key-stencil)

(define oboe-lh-cis-key-stencil little-elliptical-key-stencil)

(define oboe-lh-f-key-stencil (standard-e-stencil 0.5 1.0))

(define oboe-rh-a-key-stencil (standard-e-stencil 1.0 0.45))

(define oboe-rh-gis-key-stencil (standard-e-stencil 0.45 1.2))

(define oboe-rh-d-key-stencil little-elliptical-key-stencil)

(define oboe-rh-f-key-stencil little-elliptical-key-stencil)

(define (oboe-rh-c-rh-ees-key-stencil c?)
  (rich-path-stencil
   '((1.0 0.0 1.0 0.70 1.5 0.70)
     (2.25 0.70 2.25 -0.4 1.5 -0.4)
     (1.0 -0.4 1.0 0 0 0)
     (-0.15 0))
   2.0
   1.4
   (lambda (stencil) (ly:stencil-rotate stencil (if c? 170 180) 0 0))))

(define oboe-rh-banana-key-stencil oboe-rh-gis-key-stencil)

(define oboe-rh-c-key-stencil (oboe-rh-c-rh-ees-key-stencil #t))

(define oboe-rh-cis-key-stencil
  (rich-path-stencil
   '((0.6 0.0 0.6 0.50 1.25 0.50)
     (2.25 0.50 2.25 -0.4 1.25 -0.4)
     (0.6 -0.4 0.6 0 0 0))
   -0.9
   1.0
   (lambda (stencil) (ly:stencil-rotate stencil 0 0 0))))

(define oboe-rh-ees-key-stencil (oboe-rh-c-rh-ees-key-stencil #f))

;;; Clarinet family stencils

(define clarinet-lh-thumb-key-stencil
  (variable-column-circle-stencil 0.9))

(define clarinet-lh-R-key-stencil
  (let* ((halfbase (cos (/ PI 10)))
         (height (*
                  halfbase
                  (/ (sin (/ (* 4 PI) 10)) (cos (/ (* 4 PI) 10))))))
    (standard-path-stencil
     `(
       (0 ,(/ -4.0 3.0) -2.0 ,(/ -4.0 3.0) -2.0 0.0)
       (-1.5 ,(* 0.5 height) -1.25 ,(* 0.75 height) -1.0 ,height)
       (-0.75 ,(* 0.75 height) -0.5 ,(* 0.5 height) 0.0 0.0))
     0.9
     0.9)))

(define (clarinet-lh-a-key-stencil radius thick fill layout props)
  (let* ((width 0.4) (height 0.75) (linelen 0.45))
    (ly:stencil-add
     ((standard-e-stencil width height) radius thick fill layout props)
     (ly:stencil-translate
      (make-line-stencil thick 0 0 0 (* linelen radius))
      (cons 0 (* height radius))))))

(define clarinet-lh-gis-key-stencil (upper-key-stencil 0.0 0.0 1.3 2.0))

(define clarinet-lh-ees-key-stencil little-elliptical-key-stencil)

(define clarinet-lh-cis-key-stencil oboe-lh-gis-key-stencil)

(define clarinet-lh-f-key-stencil oboe-lh-low-b-key-stencil)

(define clarinet-lh-e-key-stencil oboe-lh-ees-key-stencil)

(define clarinet-lh-fis-key-stencil oboe-lh-bes-key-stencil)

(define clarinet-lh-d-key-stencil (standard-e-stencil 1.0 0.4))

(define clarinet-rh-low-c-key-stencil
  (standard-path-stencil
   '((0.0 1.5)
     (0.0 2.5 -1.0 2.5 -1.0 0.75)
     (-1.0 0.1 0.0 0.25 0.0 0.3)
     (0.0 0.0))
   0.8
   0.8))

(define clarinet-rh-low-cis-key-stencil
  (standard-path-stencil
   '((0.0 1.17)
     (0.0 1.67 -1.0 1.67 -1.0 0.92)
     (-1.0 0.47 0.0 0.52 0.0 0.62)
     (0.0 0.0))
   0.8
   0.8))

(define clarinet-rh-low-d-key-stencil
  (standard-path-stencil
   '((0.0 1.05)
     (0.0 1.55 -1.0 1.55 -1.0 0.8)
     (-1.0 0.35 0.0 0.4 0.0 0.5)
     (0.0 0.0))
   0.8
   0.8))

(define clarinet-rh-one-key-stencil (standard-e-stencil 0.5 0.25))

(define clarinet-rh-two-key-stencil clarinet-rh-one-key-stencil)

(define clarinet-rh-three-key-stencil clarinet-rh-one-key-stencil)

(define clarinet-rh-four-key-stencil clarinet-rh-one-key-stencil)

(define clarinet-rh-b-key-stencil little-elliptical-key-stencil)

;; cl low-rh values
(define CL-RH-HAIR 0.09)
(define CL-RH-H-STRETCH 2.7)
(define CL-RH-V-STRETCH 0.9)

;; TODO
;; there is some unnecessary information duplication here.
;; need a way to control all of the below stencils so that if one
;; changes, all change...

(define clarinet-rh-fis-key-stencil
  (standard-path-stencil
   `(,(bezier-head-for-stencil
       '((0.0 . 0.0) (0.0 . -1.0) (1.0 . -1.0) (1.0 . 0.0))
       0.5)
     ,(bezier-head-for-stencil
       '((0.5 . -0.75) (0.5 . 0.25) (1.5 . 0.25) (1.5 . -0.75))
       0.5)
     (1.0 1.0 0.0 1.0 0.0 0.0))
   CL-RH-H-STRETCH
   CL-RH-V-STRETCH))

(define clarinet-rh-gis-key-stencil
  (standard-path-stencil
   '((0.0 1.0 1.0 1.0 1.0 0.0) (1.0 -1.0 0.0 -1.0 0.0 0.0))
   CL-RH-H-STRETCH
   CL-RH-V-STRETCH))

(define clarinet-rh-e-key-stencil
  (standard-path-stencil
   `(,(bezier-head-for-stencil
       '((0.0 .  0.0) (0.0 . -1.0) (1.0 . -1.0) (1.0 . 0.0))
       0.5)
     ,(bezier-head-for-stencil
       '((0.5 . -0.75) (0.5 . 0.25) (1.5 . 0.25) (1.5 . -0.75))
       0.5)
     ,(bezier-head-for-stencil
       `((1.0 . 0.0) (,(/ 1 3) . 0.0) (,(/ 1 3) . 1.5) (1.0 .  1.5))
       0.5)
     ,(bezier-head-for-stencil
       `((0.5 . 0.75) (,(/ -1 6) . 0.75) (,(/ -1 6) . -0.75) (0.5 . -0.75))
       0.5))
   CL-RH-H-STRETCH
   CL-RH-V-STRETCH))

(define clarinet-rh-f-key-stencil clarinet-rh-gis-key-stencil)

(define bass-clarinet-rh-ees-key-stencil
  (standard-path-stencil
   `(,(bezier-head-for-stencil
       '((0.0 . 0.0) (0.0 . -1.0) (1.0 . -1.0) (1.0 . 0.0))
       0.5)
     ,(bezier-head-for-stencil
       '((0.5 . -0.75) (0.5 . 0.25) (1.5 . 0.25) (1.5 . -0.75))
       0.5)
     (1.0 1.0 0.0 1.0 0.0 0.0))
   CL-RH-H-STRETCH
   (- CL-RH-V-STRETCH)))

(define low-bass-clarinet-rh-ees-key-stencil clarinet-rh-e-key-stencil)

(define clarinet-rh-d-key-stencil clarinet-rh-gis-key-stencil)

;;; Saxophone family stencils

(define saxophone-lh-ees-key-stencil (upper-key-stencil 0.0 0.0 1.3 2.0))

(define saxophone-lh-f-key-stencil (upper-key-stencil 0.0 0.0 1.3 2.0))

(define saxophone-lh-d-key-stencil (upper-key-stencil 0.0 0.0 1.3 2.0))

(define saxophone-lh-front-f-key-stencil (standard-e-stencil 0.7 0.7))

(define saxophone-lh-bes-key-stencil (standard-e-stencil 0.5 0.5))

(define saxophone-lh-T-key-stencil (standard-e-stencil 0.75 0.75))

(define saxophone-lh-gis-key-stencil
  (standard-path-stencil
   '((0.0 0.4)
     (0.0 0.8 3.0 0.8 3.0 0.4)
     (3.0 0.0)
     (3.0 -0.4 0.0 -0.4 0.0 0.0))
   0.8
   0.8))

(define (saxophone-lh-b-cis-key-stencil flip?)
  (standard-path-stencil
   '((0.0 1.0)
     (0.4 1.0 0.8 0.9 1.35 0.8)
     (1.35 0.0)
     (0.0 0.0))
   (* (if flip? -1 1) 0.8)
   0.8))

(define saxophone-lh-cis-key-stencil (saxophone-lh-b-cis-key-stencil #t))

(define saxophone-lh-b-key-stencil (saxophone-lh-b-cis-key-stencil #f))

(define saxophone-lh-low-bes-key-stencil
  (standard-path-stencil
   '((3.0 0.0) (3.0 -1.5 0.0 -1.5 0.0 0.0))
   0.8
   0.8))

(define (saxophone-rh-side-key-stencil width height)
  (standard-path-stencil
   `((0.0 ,height)
     (0.05 ,(+ height 0.05) 0.1 ,(+ height 0.1) 0.15 ,(+ height 0.15))
     (,(- width 0.15) ,(+ height 0.15))
     (,(- width 0.1)
      ,(+ height 0.1)
      ,(- width 0.05)
      ,(+ height 0.05)
      ,width
      ,height)
     (,width 0.0)
     (,(- width 0.05) -0.05 ,(- width 0.1) -0.1 ,(- width 0.15) -0.15)
     (0.15 -0.15)
     (0.1 -0.1 0.05 -0.05 0.0 0.0))
   1.0
   1.0))

(define saxophone-rh-e-key-stencil (saxophone-rh-side-key-stencil 0.9 1.2))

(define saxophone-rh-c-key-stencil (saxophone-rh-side-key-stencil 0.9 0.6))

(define saxophone-rh-bes-key-stencil (saxophone-rh-side-key-stencil 0.9 0.45))

(define saxophone-rh-high-fis-key-stencil
  (standard-path-stencil
   (let* ((angle -30)
          (dir2 (ly:directed (* -0.5 angle)))
          ;; This comparatively awful expression calculates how far
          ;; along the tangents opened by 'angle' with a radius of 0.6
          ;; the control points need to move in order to have the
          ;; middle of the bezier curve exactly on radius.
          (out (* 0.6 (coord-y dir2) (- 4/3 (* 1/3 (coord-x dir2))))))
     (append
      '((0.0 1.0) (0.0 1.4 0.6 1.4 0.6 1.0) (0.6 0.0))
      `((0.6 ,(- out)
             ,@(flatten-list (map (lambda (x) (coord-rotated x angle))
                                  `((0.6 . ,out)
                                    (0.6 . 0.0))))))
      (map (lambda (l)
             (flatten-list
              (map (lambda (x)
                     (coord-rotated x angle))
                   l)))
           '(((0.6 . -1.0))
             ((0.6 . -1.4) (0.0 . -1.4) (0.0 . -1.0))
             ((0.0 . 0.0))))))
   0.75
   0.75))

(define saxophone-rh-fis-key-stencil (standard-e-stencil 1.0 0.5))

(define saxophone-rh-ees-key-stencil (standard-e-stencil 1.2 0.5))

(define saxophone-rh-low-c-key-stencil
  (standard-path-stencil
   '((3.0 0.0) (3.0 -1.5 0.0 -1.5 0.0 0.0))
   0.8
   0.8))

(define (saxophone-lh-low-a-key-stencil radius thick fill layout props)
  (interpret-markup
   layout
   props
   (make-general-align-markup
    Y
    DOWN
    (make-concat-markup
     (make-name-keylist
      `(,(text-fill-translate fill))
      '(("lowA" . #f))
      (* 9 radius
         (magstep
          (chain-assoc-get 'font-size props 0))))))))

;;; Bassoon family stencils

(define (bassoon-bend-info-maker height gap cut)
  (let* (
         (first-bezier
          (flatten-list
           (ly:bezier-extract
            `((0.0 . ,(+ height gap))
              (0.0 . ,(+ height (+ gap 1.0)))
              (1.0 . ,(+ height (+ gap 2.0)))
              (2.0 . ,(+ height (+ gap 2.0))))
            0
            cut)))
         (second-bezier
          (flatten-list
           (reverse
            (ly:bezier-extract
             `((1.0 . ,height)
               (1.0 . ,(+ 0.5 height))
               (1.5 . ,(+ 1.0 height))
               (2.0 . ,(+ 1.0 height)))
             0
             cut))))
         (slope-offset1
          (get-slope-offset
           `(,(list-ref first-bezier 4) . ,(list-ref first-bezier 5))
           `(,(list-ref first-bezier 6) . ,(list-ref first-bezier 7))))
         (slope-offset2
          (get-slope-offset
           `(,(list-ref second-bezier 0) . ,(list-ref second-bezier 1))
           `(,(list-ref second-bezier 2) . ,(list-ref second-bezier 3)))))
    (list first-bezier second-bezier slope-offset1 slope-offset2)))

(define
  (make-tilted-portion
   first-bezier
   second-bezier
   slope-offset1
   slope-offset2
   keylen
   bezier?)
  (append
   `((,(+ keylen (list-ref first-bezier 6))
      ,(+
        (*
         (car slope-offset1)
         (+ keylen (list-ref first-bezier 6))) (cdr slope-offset1))))
   ((if bezier? (lambda (x) `(,(concatenate x))) identity)
    `((,(+ (+ keylen 1.75) (list-ref first-bezier 6))
       ,(+
         (*
          (car slope-offset1)
          (+ (+ keylen 1.75) (list-ref first-bezier 6)))
         (cdr slope-offset1)))
      (,(+ (+ keylen 1.75) (list-ref second-bezier 0))
       ,(+
         (*
          (car slope-offset2)
          (+ (+ keylen 1.75) (list-ref second-bezier 0)))
         (cdr slope-offset2)))
      (,(+ keylen (list-ref second-bezier 0))
       ,(+
         (* (car slope-offset2)  (+ keylen (list-ref second-bezier 0)))
         (cdr slope-offset2)))))
   `(,(list-head second-bezier 2))))

(define (rich-bassoon-uber-key-stencil height gap cut keylen d1 d2 proc bezier?)
  (let* ((info-list (bassoon-bend-info-maker height gap cut))
         (first-bezier (car info-list))
         (second-bezier (cadr info-list))
         (slope-offset1 (caddr info-list))
         (slope-offset2 (cadddr info-list)))
    (rich-path-stencil
     (append
      `((0.0 ,(+ height gap))
        ,(list-tail first-bezier 2))
      (make-tilted-portion
       first-bezier
       second-bezier
       slope-offset1
       slope-offset2
       keylen
       bezier?)
      `(,(list-tail second-bezier 2)
        (1.0 0.0)
        (0.0 0.0)))
     d1
     d2
     proc)))

(define (bassoon-uber-key-stencil height gap cut keylen d1 d2)
  (rich-bassoon-uber-key-stencil height gap cut keylen d1 d2 identity #t))

(define bassoon-cc-six-key-stencil (standard-e-stencil 1.5 0.8))

(define bassoon-lh-he-key-stencil little-elliptical-key-stencil)

(define bassoon-lh-hees-key-stencil little-elliptical-key-stencil)

(define bassoon-lh-lhees-key-stencil little-elliptical-key-stencil)

(define bassoon-lh-ees-key-stencil
  (rich-e-stencil
   1.2
   0.6
   (lambda (stencil) (ly:stencil-rotate stencil 30 0 0))))

(define bassoon-lh-cis-key-stencil
  (rich-e-stencil
   1.0
   0.5
   (lambda (stencil) (ly:stencil-rotate stencil 30 0 0))))

(define bassoon-lh-lbes-key-stencil
  (bassoon-uber-key-stencil 1.0 0.5 0.7 0.5 0.6 -0.6))

(define bassoon-lh-lb-key-stencil
  (bassoon-uber-key-stencil 2.0 0.5 0.9 1.2 0.6 -0.6))

(define bassoon-lh-lc-key-stencil
  (rich-pe-stencil 1.0 1.0 135 315 identity))

(define bassoon-lh-ld-key-stencil
  (standard-path-stencil
   '((-0.8 4.0 1.4 4.0 0.6 0.0)
     (0.5 -0.5 0.5 -0.8 0.6 -1.0)
     (0.7 -1.2 0.8 -1.3 0.8 -1.8)
     (0.5 -1.8)
     (0.5 -1.4 0.4 -1.2 0.3 -1.1)
     (0.2 -1.0 0.1 -0.5 0.0 0.0))
   1.0
   1.0))

(define bassoon-lh-d-flick-key-stencil
  (let ((height 3.0))
    (standard-path-stencil
     `((0.0 ,height)
       (0.2 ,(+ height 1.6) 0.8 ,(+ height 1.8) 1.0 ,(+ height 1.8))
       (1.4 ,(+ height 1.8) 1.9 ,(+ height 1.3) 1.9 ,(+ height 1.0))
       (1.9 ,(+ height 0.7) 1.0 ,(+ height 0.4) 0.8 ,(+ height 0.3))
       (0.6 ,(+ height 0.2) 0.4 ,(+ height 0.1) 0.4 ,(- height 0.1))
       (0.4 0.0)
       (0.0 0.0))
     -1.0
     -1.0)))

(define bassoon-lh-c-flick-key-stencil
  (let ((height 3.0))
    (standard-path-stencil
     `((0.0 ,height)
       (0.0 ,(+ height 1.6) 0.4 ,(+ height 1.8) 0.5 ,(+ height 1.8))
       (0.7 ,(+ height 1.8) 0.9 ,(+ height 1.3) 0.9 ,(+ height 1.0))
       (0.9 ,(+ height 0.5) 0.7 ,(+ height 0.4) 0.6 ,(+ height 0.3))
       (0.5 ,(+ height 0.2) 0.4 ,(+ height 0.1) 0.4 ,(- height 0.1))
       (0.4 0.0)
       (0.0 0.0))
     -1.0
     -1.0)))

(define bassoon-lh-a-flick-key-stencil
  (bassoon-uber-key-stencil 5.0 1.0 0.3 0.6 -0.5 -0.5))

(define bassoon-lh-thumb-cis-key-stencil
  (bassoon-uber-key-stencil 1.5 1.5 0.6 0.6 -0.6 0.6))

(define bassoon-lh-whisper-key-stencil (variable-column-circle-stencil 0.7))

(define bassoon-rh-cis-key-stencil
  (rich-bassoon-uber-key-stencil
   1.1
   1.5
   0.9
   0.3
   0.5
   0.5
   (lambda (stencil) (ly:stencil-rotate stencil -76 0 0))
   #t))

(define bassoon-rh-bes-key-stencil little-elliptical-key-stencil)

(define bassoon-rh-fis-key-stencil
  (rich-bassoon-uber-key-stencil 0.5 1.0 0.8 1.5 -0.7 0.7 identity #f))

(define bassoon-rh-f-key-stencil
  (let* ((height 0.5) (gap 1.0) (cut 0.8) (keylen 1.5)
         (info-list (bassoon-bend-info-maker height gap cut))
         (first-bezier (car info-list))
         (second-bezier (cadr info-list))
         (slope-offset1 (caddr info-list))
         (slope-offset2 (cadddr info-list)))
    (standard-path-stencil
     (append
      (map
       (lambda (l)
         (map
          -
          l
          (apply circular-list (list-tail first-bezier 6))))
       (make-tilted-portion
        first-bezier
        second-bezier
        slope-offset1
        slope-offset2
        keylen
        #t))
      '((0.0 0.0)))
     -0.7
     0.7)))

(define bassoon-rh-gis-key-stencil
  (bassoon-uber-key-stencil 0.3 1.0 0.8 1.0 -0.7 0.7))

(define bassoon-rh-thumb-bes-key-stencil
  (bassoon-uber-key-stencil 1.0 1.0 0.9 1.0 0.7 0.7))

(define bassoon-rh-thumb-e-key-stencil (variable-column-circle-stencil 0.7))

(define bassoon-rh-thumb-fis-key-stencil
  (bassoon-uber-key-stencil 1.0 1.2 0.9 1.0 0.7 0.7))

(define bassoon-rh-thumb-gis-key-stencil
  (bassoon-uber-key-stencil 1.2 0.8 0.9 0.4 0.7 0.7))
