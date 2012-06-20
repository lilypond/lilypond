;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2009--2012 Marc Hohl <marc@hohlart.de>
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

;; helper functions

(define (get-staff-symbol grob)
  (if (grob::has-interface grob 'staff-symbol-interface)
      grob
      (ly:grob-object grob 'staff-symbol)))

(define (layout-blot-diameter grob)
  (let* ((layout (ly:grob-layout grob))
         (blot (ly:output-def-lookup layout 'blot-diameter)))

        blot))

(define (layout-line-thickness grob)
  (let* ((layout (ly:grob-layout grob))
         (line-thickness (ly:output-def-lookup layout 'line-thickness)))

        line-thickness))

(define (staff-symbol-line-count grob)
  (let ((line-count 0))

       (if (ly:grob? grob)
           (let ((line-pos (ly:grob-property grob 'line-positions '())))

                (set! line-count (if (pair? line-pos)
                                     (length line-pos)
                                     (ly:grob-property grob 'line-count 0)))))

         line-count))

(define (staff-symbol-line-span grob)
  (let ((line-pos (ly:grob-property grob 'line-positions '()))
        (iv (cons 0.0 0.0)))

       (if (pair? line-pos)
           (map (lambda (x)
                        (set! iv (cons (min (car iv) x)
                                       (max (cdr iv) x))))
                line-pos)
           (let ((line-count (ly:grob-property grob 'line-count 0)))

                (set! iv (cons (- 1 line-count)
                               (- line-count 1)))))
       iv))

(define (staff-symbol-line-positions grob)
  (let ((line-pos (ly:grob-property grob 'line-positions '())))

       (if (not (pair? line-pos))
           (let* ((line-count (ly:grob-property grob 'line-count 0))
                  (height (- line-count 1.0)))

                 (set! line-pos (map (lambda (x)
                                             (- height (* x 2)))
                                     (iota line-count)))))
       line-pos))

;; functions used by external routines

(define-public (span-bar::notify-grobs-of-my-existence grob)
  (let* ((elts (ly:grob-array->list (ly:grob-object grob 'elements)))
         (sorted-elts (sort elts ly:grob-vertical<?))
         (last-pos (1- (length sorted-elts)))
         (idx 0))

        (map (lambda (g)
                     (ly:grob-set-property!
                       g
                       'has-span-bar
                       (cons (if (eq? idx last-pos)
                                 #f
                                 grob)
                             (if (zero? idx)
                                 #f
                                 grob)))
                      (set! idx (1+ idx)))
             sorted-elts)))

;; How should a bar line behave at a break?
;; the following alist has the form
;; ( unbroken-bar-glyph . ( bar-glyph-at-end-of-line . bar-glyph-at-begin-of-line ))

(define bar-glyph-alist
  '((":|:" . (":|" . "|:"))
    (":|.|:" . (":|" . "|:"))
    (":|.:" . (":|" . "|:"))
    ("||:" . ("||" . "|:"))
    ("dashed" . ("dashed" . '()))
    ("|" . ("|" . ()))
    ("|s" . (() . "|"))
    ("|:" . ("|" . "|:"))
    ("|." . ("|." . ()))

    ;; hmm... should we end with a bar line here?
    (".|" . ("|" . ".|"))
    (":|" . (":|" . ()))
    ("||" . ("||" . ()))
    (".|." . (".|." . ()))
    ("|.|" . ("|.|" . ()))
    ("" . ("" . ""))
    (":" . (":" . ""))
    ("." . ("." . ()))
    ("'" . ("'" . ()))
    ("empty" . (() . ()))
    ("brace" . (() . "brace"))
    ("bracket" . (() . "bracket"))

    ;; segno bar lines
    ("S" . ("||" . "S"))
    ("|S" . ("|" . "S"))
    ("S|" . ("S" . ()))
    (":|S" . (":|" . "S"))
    (":|S." . (":|S" . ()))
    ("S|:" . ("S" . "|:"))
    (".S|:" . ("|" . "S|:"))
    (":|S|:" . (":|" . "S|:"))
    (":|S.|:" . (":|S" . "|:"))

    ;; ancient bar lines
    ("kievan" . ("kievan" . ""))))

;; drawing functions for various bar line types

(define (make-empty-bar-line grob extent)
  (ly:make-stencil "" (cons 0 0) extent))

(define (make-simple-bar-line grob width extent rounded)
  (let ((blot (if rounded
                  (layout-blot-diameter grob)
                  0)))

        (ly:round-filled-box (cons 0 width)
                             extent
                             blot)))

(define (make-tick-bar-line grob height rounded)
  (let ((half-staff (* 1/2 (ly:staff-symbol-staff-space grob)))
        (staff-line-thickness (ly:staff-symbol-line-thickness grob))
        (blot (if rounded
                  (layout-blot-diameter grob)
                  0)))

       (ly:round-filled-box (cons 0 staff-line-thickness)
                            (cons (- height half-staff) (+ height half-staff))
                            blot)))

(define (make-colon-bar-line grob)
  (let* ((staff-space (ly:staff-symbol-staff-space grob))
         (dot (ly:font-get-glyph (ly:grob-default-font grob) "dots.dot"))
         (staff-symbol (get-staff-symbol grob))
         (lines (staff-symbol-line-count staff-symbol))
         (stencil empty-stencil)
         (dist (* (if (or (odd? lines)
                          (zero? lines))
                      1
                      (if (< staff-space 2)
                          2
                          0.5))
                  staff-space)))

        (if (zero? staff-space)
            (set! staff-space 1.0))

        (let* ((stencil (ly:stencil-add stencil dot))
               (stencil (ly:stencil-translate-axis
                          stencil dist Y))
               (stencil (ly:stencil-add stencil dot))
               (stencil (ly:stencil-translate-axis
                          stencil (/ dist -2) Y)))
              stencil)))

(define (make-dotted-bar-line grob extent)
  (let* ((position (round (* (interval-end extent) 2)))
         (correction (if (even? position) 0.5 0.0))
         (dot (ly:font-get-glyph (ly:grob-default-font grob) "dots.dot"))
         (i (round (+ (interval-start extent)
                      (- 0.5 correction))))
         (e (round (+ (interval-end extent)
                      (- 0.5 correction))))
         (counting (interval-length (cons i e)))
         (stil-list (map
                      (lambda (x)
                              (ly:stencil-translate-axis
                                dot (+ x correction) Y))
                      (iota counting i 1))))

        (define (add-stencils! stil l)
          (if (null? l)
              stil
              (if (null? (cdr l))
                  (ly:stencil-add stil (car l))
                  (add-stencils! (ly:stencil-add stil (car l)) (cdr l)))))

        (add-stencils! empty-stencil stil-list)))

(define (make-dashed-bar-line grob extent thickness)
  (let* ((height (interval-length extent))
         (staff-symbol (get-staff-symbol grob))
         (staff-space (ly:staff-symbol-staff-space grob))
         (line-thickness (layout-line-thickness grob))
         (dash-size (- 1.0 (ly:grob-property grob 'gap 0.3)))
         (line-count (staff-symbol-line-count staff-symbol)))

        (if (< (abs (+ line-thickness
                       (* (1- line-count) staff-space)
                       (- height)))
               0.1)
            (let ((blot (layout-blot-diameter grob))
                  (half-space (/ staff-space 2.0))
                  (half-thick (/ line-thickness 2.0))
                  (stencil empty-stencil))

                 (map (lambda (i)
                      (let ((top-y (min (* (+ i dash-size) half-space)
                                        (+ (* (1- line-count) half-space)
                                           half-thick)))
                            (bot-y (max (* (- i dash-size) half-space)
                                        (- 0 (* (1- line-count) half-space)
                                           half-thick))))

                           (set! stencil
                                 (ly:stencil-add
                                   stencil
                                   (ly:round-filled-box (cons 0 thickness)
                                                        (cons bot-y top-y)
                                                        blot)))))
                      (iota line-count (1- line-count) (- 2)))
            stencil)
            (let* ((dashes (/ height staff-space))
                   (total-dash-size (/ height dashes))
                   (factor (/ (- dash-size thickness) staff-space)))

                  (ly:stencil-translate-axis
                    (ly:make-stencil (list 'dashed-line
                                           thickness
                                           (* factor total-dash-size)
                                           (* (- 1 factor) total-dash-size)
                                           0
                                           height
                                           (* factor total-dash-size 0.5))
                                           (cons 0 thickness)
                                           (cons 0 height))
                                           (interval-start extent)
                                           Y)))))

(define (make-segno-bar-line grob glyph extent rounded)
  (let* ((line-thickness (layout-line-thickness grob))
         (kern (* (ly:grob-property grob 'kern 1) line-thickness))
         (thinkern (* (ly:grob-property grob 'thin-kern 1) line-thickness))
         (hair (* (ly:grob-property grob 'hair-thickness 1) line-thickness))
         (fatline (* (ly:grob-property grob 'thick-thickness 1) line-thickness))
         (thin-stil (make-simple-bar-line grob hair extent rounded))
         (thick-stil (make-simple-bar-line grob fatline extent rounded))
         (colon-stil (make-colon-bar-line grob))
         (segno-stil (ly:stencil-add
                       (ly:stencil-combine-at-edge
                         (ly:stencil-combine-at-edge
                           '() X LEFT thin-stil thinkern)
                         X RIGHT thin-stil thinkern)
                       (ly:font-get-glyph (ly:grob-default-font grob) "scripts.varsegno")))
         (glyph (cond
                  ((string=? glyph "|S") "S")
                  ((string=? glyph "S|") "S")
                  (else glyph)))
         (stencil (cond
                    ((or (string=? glyph "S|:")
                         (string=? glyph ".S|:"))
                     (ly:stencil-combine-at-edge
                       (ly:stencil-combine-at-edge
                         (ly:stencil-combine-at-edge
                           thick-stil X RIGHT thin-stil kern)
                         X RIGHT colon-stil kern)
                       X LEFT segno-stil thinkern))
                    ((or (string=? glyph ":|S")
                         (string=? glyph ":|S."))
                     (ly:stencil-combine-at-edge
                       (ly:stencil-combine-at-edge
                         (ly:stencil-combine-at-edge
                           thick-stil X LEFT thin-stil kern)
                         X LEFT colon-stil kern)
                       X RIGHT segno-stil thinkern))
                    ((or (string=? glyph ":|S|:")
                         (string=? glyph ":|S.|:"))
                     (ly:stencil-combine-at-edge
                       (ly:stencil-combine-at-edge
                         (ly:stencil-combine-at-edge
                           (ly:stencil-combine-at-edge
                             (ly:stencil-combine-at-edge
                               (ly:stencil-combine-at-edge
                                 thick-stil X LEFT thin-stil kern)
                               X LEFT colon-stil kern)
                             X RIGHT segno-stil thinkern)
                           X RIGHT thick-stil thinkern)
                         X RIGHT thin-stil kern)
                       X RIGHT colon-stil kern))
                    ((string=? glyph "|._.|")
                     (ly:stencil-combine-at-edge
                       (ly:stencil-combine-at-edge
                         (ly:stencil-combine-at-edge
                           thick-stil X LEFT thin-stil kern)
                         X RIGHT thick-stil (+ (interval-length
                                                 (ly:stencil-extent segno-stil X))
                                               (* 2 thinkern)))
                       X RIGHT thin-stil kern))
                    (else segno-stil))))

       stencil))

(define (make-kievan-bar-line grob)
  (let* ((font (ly:grob-default-font grob))
         (stencil (stencil-whiteout
                    (ly:font-get-glyph font "scripts.barline.kievan"))))

        ;; the kievan bar line has mo staff lines underneath,
        ;; so we whiteout them and move ithe grob to a higher layer
        (ly:grob-set-property! grob 'layer 1)
        stencil))

;; bar line callbacks

(define-public (ly:bar-line::calc-bar-extent grob)
  (let ((staff-symbol (get-staff-symbol grob))
        (staff-extent (cons 0 0)))

       (if (ly:grob? staff-symbol)
           (let* ((bar-line-color (ly:grob-property grob 'color))
                  (staff-color (ly:grob-property staff-symbol 'color))
                  (radius (ly:staff-symbol-staff-radius grob))
                  (staff-line-thickness (ly:staff-symbol-line-thickness grob)))

                 ;; Due to rounding problems, bar lines extending to the outermost edges
                 ;; of the staff lines appear wrongly in on-screen display
                 ;; (and, to a lesser extent, in print) - they stick out a pixel.
                 ;; The solution is to extend bar lines only to the middle
                 ;; of the staff line - unless they have different colors,
                 ;;when it would be undesirable.
                 (set! staff-extent (ly:staff-symbol::height staff-symbol))
                 (if (and (eq? bar-line-color staff-color)
                          radius)
                     (set! staff-extent
                       (interval-scale staff-extent
                                       (- 1 (* 1/2 (/ staff-line-thickness radius))))))))
       staff-extent))

(define (bar-line::bar-y-extent grob refpoint)
  (let* ((extent (ly:grob-property grob 'bar-extent '(0 . 0)))
         (rel-y (ly:grob-relative-coordinate grob refpoint Y))
         (y-extent (coord-translate extent rel-y)))

        y-extent))

(define-public (ly:bar-line::print grob)
  (let ((glyph (ly:grob-property grob 'glyph-name))
        (extent (ly:grob-property grob 'bar-extent '(0 . 0))))

       (if (and (not (eq? glyph '()))
                (> (interval-length extent) 0))
           (bar-line::compound-bar-line grob glyph extent #f)
           #f)))

(define-public (bar-line::compound-bar-line grob glyph extent rounded)
  (let* ((line-thickness (layout-line-thickness grob))
         (height (interval-length extent))
         (kern (* (ly:grob-property grob 'kern 1) line-thickness))
         (thinkern (* (ly:grob-property grob 'thin-kern 1) line-thickness))
         (hair (* (ly:grob-property grob 'hair-thickness 1) line-thickness))
         (fatline (* (ly:grob-property grob 'thick-thickness 1) line-thickness))
         (thin-stil (make-simple-bar-line grob hair extent rounded))
         (thick-stil (make-simple-bar-line grob fatline extent rounded))
         (colon-stil (make-colon-bar-line grob))
         (glyph (cond
                  ((not glyph) "")
                  ((string=? glyph "||:") "|:")
                  ;; bar-line::compound-bar-line is called only if
                  ;; height > 0, but just in case ...
                  ((and (string=? glyph ":|")
                        (zero? height)) "|.")
                  ((and (string=? glyph "|:")
                        (zero? height)) ".|")
                  (else glyph)))
         (stencil (cond
                    ((string=? glyph "|") thin-stil)
                    ((string=? glyph ".") thick-stil)
                    ((string=? glyph "||")
                     (ly:stencil-combine-at-edge
                       (ly:stencil-combine-at-edge
                         '() X LEFT thin-stil thinkern)
                       X RIGHT thin-stil thinkern))
                    ((string=? glyph "|.")
                     (ly:stencil-combine-at-edge
                       thick-stil X LEFT thin-stil kern))
                    ((string=? glyph ".|")
                     (ly:stencil-combine-at-edge
                       thick-stil X RIGHT thin-stil kern))
                    ((string=? glyph "|:")
                     (ly:stencil-combine-at-edge
                       (ly:stencil-combine-at-edge
                         thick-stil X RIGHT thin-stil kern)
                       X RIGHT colon-stil kern))
                    ((string=? glyph ":|")
                     (ly:stencil-combine-at-edge
                       (ly:stencil-combine-at-edge
                         thick-stil X LEFT thin-stil kern)
                       X LEFT colon-stil kern))
                    ((string=? glyph ":|:")
                     (ly:stencil-combine-at-edge
                       (ly:stencil-combine-at-edge
                         (ly:stencil-combine-at-edge
                           (ly:stencil-combine-at-edge
                             '() X LEFT thick-stil thinkern)
                           X LEFT colon-stil kern)
                         X RIGHT thick-stil kern)
                       X RIGHT colon-stil kern))
                    ((string=? glyph ":|.|:")
                     (ly:stencil-combine-at-edge
                       (ly:stencil-combine-at-edge
                         (ly:stencil-combine-at-edge
                           (ly:stencil-combine-at-edge
                             thick-stil X LEFT thin-stil kern)
                           X LEFT colon-stil kern)
                         X RIGHT thin-stil kern)
                       X RIGHT colon-stil kern))
                    ((string=? glyph ":|.:")
                     (ly:stencil-combine-at-edge
                       (ly:stencil-combine-at-edge
                         (ly:stencil-combine-at-edge
                           thick-stil X LEFT thin-stil kern)
                         X LEFT colon-stil kern)
                       X RIGHT colon-stil kern))
                    ((string=? glyph ".|.")
                     (ly:stencil-combine-at-edge
                       (ly:stencil-combine-at-edge
                         '() X LEFT thick-stil thinkern)
                       X RIGHT thick-stil kern))
                    ((string=? glyph "|.|")
                     (ly:stencil-combine-at-edge
                       (ly:stencil-combine-at-edge
                         thick-stil X LEFT thin-stil kern)
                       X RIGHT thin-stil kern))
                    ((string=? glyph ":")
                     (make-dotted-bar-line grob extent))
                    ((or (string=? glyph "|._.|")
                         (string-contains glyph "S"))
                     (make-segno-bar-line grob glyph extent rounded))
                    ((string=? glyph "'")
                     (make-tick-bar-line grob (interval-end extent) rounded))
                    ((string=? glyph "dashed")
                     (make-dashed-bar-line grob extent hair))
                    ((string=? glyph "kievan")
                     (make-kievan-bar-line grob))
                    (else (make-empty-bar-line grob extent)))))
         stencil))

(define-public (ly:bar-line::calc-anchor grob)
  (let* ((line-thickness (layout-line-thickness grob))
         (kern (* (ly:grob-property grob 'kern 1) line-thickness))
         (glyph (ly:grob-property grob 'glyph-name ""))
         (x-extent (ly:grob-extent grob grob X))
         (dot-width (+ (interval-length
                         (ly:stencil-extent
                           (ly:font-get-glyph
                             (ly:grob-default-font grob)
                             "dots.dot")
                           X))
                       kern))
         (anchor 0.0))

        (if (> (interval-length x-extent) 0)
            (begin
              (set! anchor (interval-center x-extent))
              (cond ((string=? glyph "|:")
                     (set! anchor (+ anchor (/ dot-width -2.0))))
                    ((string=? glyph ":|")
                     (set! anchor (+ anchor (/ dot-width 2.0)))))))
        anchor))

(define-public (bar-line::calc-glyph-name grob)
  (let* ((glyph (ly:grob-property grob 'glyph))
         (dir (ly:item-break-dir grob))
         (result (assoc-get glyph bar-glyph-alist))
         (glyph-name (if (= dir CENTER)
                         glyph
                         (if (and result
                                  (string? (index-cell result dir)))
                            (index-cell result dir)
                            #f))))
        glyph-name))

(define-public (bar-line::calc-break-visibility grob)
  (let* ((glyph (ly:grob-property grob 'glyph))
         (result (assoc-get glyph bar-glyph-alist)))

    (if result
        (vector (string? (car result)) #t (string? (cdr result)))
        all-invisible)))

;; which span bar belongs to a bar line?

(define-public span-bar-glyph-alist
  '(("|:" . ".|")
    ("||:" . ".|")
    (":|" . "|.")
    (":|.:" . "|.")
    (":|:" . ".|.")
    (":|.|:" . "|.|")
    (":|.|" . "|.")
    ("S" . "||" )
    ("S|" . "||")
    ("|S" . "||")
    ("S|:" . ".|")
    (".S|:" . ".|")
    (":|S" . "|.")
    (":|S." . "|.")
    (":|S|:" . "|._.|")
    (":|S.|:" . "|._.|")
    ("kievan" . "")
    ("'" . "")))

;; span bar callbacks

(define-public (ly:span-bar::calc-glyph-name grob)
  (let* ((elts (ly:grob-object grob 'elements))
         (pos (1- (ly:grob-array-length elts)))
         (glyph '()))

        (while (and (eq? glyph '())
                    (> pos -1))
               (begin (set! glyph (ly:grob-property (ly:grob-array-ref elts pos)
                                                    'glyph-name))
                      (set! pos (1- pos))))
         (if (eq? glyph '())
             (begin (ly:grob-suicide! grob)
                    (set! glyph "")))
        (assoc-get glyph span-bar-glyph-alist glyph)))

(define-public (ly:span-bar::width grob)
  (let ((width (cons 0 0)))

       (if (grob::is-live? grob)
           (let* ((glyph (ly:grob-property grob 'glyph-name))
                  (stencil (bar-line::compound-bar-line grob glyph (cons -1 1) #f)))

                 (set! width (ly:stencil-extent stencil X))))
       width))

(define-public (ly:span-bar::before-line-breaking grob)
  (let ((elts (ly:grob-object grob 'elements)))

       (if (zero? (ly:grob-array-length elts))
           (ly:grob-suicide! grob))))

;; The method used in the following routine depends on bar_engraver
;; not being removed from staff context.  If bar_engraver is removed,
;; the size of the staff lines is evaluated as 0, which results in a
;; solid span bar line with faulty y coordinate.
;;
;; This routine was originally by Juergen Reuter, but it was a on the
;; bulky side. Rewritten by Han-Wen. Ported from c++ to Scheme by Marc Hohl.
(define-public (ly:span-bar::print grob)
  (let* ((elts-array (ly:grob-object grob 'elements))
         (refp (ly:grob-common-refpoint-of-array grob elts-array Y))
         (elts (reverse (sort (ly:grob-array->list elts-array)
                              ly:grob-vertical<?)))
         ;; Elements must be ordered according to their y coordinates
         ;; relative to their common axis group parent.
         ;; Otherwise, the computation goes mad.
         (glyph (ly:grob-property grob 'glyph-name))
         (span-bar empty-stencil))

        (if (string? glyph)
            (let* ((extents '())
                   (make-span-bars '())
                   (model-bar #f))

                  ;; we compute the extents of each system and store them
                  ;; in a list; dito for the 'allow-span-bar property.
                  ;; model-bar takes the bar grob, if given.
                  (map (lambda (bar)
                       (let* ((ext (bar-line::bar-y-extent bar refp))
                              (staff-symbol (ly:grob-object bar 'staff-symbol)))

                             (if (ly:grob? staff-symbol)
                                 (let ((refp-extent (ly:grob-extent staff-symbol refp Y)))

                                      (set! ext (interval-union ext refp-extent))

                                      (if (> (interval-length ext) 0)
                                          (begin
                                            (set! extents (append extents (list ext)))
                                            (set! model-bar bar)
                                            (set! make-span-bars
                                              (append make-span-bars
                                                      (list (ly:grob-property bar 'allow-span-bar #t))))))))))
                       elts)
                  ;; if there is no bar grob, we use the callback argument
                  (if (not model-bar)
                      (set! model-bar grob))
                  ;; we discard the first entry in make-span-bars, because its corresponding
                  ;; bar line is the uppermost and therefore not connected to another bar line
                  (if (pair? make-span-bars)
                      (set! make-span-bars (cdr make-span-bars)))
                  ;; the span bar reaches from the lower end of the upper staff
                  ;; to the upper end of the lower staff - when allow-span-bar is #t
                  (reduce (lambda (curr prev)
                                  (let ((l (cons 0 0))
                                        (allow-span-bar (car make-span-bars)))

                                       (set! make-span-bars (cdr make-span-bars))
                                       (if (> (interval-length prev) 0)
                                           (begin
                                             (set! l (cons (cdr prev) (car curr)))
                                             (if (or (zero? (interval-length l))
                                                     (not allow-span-bar))
                                                 (begin
                                                   ;; there is overlap between the bar lines
                                                   ;; or 'allow-span-bar = #f.
                                                   ;; Do nothing.
                                                 )
                                                 (set! span-bar
                                                       (ly:stencil-add span-bar
                                                                       (bar-line::compound-bar-line
                                                                         model-bar
                                                                         glyph
                                                                         l
                                                                         #f))))))
                                       curr))
                          "" extents)
                  (set! span-bar (ly:stencil-translate-axis
                                   span-bar
                                   (- (ly:grob-relative-coordinate grob refp Y))
                                   Y))))
        span-bar))
