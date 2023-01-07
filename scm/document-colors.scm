;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2022--2023 Werner Lemberg  <wl@gnu.org>
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

(define (every-nth-element lst offset nth)
  "Get every NTH element of list LST, with the start element having a zero-based
offset OFFSET"
  (define (build-list l cnt n)
    (cond ((null? l) '())
          ((= cnt n)
           (cons (car l)
                 (build-list (cdr l) 1 n)))
          (else
           (build-list (cdr l) (+ cnt 1) n))))
  (let ((count (- nth offset)))
    (build-list lst count nth)))

(define (list-longest lst)
  "Return the longest symbol (character-wise) in LST as a string."
  (let ((str "")
        (strlen 0))
    (for-each (lambda (x)
                (let* ((s (symbol->string x))
                       (slen (string-length s)))
                  (when (> slen strlen)
                    (set! str s)
                    (set! strlen slen))))
              lst)
    str))

(define (get-longest-color-name lst num-columns column)
  "If the color names of a color definition list LST are arranged as an array
with NUM-COLUMNS columns, return the longest color name (as a string) of the
column with zero-based index COLUMN."
  (list-longest (map car
                     (every-nth-element lst column num-columns))))

(define color-formatter-alist
  '((css . "\"~a\"")
    (definition . "#~a")
    (universal . "#(universal-color '~a)")
    (x11 . "#(x11-color '~a)")
    ))

;; Some large color lists like `x11-unnumbered-color-list` share colors; as a
;; consequence, these lists have to be sorted for documentation.
(define (make-color-doc-string color-list color-type num-columns sorted?)
  (let ((clist (if sorted?
                   (sort color-list ly:alist-ci<?)
                   color-list)))
    (format #f
            "\
@smallIndentedBlock
@multitable ~a
~a
@end multitable
@endSmallIndentedBlock
"
            (string-join
             (map (lambda (x)
                    (format
                     #f
                     "{@code{@ @ @ ~a}}"
                     (get-longest-color-name clist num-columns x)))
                  (iota num-columns)))
            (let ((formatter (assq-ref color-formatter-alist color-type))
                  (column -1))
              (string-join
               (map-in-order (lambda (x)
                               (let ((color (symbol->string (car x))))
                                 (set! column (1+ column))
                                 (format #f
                                         "\
~a
  @lilypond[inline]{
    \\markup
      \\override #'(box-padding . 0)
      \\box \\with-color ~a
        \\filled-box #'(0 . 2) #'(0 . 2) #0}
  @code{~a}"
                                         (if (= (modulo column num-columns) 0)
                                             "@item"
                                             "  @tab")
                                         (format #f formatter color)
                                         color)))
                             clist)
               "\n")))))

(define standard-colors-doc-string
  (make-color-doc-string standard-color-list 'definition 4 #f))
(define x11-unnumbered-colors-doc-string
  (make-color-doc-string x11-unnumbered-color-list 'x11 3 #t))
(define x11-colorN-doc-string
  (make-color-doc-string x11-colorN-list 'x11 4 #t))
(define x11-grayN-doc-string
  (make-color-doc-string x11-grayN-list 'x11 5 #t))
(define css-colors-doc-string
  (make-color-doc-string css-color-list 'css 3 #t))
(define universal-colors-doc-string
  (make-color-doc-string universal-color-list 'universal 4 #f))
