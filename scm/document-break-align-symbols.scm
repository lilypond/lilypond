;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2026--2026 Lukas-Fabian Moser <lfm@gmx.de>
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

(define grobs-symbols
;;; An alist, indexed by Grob name. Each entry is either
;;; i)  a list of the form: (symbol1 symbol2 ...)
;;; or, in case 'break-align-symbol is a callback without suitable
;;; 'callback-range object property,
;;; ii) simply #f.
  (filter-map
   (lambda (desc)
     (let*
      ((break-align-symbol
        (assoc-get 'break-align-symbol (cdr desc))))
      (cond
       ((procedure? break-align-symbol)
        (let
         ((callback-range
           (object-property break-align-symbol 'callback-range)))
         (if callback-range
             (cons (car desc) (filter symbol? callback-range))
             (begin
              (ly:programming-error
               "\
Grob ~a has a callback for grob property break-align-symbol, \
but the callback has no callback-range object property. \
Please add for documentation."
               (car desc))
              (cons (car desc) #f)))))
       (break-align-symbol
        (list (car desc) break-align-symbol))
       (else #f))))
   all-grob-descriptions))

(define symbols-grobs
;;; A sorted alist, indexed by break-align symbol.
;;; Each entry is a list of grob names.
  (sort-list
   (let loop
     ((grobs-symbols grobs-symbols)
      (grobs-per-symbol '()))
     (define (add-symbol-grob-pair grob symb)
       (let ((symbol-state (assq symb grobs-per-symbol)))
         (if symbol-state
             (set-cdr! symbol-state
                       (cons grob (cdr symbol-state)))
             (set! grobs-per-symbol
                   (cons (list symb grob)
                         grobs-per-symbol)))))
     (if (null? grobs-symbols)
         grobs-per-symbol
         (let*
          ((entry (car grobs-symbols))
           (grob (car entry))
           (symbols (cdr entry)))
          (when symbols
            (for-each (lambda (symb)
                        (add-symbol-grob-pair grob symb))
                      symbols))
          (loop (cdr grobs-symbols)
                grobs-per-symbol))))
   ly:alist<?))

(define break-align-grobs-by-symbols-doc-string
  (format
   #f
   "\
@indentedblock
@raggedright
@multitable @columnfractions 0.5 0.5
@headitem break-align symbol @tab used by grob(s)
~a
@end multitable
@end raggedright
@end indentedblock
"
   (string-join
    (map
     (lambda (entry)
       (format #f
               "@item ~a @tab ~a"
               (symbol->@code (car entry))
               (human-listify (map symbol->@code (cdr entry)))))
     symbols-grobs)
    "\n")))

(define break-align-symbols-by-grobs-doc-string
  (format
   #f
   "\
@indentedblock
@raggedright
@multitable @columnfractions 0.4 0.5
@headitem Grob @tab break-align symbol(s)
~a
@end multitable
@end raggedright
@end indentedblock
"
   (string-join
    (map
     (lambda (entry)
       (let ((grob (car entry))
             (data (cdr entry)))
         (format #f
                 "@item ~a @tab ~a"
                 (symbol->@code grob)
                 (if data
                     (human-listify (map symbol->@code data))
                     "\
unknown (callback missing @code{callback-range} object property)"
                     ))))
     grobs-symbols)
    "\n")))
