;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2008--2022 Carl D. Sorensen <c_sorensen@byu.edu>
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


(define-public (parse-terse-string terse-definition)
  "Parse a @code{fret-diagram-terse} definition string @var{terse-definition}
and return a marking list, which can be used with a fretboard grob."
  (cdr (fret-parse-terse-definition-string (list '()) terse-definition)))

(define-public (get-chord-shape shape-code tuning base-chord-shapes)
  "Return the chord shape associated with @var{shape-code} and
@var{tuning} in the hash-table @var{base-chord-shapes}."
  (let ((hash-handle (hash-get-handle base-chord-shapes
                                      (cons shape-code tuning))))
    (if hash-handle
        (cdr hash-handle)
        '())))

(define-public (offset-fret fret-offset diagram-definition)
  "Add @var{fret-offset} to each fret indication in
@var{diagram-definition} and return the resulting verbose
@code{fret-diagram-definition}."
  (let ((verbose-definition
         (if (string? diagram-definition)
             (parse-terse-string diagram-definition)
             diagram-definition)))
    (map (lambda (item)
           (let* ((code (car item))
                  (nth (assq-ref '((barre . 3) (capo . 1) (place-fret . 2))
                                 code)))
             (if nth
                 ;; offset nth element of item by offset-fret
                 ;; without modifying the original list but
                 ;; sharing its tail
                 (let ((tail (list-tail item nth)))
                   (append! (list-head item nth)
                            (cons (+ (car tail) fret-offset)
                                  (cdr tail))))
                 item)))
         verbose-definition)))
