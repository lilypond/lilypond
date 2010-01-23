;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2008--2010 Carl D. Sorensen <c_sorensen@byu.edu>
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
"Parse a fret-diagram-terse definition string @code{terse-definition} and
return a marking list, which can be used with a fretboard grob."
   (cdr (fret-parse-terse-definition-string (list '()) terse-definition)))

(define-public (get-chord-shape shape-code tuning base-chord-shapes)
"Return the chord shape associated with @code{shape-code} and
@code{tuning} in the hash-table @code{base-chord-shapes}."
  (let ((hash-handle (hash-get-handle base-chord-shapes
                                       (cons shape-code tuning))))
     (if hash-handle
         (cdr hash-handle)
         '())))

(define-public (offset-fret fret-offset diagram-definition)
"Add @code{fret-offset} to each fret indication in @code{diagram-definition}
and return the resulting verbose fret-diagram-definition."
   (let ((verbose-definition 
           (if (string? diagram-definition)
               (parse-terse-string diagram-definition)
               diagram-definition)))
     (map (lambda(item) 
            (let ((code (car item)))
              (cond
                ((eq? code 'barre)
                  (list-set! item 3
                     (+ fret-offset (list-ref item 3)))
                  item)
                ((eq? code 'capo)
                  (list-set! item 1
                     (+ fret-offset (list-ref item 1)))
                  item)
                ((eq? code 'place-fret)
                  (list-set! item 2
                     (+ fret-offset (list-ref item 2)))
                  item)
                (else item))))
            verbose-definition)))

