;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2021--2022 Jean Abou Samra <jean@abou-samra.fr>
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

(use-modules (ice-9 match))

(define paper-sizes-doc-string
  (string-join
   (map
    (lambda (size-group)
      (define (mm->in mm)
        (/ (ly:mm mm)
           (ly:inch 1)))
      (define (in->mm in)
        (inexact->exact
         (round
          (/ (ly:inch in)
             (ly:mm 1)))))
      (format #f
              "
@noindent
@strong{~a}

@indentedBlock
@multitable @columnfractions 0.27 0.5
~a
@end multitable
@endIndentedBlock
"
              (car size-group)
              (string-join
               (map
                (lambda (size-entry)
                  (match size-entry
                    ((name . ('cons ('* width 'mm)
                                    ('* height 'mm)))
                     (format #f
                             "@item @code{~s} @tab ~d@dmn{mm} x ~d@dmn{mm} \
(~,2f@dmn{in} x ~,2f@dmn{in})"
                             name
                             width
                             height
                             (mm->in width)
                             (mm->in height)))
                    ((name . ('cons ('* width 'in)
                                    ('* height 'in)))
                     (format #f
                             "@item @code{~s} @tab ~f@dmn{in} x ~f@dmn{in} \
(~d@dmn{mm} x ~d@dmn{mm})"
                             name
                             width
                             height
                             (in->mm width)
                             (in->mm height)))))
                (cdr size-group))
               "\n")))
    documented-paper-alist)
   "\n"))
