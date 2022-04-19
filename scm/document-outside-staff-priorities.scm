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

(define outside-staff-priorities-doc-string
  (format #f
          "@multitable {@code{CenteredBarNumberLineSpanner}} {Priority}
@headitem Grob @tab Priority
~a
@end multitable
"
          (string-join
           (map
            (lambda (name-priority-pair)
              (let ((name (car name-priority-pair))
                    (priority (cdr name-priority-pair)))
                (format #f "@item @code{~a} @tab @code{~a}" name priority)))
            (sort
             (filter-map
              (lambda (grob-definition)
                (let* ((name (car grob-definition))
                       (properties (cdr grob-definition))
                       (priority (assq-ref properties 'outside-staff-priority)))
                  (and priority (cons name priority))))
              all-grob-descriptions)
             (comparator-from-key cdr <)))
           "\n")))
