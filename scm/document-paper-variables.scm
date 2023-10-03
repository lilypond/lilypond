;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2023 Werner Lemberg <wl@gnu.org>
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

(set! all-paper-variable-descriptions
      (sort all-paper-variable-descriptions ly:alist-ci<?))

(define (format-entry lst)
  (let* ((name (symbol->string (first lst)))
         (typename (type-name (second lst)))
         (desc (third lst))
         (entry (cons (string-append "@code{" name "} (" typename ")")
                      (string-append "@funindex " name "\n" desc))))
    entry))

(define (paper-variables-doc-string lst)
  (let* ((descs (map format-entry lst))
         (texi (description-list->texi descs #f)))
    texi))

(define (paper-variables-doc-node)
  (make <texi-node>
    #:appendix #t
    #:name "Paper variables"
    #:desc "All paper variables in a big list."
    #:text (paper-variables-doc-string all-paper-variable-descriptions)))
