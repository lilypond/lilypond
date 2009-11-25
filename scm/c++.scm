;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 1998--2009 Jan Nieuwenhuizen <janneke@gnu.org>
;;;;                 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

;;; Note: this file can't be used without LilyPond executable


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type predicates.
(define-public (number-pair? x)
  (and (pair? x)
       (number? (car x)) (number? (cdr x))))

(define-public (number-or-grob? x)
  (or (ly:grob? x) (number? x)))

(define-public (grob-list? x)
  (list? x))

(define-public (moment-pair? x)
  (and (pair? x)
       (ly:moment? (car x)) (ly:moment? (cdr x))))

(define-public (boolean-or-symbol? x)
  (or (boolean? x) (symbol? x)))

(define-public (string-or-symbol? x)
  (or (string? x) (symbol? x)))

(define-public (number-or-string? x)
  (or (number? x) (string? x)))

(define-public (string-or-pair? x)
  (or (string? x) (pair? x)))

(define-public (cheap-list? x)
  (or (pair? x) (null? x)))

(define-public (list-or-symbol? x)
  (or (cheap-list? x) (symbol? x)))

(define-public (scheme? x) #t)


;; moved list to end of lily.scm: then all type-predicates are
;; defined.
(define type-p-name-alist '())

(define (match-predicate obj alist)
  (if (null? alist)
      "Unknown type"
      (if (apply (caar alist) obj)
	  (cdar alist)
	  (match-predicate obj (cdr alist)))))

(define-public (object-type obj)
  (match-predicate obj type-p-name-alist))

(define-public (object-type-name obj)
  (type-name (match-predicate obj type-p-name-alist)))

(define-public (type-name predicate)
  (assoc-get predicate type-p-name-alist "unknown"))
