;;;; c++.scm -- implement Scheme frontends to C++ functions
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;;
;;;; (c) 1998--2009 Jan Nieuwenhuizen <janneke@gnu.org>
;;;;                 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
  (let ((entry (assoc predicate type-p-name-alist)))
    (if (pair? entry) (cdr entry)
	"unknown")))
