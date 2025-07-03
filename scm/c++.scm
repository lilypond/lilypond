;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 1998--2023 Jan Nieuwenhuizen <janneke@gnu.org>
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

;; Some of them are defined in `lily.scm` since they are needed to check Scheme
;; command-line arguments.

(define-public (duration-or-music? x)
  (or (ly:duration? x) (ly:music? x)))

(define-public (number-pair? x)
  (and (pair? x)
       (number? (car x)) (number? (cdr x))))

(define-public (number-pair-list? x)
  (and (list? x)
       (every number-pair? x)))

(define-public (fraction? x)
  (and (pair? x)
       (index? (car x)) (index? (cdr x))))

(define-public (positive-fraction? x)
  (and (pair? x)
       (positive-exact-integer? (car x))
       (positive-exact-integer? (cdr x))))

(define-public (exact-rational? x)
  (and (rational? x)
       (exact? x)))

(define (positive-exact-rational? x)
  (and (rational? x) (exact? x) (positive? x)))

(define-public (rational-or-procedure? x)
  (or
   (exact-rational? x)
   (procedure? x)))

(define-public (number-or-grob? x)
  (or (ly:grob? x) (number? x)))

(define-public (non-negative-number? x)
  (and (number? x)
       (or (positive? x)
           (zero? x))))

(define-public (grob-list? x)
  (list? x))

(define-public (number-list? x)
  (and (list? x) (every number? x)))

(define-public (musical-length-as-number? x)
  (or (and (exact-rational? x) (not (negative? x)))
      (and (real? x) (= x +inf.0))))

(define-public (positive-musical-length-as-number? x)
  (or (and (exact-rational? x) (positive? x))
      (and (real? x) (= x +inf.0))))

;; For musical-length properties that have historically been stored as moments.
(define-public (musical-length-as-moment? x)
  (and (ly:moment? x)
       (zero? (ly:moment-grace x))
       (musical-length-as-number? (ly:moment-main x))))

;; For musical-length properties that have historically been stored as moments.
(define-public (positive-musical-length-as-moment? x)
  (and (ly:moment? x)
       (zero? (ly:moment-grace x))
       (positive-musical-length-as-number? (ly:moment-main x))))

;; This allows tolerant input to music functions.  A function that receives a
;; musical-length? argument should normalize it with musical-length->number
;; rather than handle each different type itself.
(define-public (musical-length? x)
  (or (musical-length-as-number? x)
      (musical-length-as-moment? x)
      (and (fraction? x) (not (zero? (cdr x))))))

;; This allows tolerant input to music functions.  A function that receives a
;; positive-musical-length? argument should normalize it with
;; musical-length->number rather than handle each different type itself.
(define-public (positive-musical-length? x)
  (or (positive-musical-length-as-number? x)
      (positive-musical-length-as-moment? x)
      (positive-fraction? x)))

(define-public (musical-length->number x)
  ;; assumes valid input
  (cond ((number? x) x)
        ((pair? x) (/ (car x) (cdr x)))
        ((ly:moment? x) (ly:moment-main x))
        (else #f)))

(define-public (moment-pair? x)
  (and (pair? x)
       (ly:moment? (car x)) (ly:moment? (cdr x))))

(define-public (boolean-or-fraction? x)
  (or (boolean? x) (fraction? x)))

(define-public (boolean-or-number? x)
  (or (boolean? x) (number? x)))

(define-public (boolean-or-symbol? x)
  (or (boolean? x) (symbol? x)))

(define-public (index-or-markup? x)
  (or (index? x) (markup? x)))

(define-public (key? x)
  (or (symbol? x) (index? x)))

(define-public (key-list? x)
  (and (list? x) (every key? x)))

(define-public (key-list-or-music? x)
  (if (list? x)
      (every key? x)
      (ly:music? x)))

(define-public (key-list-or-symbol? x)
  (if (list? x)
      (every key? x)
      (symbol? x)))

(define-public (symbol-list? x)
  (and (list? x) (every symbol? x)))

(define-public (symbol-list-or-music? x)
  (if (list? x)
      (every symbol? x)
      (ly:music? x)))

(define-public (string-or-symbol? x)
  (or (string? x) (symbol? x)))

(define-public (number-or-string? x)
  (or (number? x) (string? x)))

(define-public (string-or-pair? x)
  (or (string? x) (pair? x)))

(define-public (string-or-music? x)
  (or (string? x) (ly:music? x)))

(define-public (number-or-number-pair? x)
  (or (number? x) (number-pair? x)))

(define-public (number-or-pair? x)
  (or (number? x) (pair? x)))

(define-public (cheap-list? x)
  (or (pair? x) (null? x)))

(define-public (symbol-list-or-symbol? x)
  (if (list? x)
      (every symbol? x)
      (symbol? x)))

(define-public (alist? x)
  (and (list? x)
       (every pair? x)))

(define-public (symbol-key-alist? x)
  (and (alist? x)
       (every (lambda (p)
                (symbol? (car p)))
              x)))

(define-public (sane-simple-time-signature? x)
  (and (pair? x)
       (positive-exact-rational? (car x))
       (positive-exact-rational? (cdr x))))

(define-public (sane-time-signature? x)
  "Is @var{x} a supported, semantically valid time signature in canonical form?

A sane time signature is one sane time-signature fraction or a list of two or
more of them (representing concatenation).

A sane time-signature fraction is a pair, @code{(@var{numerator}
.@tie{}@var{denominator})}.  The denominator is a positive exact rational
number.  The numerator is one positive exact rational number or a list of two or
more of them (representing concatenation)."
  (define (sane-time-signature-fraction? x)
    ;; This can't express the silly 3-and-two-halves/4 in "Reverie der Laputier,
    ;; nebst ihren Aufweckern" from _Intrada, nebst burlesquer Suite_ by
    ;; Telemann.  It seems better to keep the code simple and require stencil
    ;; overrides for rare cases like that.
    (define numerator?
      (match-lambda
        ((? positive-exact-rational?) #t)
        (((? positive-exact-rational?) (? positive-exact-rational?) ..1) #t)
        (_ #f)))
    (match x
      (((? numerator?) . (? positive-exact-rational?)) #t)
      (_ #f)))

  (match x
    ((? sane-time-signature-fraction?) #t)
    (((? sane-time-signature-fraction?) (? sane-time-signature-fraction?) ..1)
     #t)
    (_ #f)))

(define-public (scale? x)
  (or (and (rational? x) (exact? x) (not (negative? x)))
      (fraction? x)
      (and (ly:moment? x) (scale? (ly:moment-main x)))))

(define-public (scale->factor x)
  ;; assumes valid input
  (cond ((pair? x) (/ (car x) (cdr x)))
        ((rational? x) x)
        ((ly:moment? x) (ly:moment-main x))
        (else #f)))

(define-public (ly:skyline-pair? x)
  (and (pair? x)
       (ly:skyline? (car x))
       (ly:skyline? (cdr x))))

(define-public (scheme? x) #t)

(define-public (void? x)
  (unspecified? x))

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
        (string-trim-right
         (symbol->string (procedure-name predicate))
         #\?))))
