;;;; song-util.scm --- Festival singing mode output
;;;;
;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2006--2022 Brailcom, o.p.s.
;;;; Author: Milan Zamazal <pdm@brailcom.org>
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


(define-module (lily song-util))

(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs))
(use-modules (ice-9 pretty-print))

(use-modules (lily))


;;; Debugging utilities


;; Iff true, enable a lot of debugging output
(define-public *debug* #f)

(define-macro (assert condition . data)
  (if *debug*
      `(if (not ,condition)
           (error "Assertion failed" (quote ,condition) ,@data))
      #f))
(export assert)

(define-macro (debug message object)
  (if *debug*
      `(debug* ,message ,object)
      object))
(export debug)

(define (debug* message object)
  (display "[[") (display message) (display "]] ") (pretty-print object)
  object)


;;; General utilities


(define-macro (defstruct name . slots)
  ;; Similar as in Common Lisp, but much simplier -- no structure and slot options, no docstring
  (let* ((slots* (map (lambda (s) (if (pair? s) s (list s))) slots))
         (make-symbol (lambda (format% . extra-args)
                        (string->symbol (apply format #f format% name extra-args))))
         ($record? (make-symbol "~a?"))
         ($make-record (make-symbol "make-~a"))
         ($copy-record (make-symbol "copy-~a"))
         (reader-format "~a-~a")
         (writer-format "set-~a-~a!")
         (record (gensym)))
    `(begin
       (define ,$record? #f)
       (define ,$make-record #f)
       (define ,$copy-record #f)
       ,@(map (lambda (s) `(define ,(make-symbol reader-format (car s)) #f)) slots*)
       ,@(map (lambda (s) `(define ,(make-symbol writer-format (car s)) #f)) slots*)
       (let ((,record (make-record-type ',name ',(map car slots*))))
         (set! ,$record?
               (lambda (record) ((record-predicate ,record) record)))
         (set! ,$make-record
               (lambda* (#:key ,@slots)
                        ((record-constructor ,record) ,@(map car slots*))))
         (set! ,$copy-record
               (lambda (record)
                 (,$make-record ,@(append-map
                                   (lambda (slot)
                                     (list (symbol->keyword slot)
                                           (list (make-symbol reader-format slot) 'record)))
                                   (map car slots*)))))
         ,@(map (lambda (s)
                  `(set! ,(make-symbol reader-format (car s))
                         (record-accessor ,record (quote ,(car s)))))
                slots*)
         ,@(map (lambda (s)
                  `(set! ,(make-symbol writer-format (car s))
                         (record-modifier ,record (quote ,(car s)))))
                slots*)))))
(export defstruct)

(define-macro (push! object list-var)
  ;; The same as in Common Lisp
  `(set! ,list-var (cons ,object ,list-var)))
(export push!)

(define-macro (add! object list-var)
  `(set! ,list-var (append ,list-var (list ,object))))
(export add!)

(define-public (safe-car list)
  (if (null? list)
      #f
      (car list)))

(define-public (safe-last list)
  (if (null? list)
      #f
      (last list)))


;;; LilyPond utility functions


(define-public (music-property-value? music property value)
  "Return @code{#t} iff @var{music}'s @var{property} is equal to
@var{value}."
  (equal? (ly:music-property music property) value))

(define-public (music-name? music name)
  "Return @code{#t} iff @var{music}'s name is @var{name}."
  (if (list? name)
      (member (ly:music-property music 'name) name)
      (music-property-value? music 'name name)))

(define-public (music-property? music property)
  "Return @code{#t} iff @var{music} is a property setter and sets
or unsets @var{property}."
  (and (music-name? music '(PropertySet PropertyUnset))
       (music-property-value? music 'symbol property)))

(define-public (music-has-property? music property)
  "Return @code{#t} iff @var{music} contains @var{property}."
  (not (eq? (ly:music-property music property) '())))

(define-public (property-value music)
  "Return value of a property setter @var{music}.
If it unsets the property, return @code{#f}."
  (if (music-name? music 'PropertyUnset)
      #f
      (ly:music-property music 'value)))

(define-public (music-elements music)
  "Return list of all @var{music}'s top-level children."
  (let ((elt (ly:music-property music 'element))
        (elts (ly:music-property music 'elements))
        (arts (ly:music-property music 'articulations)))
    (if (pair? arts)
        (set! elts (append elts arts)))
    (if (null? elt)
        elts
        (cons elt elts))))

(define-public (find-child music predicate)
  "Find the first node in @var{music} that satisfies @var{predicate}."
  (define (find-child queue)
    (if (null? queue)
        #f
        (let ((elt (car queue)))
          (if (predicate elt)
              elt
              (find-child (append (music-elements elt) (cdr queue)))))))
  (find-child (list music)))

(define-public (find-child-named music name)
  "Return the first child in @var{music} that is named @var{name}."
  (find-child music (lambda (elt) (music-name? elt name))))

(define-public (process-music music function)
  "Process all nodes of @var{music} (including @var{music}) in the DFS order.
Apply @var{function} on each of the nodes.  If @var{function} applied on a
node returns @code{#t}, don't process the node's subtree.

If a non-boolean is returned, it is considered the material to recurse."
  (define (process-music queue)
    (if (not (null? queue))
        (let* ((elt (car queue))
               (stop (function elt)))
          (process-music (if (boolean? stop)
                             (if stop
                                 (cdr queue)
                                 (append (music-elements elt) (cdr queue)))
                             ((if (cheap-list? stop) append cons)
                              stop (cdr queue)))))))
  (process-music (list music)))
