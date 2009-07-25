;;; song-util.scm --- Festival singing mode output

;; Copyright (C) 2006, 2007 Brailcom, o.p.s.

;; Author: Milan Zamazal <pdm@brailcom.org>

;; COPYRIGHT NOTICE

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.


(define-module (scm song-util))

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
       (let ((,record ,(make-record-type name (map car slots*))))
         (set! ,$record?
               (lambda (record) ((record-predicate ,record) record)))
         (set! ,$make-record
               (lambda* (#:key ,@slots)
                 ((record-constructor ,record) ,@(map car slots*))))
         (set! ,$copy-record
               (lambda (record)
                 (,$make-record ,@(apply
                                   append
                                   (map (lambda (slot)
                                          (list (symbol->keyword slot)
                                                (list (make-symbol reader-format slot) 'record)))
                                        (map car slots*))))))
         ,@(map (lambda (s)
                  `(set! ,(make-symbol reader-format (car s))
                         (record-accessor ,record (quote ,(car s)))))
                slots*)
         ,@(map (lambda (s)
                  `(set! ,(make-symbol writer-format (car s))
                         (record-modifier ,record (quote ,(car s)))))
                slots*)))))
(export defstruct)

(define-public (compose . functions)
  (let ((functions* (drop-right functions 1))
        (last-function (last functions)))
    (letrec ((reduce (lambda (x functions)
                       (if (null? functions)
                           x
                           (reduce ((car functions) x) (cdr functions))))))
      (lambda args (reduce (apply (last functions) args) (reverse functions*))))))

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
  "Return true iff MUSIC's PROPERTY is equal to VALUE."
  (equal? (ly:music-property music property) value))

(define-public (music-name? music name)
  "Return true iff MUSIC's name is NAME."
  (if (list? name)
      (member (ly:music-property music 'name) name)
      (music-property-value? music 'name name)))

(define-public (music-property? music property)
  "Return true iff MUSIC is a property setter and sets or unsets PROPERTY."
  (and (music-name? music '(PropertySet PropertyUnset))
       (music-property-value? music 'symbol property)))

(define-public (music-has-property? music property)
  "Return true iff MUSIC contains PROPERTY."
  (not (eq? (ly:music-property music property) '())))

(define-public (property-value music)
  "Return value of a property setter MUSIC.
If it unsets the property, return #f."
  (if (music-name? music 'PropertyUnset)
      #f
      (ly:music-property music 'value)))

(define-public (music-elements music)
  "Return list of all MUSIC's top-level children."
  (let ((elt (ly:music-property music 'element))
        (elts (ly:music-property music 'elements)))
    (if (not (null? elt))
        (cons elt elts)
        elts)))

(define-public (find-child music predicate)
  "Find the first node in MUSIC that satisfies PREDICATE."
  (define (find-child queue)
    (if (null? queue)
        #f
        (let ((elt (car queue)))
          (if (predicate elt)
              elt
              (find-child (append (music-elements elt) (cdr queue)))))))
  (find-child (list music)))

(define-public (find-child-named music name)
  "Return the first child in MUSIC that is named NAME."
  (find-child music (lambda (elt) (music-name? elt name))))

(define-public (process-music music function)
  "Process all nodes of MUSIC (including MUSIC) in the DFS order.
Apply FUNCTION on each of the nodes.
If FUNCTION applied on a node returns true, don't process the node's subtree."
  (define (process-music queue)
    (if (not (null? queue))
        (let* ((elt (car queue))
               (stop (function elt)))
          (process-music (if stop
                             (cdr queue)
                             (append (music-elements elt) (cdr queue)))))))
  (process-music (list music)))
