;;;; lily-sort.scm -- improved sorting of symbols, strings, and alists.
;;;;
;;;; source file of the GNU LilyPond music typesetter
;;;;
;;;; Copyright 2009--2022 Mark Polesky <markpolesky@yahoo.com>


;; This file implements a LilyPond-specific character-sorting algorithm
;; that can be used to sort lists, alists, etc. consistently and
;; predictably throughout the source code. The primary advantage of this
;; algorithm is that lists are sorted in a more intuitive way, which may
;; allow users to find items faster in the documentation.
;;
;; As an example, a user, looking in the documentation to see if there's
;; a function called "ly:grob?", might assume that there isn't one,
;; since it doesn't appear before "ly:grob-alist-chain" (using the
;; default sort).
;;
;; This happens because "-" comes before "?" in the default sort order.
;; But since "?" is more likely to come at the end of a scheme symbol, a
;; more intuitive sort can be achieved by reversing that order.
;;
;; Similarly, non-alphanumeric characters can be ranked in terms of how
;; likely one will be found closer to the end of a symbol. For example,
;; ":" is stronger separator than "-", as can be seen here:
;;
;; "ly:staff-symbol::print"
;; "ly:staff-symbol-referencer::callback"
;;
;; Intuitively, "staff-symbol-referencer" ought to come after
;; "staff-symbol", but since "-" comes before ":" in the default sort
;; order, these symbols are by default listed in the opposite order.
;;
;; Thus the algorithm implemented here ranks the following nine
;; characters (starting with the space character) in order from
;; most-to-least likely to terminate a symbol: " !?<=>:-_". These nine
;; characters are in effect "extracted" from the default order and then
;; "prepended" to it so that they now come first. This is achieved with
;; the function "ly:char-generic-<?".
;;
;; This file defines 3 case-sensitive binary comparison predicates:
;;   ly:string<?     ly:symbol<?     ly:alist<?
;; and their case-insensitive counterparts:
;;   ly:string-ci<?  ly:symbol-ci<?  ly:alist-ci<?
;;
;; Case-insensitive predicates are recommended in general; otherwise
;; symbols like "Y-offset" appear near the top of lists which
;; otherwise include mostly lowercase symbols.

(define (ly:char-generic-<? a b ci)
  (let* ((init-list (string->list " !?<=>:-_"))
         (mem-a (member a init-list))
         (mem-b (member b init-list)))
    (cond ((and mem-a mem-b) (< (length mem-b) (length mem-a)))
          (mem-a #t)
          (mem-b #f)
          (else ((if ci char-ci<? char<?) a b)))))

(define (ly:char<? a b)
  (ly:char-generic-<? a b #f))

(define (ly:char-ci<? a b)
  (ly:char-generic-<? a b #t))

(define (first-diff-chars str0 str1 ci)
  (let find-mismatch ((a (string->list str0)) (b (string->list str1)))
    (cond ((and (null? a) (null? b)) #f)
          ((null? a) (cons #f (car b)))
          ((null? b) (cons (car a) #f))
          ((not ((if ci char-ci=? char=?) (car a) (car b)))
           (cons (car a) (car b)))
          (else (find-mismatch (cdr a) (cdr b))))))

(define (ly:string-generic-<? a b ci)
  (let ((mismatch (first-diff-chars a b ci)))
    (cond ((and mismatch (car mismatch) (cdr mismatch))
           ((if ci ly:char-ci<? ly:char<?)
            (car mismatch) (cdr mismatch)))
          ((and mismatch (cdr mismatch)) #t)
          (else #f))))

(define (ly:string<? a b)
  "Return #t if string A is less than string B in case-sensitive
  LilyPond sort order."
  (ly:string-generic-<? a b #f))

(define (ly:string-ci<? a b)
  "Return #t if string A is less than string B in case-insensitive
  LilyPond sort order."
  (ly:string-generic-<? a b #t))

(define (ly:symbol<? a b)
  "Return #t if symbol A is less than symbol B in case-sensitive
  LilyPond sort order."
  (ly:string<? (symbol->string a)
               (symbol->string b)))

(define (ly:symbol-ci<? a b)
  "Return #t if symbol A is less than symbol B in case-insensitive
  LilyPond sort order."
  (ly:string-ci<? (symbol->string a)
                  (symbol->string b)))

(define (ly:alist<? a b)
  "Return #t if the first key of alist A is less than the first key of
  alist B, using case-sensitive LilyPond sort order.  Keys are assumed to
  be symbols."
  (ly:string<? (symbol->string (car a))
               (symbol->string (car b))))

(define (ly:alist-ci<? a b)
  "Return #t if the first key of alist A is less than the first key of
  alist B, using case-insensitive LilyPond sort order.  Keys are assumed
  to be symbols."
  (ly:string-ci<? (symbol->string (car a))
                  (symbol->string (car b))))
