;;;; lily-sort.scm -- improved sorting of symbols, strings, and alists.
;;;;
;;;; source file of the GNU LilyPond music typesetter
;;;;
;;;; Copyright (C) 2009--2023 Mark Polesky <markpolesky@yahoo.com>
;;;; Copyright (C) 2022--2023 Jean Abou Samra <jean@abou-samra.fr>

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
;; Furthermore, the string comparison function recognizes numeric parts
;; and sorts them by numeric value, so that "grey10" comes after "grey2".
;;
;; This file defines 3 case-sensitive binary comparison predicates:
;;   ly:string<?     ly:symbol<?     ly:alist<?
;; and their case-insensitive counterparts:
;;   ly:string-ci<?  ly:symbol-ci<?  ly:alist-ci<?
;;
;; Case-insensitive predicates are recommended in general; otherwise
;; symbols like "Y-offset" appear near the top of lists which
;; otherwise include mostly lowercase symbols.

(use-modules (srfi srfi-11)) ; let-values

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

(define (ly:string-generic-<? a b ci)
  ;; Don't use char-set:digit, it also contains Unicode digits from other
  ;; alphabets.
  (define char-set:latin-digit (char-set #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
  (define (digit-at? s i)
    (char-set-contains? char-set:latin-digit (string-ref s i)))
  (define (split-number s)
    (let loop ((i 0))
      (cond
       ((eqv? i (string-length s))
        (values (string->number s)
                ""))
       ((digit-at? s i)
        (loop (1+ i)))
       (else
        (values (string->number (substring s 0 i))
                (substring s i))))))
  (cond
   ((string-null? b)
    #f)
   ((string-null? a)
    #t)
   ((and (digit-at? a 0)
         (digit-at? b 0))
    (let-values (((num-a rest-a) (split-number a))
                 ((num-b rest-b) (split-number b)))
      (cond
       ((< num-a num-b)
        #t)
       ((> num-a num-b)
        #f)
       (else
        (ly:string-generic-<? rest-a rest-b ci)))))
   ((ly:char-generic-<? (string-ref a 0)
                        (string-ref b 0)
                        ci)
    #t)
   ((ly:char-generic-<? (string-ref b 0)
                        (string-ref a 0)
                        ci)
    #f)
   (else
    ;; This isn't quadratic as it may seem: Guile substrings are copy-on-write.
    (ly:string-generic-<? (substring a 1)
                          (substring b 1)
                          ci))))

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
