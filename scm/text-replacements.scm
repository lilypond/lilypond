;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2022 Jean Abou Samra <jean@abou-samra.fr>
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

;; The worker function for text replacements in markup.  The implementation
;; uses regular expressions to do the substitution, which is simple and
;; should be made fast by the regexp engine.

;; Before using the replacement alists, they are converted to hash tables
;; for efficiency.

(use-modules
 (ice-9 regex)
 ;; Our alist->hash-table should be called
 ;; alist->hashq-table, see the comment there.
 ((ice-9 hash-table)
  #:select ((alist->hash-table . real-alist->hash-table))))

;; These caches avoid regenerating the regexp and hash table in most cases.

(define (make-hashq-cached-function func)
  (let ((hashtab (make-hash-table)))
    (lambda (arg)
      (let ((handle (hashq-get-handle hashtab arg)))
        (if handle
            (cdr handle)
            (let ((new-val (func arg)))
              (hashq-set! hashtab arg new-val)
              new-val))))))

(define replacement-regexp
  (make-hashq-cached-function
   (lambda (replacements)
     (make-regexp
      (string-join (map regexp-quote (map car replacements))
                   "|")))))

(define replacement-hashtab
  (make-hashq-cached-function real-alist->hash-table))

(define-public (perform-text-replacements layout props str)
  (let ((replacements (chain-assoc-get 'replacement-alist props '())))
    (if (null? replacements)
        ;; We need to treat this specially, since "" would match an
        ;; empty string anywhere whereas we would have wanted a truly
        ;; empty alternative matching nothing.
        str
        (let ((pattern (replacement-regexp replacements))
              (hashtab (replacement-hashtab replacements)))
          (let loop ((str str)
                     (acc '()))
            (let ((match (regexp-exec pattern str)))
              (if match
                  (let* ((before (match:prefix match))
                         (matched (match:substring match))
                         ;; NB: it looks like this has quadratic complexity, but
                         ;; it doesn't.  Guile implements substrings as views on
                         ;; the original string until they need to be mutated,
                         ;; and this one won't ever need to be.  So, no copy is
                         ;; involved here.
                         (after (match:suffix match))
                         (replacement (hash-ref hashtab matched)))
                    (loop after
                          (cons* replacement before acc)))
                  (make-concat-markup (reverse! (cons str acc))))))))))
