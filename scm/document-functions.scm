;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 1998--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;;                 Jan Nieuwenhuizen <janneke@gnu.org>
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

(use-modules
 (ice-9 match)
 (ice-9 regex)
 (ice-9 session))

(define (dashify-underscores str)
  (regexp-substitute/global #f "_" str 'pre "-" 'post))

(define (format-c-header c-h)
  (regexp-substitute/global
   #f ","
   (regexp-substitute/global #f "(SCM|\\)|\\() *" (dashify-underscores c-h)
                             'pre "" 'post)
   'pre " " 'post))

;; If there is `::` in the function name, insert a breakpoint to avoid
;; overlong index entries that would otherwise stick out to the right
;; in the two-column output of the PDF documentation.  Alas, this
;; triggers a bug in `texi2html` 1.82; we thus have to temporarily
;; enclose `prettier-name` with `@code` until an upgrade to a recent
;; `texi2any` version.
(define (document-function name arguments doc-string is-macro)
  (let* ((cmd (if is-macro "defmac" "defun"))
         (str-name (symbol->string name))
         (prettier-name
          (regexp-substitute/global #f "::" str-name 'pre "::@/" 'post)))
    (format #f "@~a @code{~a} ~a\n~a\n@end ~a\n\n"
            cmd prettier-name arguments doc-string cmd)))

;; Map function names (as strings) to full documentation entries
;; including signature and doc-string.
(define all-primitive-function-docs-alist
  (hash-map->list
   (lambda (name header-doc-string-pair)
     (cons (symbol->string name)
           (document-function name
                              (format-c-header (car header-doc-string-pair))
                              (cdr header-doc-string-pair)
                              #f)))
   (ly:get-all-function-documentation)))

(define (format-scheme-signature proc)
  (let* ((signature (procedure-arguments proc))
         (required-args (assq-ref signature 'required))
         (optional-args (assq-ref signature 'optional))
         (keyword-args (assq-ref signature 'keyword))
         (rest-arg (assq-ref signature 'rest)))
    ;; Example formatted signature:
    ;; (define* (f arg1 arg2 #:optional opt1 opt2 #:key kw1 (kw2 default2) . rest)
    ;;   ...)
    ;; => f arg1 arg2 [opt1 [opt2]] #:kw1 kw1 #:kw2 kw2 rest ...
    ;; It would be nicer to use [#:kw2=default2] when there is a default,
    ;; but sadly, Guile doesn't seem to make it available (or Jean has not
    ;; found it).  We don't even know whether the keyword argument has a
    ;; default (and is thus optional) or not.
    (string-join
     (append
      (map symbol->string required-args)
      (match optional-args
        (()
         '())
        ((opts ... last-opt)
         (list
          (fold-right
           (lambda (next-arg args-so-far)
             (format #f "[~a ~a]" next-arg args-so-far))
           (format #f "[~a]" last-opt)
           opts))))
      (map (lambda (pair)
             (let ((kw (car pair)))
               (format #f "~a ~a"
                       kw
                       (keyword->symbol kw))))
           keyword-args)
      (if rest-arg
          (list
           (format #f "~a @dots{}" rest-arg))
          '()))
     " ")))


;; Same format as all-primitive-function-docs-alist.
(define all-scheme-function-docs-alist
  (let* ((module (resolve-module '(lily)))
         ;; Restrict to public functions.
         (iface (module-public-interface module))
         (alist (ly:module->alist iface)))
    (filter-map
     (lambda (entry)
       (let* ((name (car entry))
              (raw-value (cdr entry))
              (is-macro (macro? raw-value))
              (value (if is-macro
                         (macro-transformer raw-value)
                         raw-value)))
         (if (and (procedure? value)
                  ;; Exclude functions that are documented through the
                  ;; C++ infrastructure (all-primitive-function-docs-alist
                  ;; above).
                  (not (hashq-get-handle (ly:get-all-function-documentation)
                                         name))
                  ;; Exclude xxx-markup functions, they are documented in NR.
                  ;; Note that make-xxx-markup is excluded by the documentation
                  ;; check (those don't have doc-strings).
                  (not (or (markup-function? value)
                           (markup-list-function? value))))
             (let* ((doc-string (procedure-documentation value)))
               ;; Many functions are publicly defined but do not need
               ;; documentation, like all Scheme-defined callbacks,
               ;; which are often self-telling and present in individual
               ;; grobs' pages.  We only retain functions that have
               ;; doc-strings.  If your pet function doesn't appear in
               ;; the internals, please give it a doc-string!
               (if doc-string
                   (cons (symbol->string name)
                         (document-function name
                                            (if is-macro
                                                ;; Guile doesn't give us meaningful
                                                ;; signatures for macros.
                                                "@dots{}"
                                                (format-scheme-signature value))
                                            doc-string
                                            is-macro))
                   #f))
             #f)))
     alist)))

;; We want to sort without taking the ly: prefix into account.
(define (no-ly-prefix-key name-doc-alist-entry)
  (let* ((name (car name-doc-alist-entry)))
    (if (string-startswith name "ly:")
        (string-drop name 3)
        name)))

(define (all-functions-doc)
  (let* ((all-function-doc-alists
          (append
           all-primitive-function-docs-alist
           all-scheme-function-docs-alist))
         (sorted-alist
          (sort all-function-doc-alists
                (comparator-from-key no-ly-prefix-key ly:string-ci<?)))
         (formatted-docs
          (map cdr sorted-alist)))
    (make <texi-node>
      #:name "Scheme functions"
      #:desc "Functions exported by LilyPond."
      #:text
      (string-concatenate formatted-docs))))

;; (dump-node (all-functions-doc)  (current-output-port) 0 )
