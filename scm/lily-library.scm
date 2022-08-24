;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 1998--2022 Jan Nieuwenhuizen <janneke@gnu.org>
;;;; Han-Wen Nienhuys <hanwen@xs4all.nl>
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
 ;; for take, drop, take-while, list-index, and find-tail:
 (srfi srfi-1)
 (ice-9 pretty-print)
 (ice-9 match))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants.

(define-public X 0)
(define-public Y 1)
(define-public START -1)
(define-public STOP 1)
(define-public LEFT -1)
(define-public RIGHT 1)
(define-public UP 1)
(define-public DOWN -1)
(define-public CENTER 0)

(define-public DOUBLE-FLAT  -1)
(define-public THREE-Q-FLAT -3/4)
(define-public FLAT -1/2)
(define-public SEMI-FLAT -1/4)
(define-public NATURAL 0)
(define-public SEMI-SHARP 1/4)
(define-public SHARP 1/2)
(define-public THREE-Q-SHARP 3/4)
(define-public DOUBLE-SHARP 1)
(define-public SEMI-TONE 1/2)
(define-public FIVE-HALF-FLAT -5/2)
(define-public SEVEN-HALF-FLAT -7/2)
(define-public FIVE-HALF-SHARP 5/2)
(define-public SEVEN-HALF-SHARP 7/2)

(define-public INFINITY-INT 1000000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; moments

(define-public INF-MOMENT (ly:make-moment +inf.0))
(define-public ZERO-MOMENT (ly:make-moment 0 1))

(define-public (moment-min a b)
  (if (ly:moment<? a b) a b))

(define-public (moment<=? a b)
  (or (equal? a b)
      (ly:moment<? a b)))

(define-public (fraction->moment fraction)
  (if (null? fraction)
      ZERO-MOMENT
      (ly:make-moment (car fraction) (cdr fraction))))

(define-public (moment->fraction moment)
  (cons (ly:moment-main-numerator moment)
        (ly:moment-main-denominator moment)))

(define-public (seconds->moment s context)
  "Return a moment equivalent to @var{s}@tie{}seconds at the current tempo."
  (ly:moment-mul (ly:context-property context 'tempoWholesPerMinute)
                 (ly:make-moment (/ s 60))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; durations

;; Do not confuse this with (ly:make-duration 0 0), which is the
;; duration of a whole note.
(define-public ZERO-DURATION (ly:make-duration 0 0 0))

(define-public (duration-log-factor lognum)
  "Given a logarithmic duration number, return the length of the duration,
as a number of whole notes."
  (or (and (exact? lognum) (integer? lognum))
      (scm-error 'wrong-type-arg "duration-log-factor" "Not an integer: ~S" (list lognum) #f))
  (if (<= lognum 0)
      (ash 1 (- lognum))
      (/ (ash 1 lognum))))

(define-public (duration-dot-factor dotcount)
  "Given a count of the dots used to extend a musical duration, return
the numeric factor by which they increase the duration."
  (or (and (exact? dotcount) (integer? dotcount) (>= dotcount 0))
      (scm-error 'wrong-type-arg "duration-dot-factor" "Not a count: ~S" (list dotcount) #f))
  (- 2 (/ (ash 1 dotcount))))

(define-public (duration-length dur)
  "Return the overall length of a duration, as a number of whole
notes.  (Not to be confused with @code{ly:duration-length}, which returns a
less useful @code{Moment} object.)"
  (ly:moment-main (ly:duration-length dur)))

(define-public (duration-visual dur)
  "Given a duration object, return the visual part of the duration (base
note length and dot count), in the form of a duration object with
non-visual scale factor 1."
  (ly:make-duration (ly:duration-log dur) (ly:duration-dot-count dur) 1))

(define-public (duration-visual-length dur)
  "Given a duration object, return the length of the visual part of the
duration (base note length and dot count), as a number of whole notes."
  (duration-length (duration-visual dur)))

(define-public (unity-if-multimeasure context dur)
  "Given a context and a duration, return @code{1} if the duration is
longer than the @code{measureLength} in that context, and @code{#f} otherwise.
This supports historic use of @code{Completion_heads_engraver} to split
@code{c1*3} into three whole notes."
  (if (ly:moment<? (ly:context-property context 'measureLength)
                   (ly:duration-length dur))
      1
      #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; arithmetic
(define-public (average x . lst)
  (/ (+ x (apply + lst)) (1+ (length lst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parser <-> output hooks.

(define-public (collect-bookpart-for-book book-part)
  "Top-level book-part handler."
  (define (add-bookpart book-part)
    (ly:parser-define! 'toplevel-bookparts
                       (cons book-part (ly:parser-lookup 'toplevel-bookparts))))
  ;; If top-level scores have been found before this \bookpart,
  ;; add them first to a dedicated bookpart
  (if (pair? (ly:parser-lookup 'toplevel-scores))
      (begin
        (add-bookpart (ly:make-book-part
                       (ly:parser-lookup 'toplevel-scores)))
        (ly:parser-define! 'toplevel-scores (list))))
  (add-bookpart book-part))

(define-public (collect-scores-for-book score)
  (ly:parser-define! 'toplevel-scores
                     (cons score (ly:parser-lookup 'toplevel-scores))))

(define-public (collect-music-aux score-handler music)
  "Pass @var{music} to @var{score-handler}, with preprocessing
for page layout instructions. "
  (define (music-property symbol)
    (ly:music-property music symbol #f))
  (cond ((music-property 'page-marker)
         ;; a page marker: set page break/turn permissions or label
         (let ((label (music-property 'page-label)))
           (if (symbol? label)
               (score-handler (ly:make-page-label-marker label))))
         (for-each (lambda (symbol)
                     (let ((permission (music-property symbol)))
                       (if (symbol? permission)
                           (score-handler
                            (ly:make-page-permission-marker symbol
                                                            (if (eq? 'forbid permission)
                                                                '()
                                                                permission))))))
                   '(line-break-permission page-break-permission
                                           page-turn-permission)))
        ((not (music-property 'void))
         ;; a regular music expression: make a score with this music
         ;; void music is discarded
         (score-handler (scorify-music music)))))

(define-public (collect-music-for-book music)
  "Top-level music handler."
  (collect-music-aux (lambda (score)
                       (collect-scores-for-book score))
                     music))

(define-public (collect-book-music-for-book book music)
  "Book music handler."
  (collect-music-aux (lambda (score)
                       (ly:book-add-score! book score))
                     music))

(define-public (scorify-music music)
  "Preprocess @var{music}."
  (ly:make-score
   (fold (lambda (f m) (f m))
         music
         toplevel-music-functions)))

(define (get-current-filename book)
  "return any suffix value for output filename allowing for settings by
calls to bookOutputName function"
  (or (paper-variable book 'output-filename)
      (ly:parser-output-name)))

(define (get-current-suffix book)
  "return any suffix value for output filename allowing for settings by calls to
bookoutput function"
  (let ((book-output-suffix (paper-variable book 'output-suffix)))
    (if (not (string? book-output-suffix))
        (ly:parser-lookup 'output-suffix)
        book-output-suffix)))

(define-public current-outfile-name #f)  ; for use by regression tests

(define (get-outfile-name book)
  "return current filename for generating backend output files"
  ;; user can now override the base file name, so we have to use
  ;; the file-name concatenated with any potential output-suffix value
  ;; as the key to out internal a-list
  (let* ((base-name (get-current-filename book))
         (output-suffix (get-current-suffix book))
         (alist-key (format #f "~a~a" base-name output-suffix))
         (counter-alist (ly:parser-lookup 'counter-alist))
         (output-count (assoc-get alist-key counter-alist 0))
         (result base-name))
    ;; Allow all ASCII alphanumerics, including accents
    (if (string? output-suffix)
        (set! result
              (format #f "~a-~a"
                      result
                      (string-regexp-substitute
                       "[^-[:alnum:]]"
                       "_"
                       output-suffix))))

    ;; assoc-get call will always have returned a number
    (if (> output-count 0)
        (set! result (format #f "~a-~a" result output-count)))

    (ly:parser-define! 'counter-alist
                       (assoc-set! counter-alist alist-key (1+ output-count)))
    (set! current-outfile-name result)
    result))

(define (print-book-with book process-procedure)
  (let* ((paper (ly:parser-lookup '$defaultpaper))
         (layout (ly:parser-lookup '$defaultlayout))
         (outfile-name (get-outfile-name book)))
    (process-procedure book paper layout outfile-name)))

(define-public (print-book-with-defaults book)
  (print-book-with book ly:book-process))

(define-public (print-book-with-defaults-as-systems book)
  (print-book-with book ly:book-process-to-systems))

;; Add a score to the current bookpart, book or top-level.
(define-public (add-score score)
  (cond
   ((ly:parser-lookup '$current-bookpart)
    ((ly:parser-lookup 'bookpart-score-handler)
     (ly:parser-lookup '$current-bookpart) score))
   ((ly:parser-lookup '$current-book)
    ((ly:parser-lookup 'book-score-handler)
     (ly:parser-lookup '$current-book) score))
   (else
    ((ly:parser-lookup 'toplevel-score-handler) score))))

(define-public paper-variable
  (let
      ((get-papers
        (lambda (book)
          (append (if (and book (ly:output-def? (ly:book-paper book)))
                      (list (ly:book-paper book))
                      '())
                  (ly:parser-lookup '$papers)
                  (list (ly:parser-lookup '$defaultpaper))))))
    (make-procedure-with-setter
     (lambda (book symbol)
       (any (lambda (p) (ly:output-def-lookup p symbol #f))
            (get-papers book)))
     (lambda (book symbol value)
       (ly:output-def-set-variable!
        (car (get-papers book))
        symbol value)))))

(define-public (add-text text)
  (add-score (list text)))

(define-public (add-music music)
  (collect-music-aux (lambda (score)
                       (add-score score))
                     music))

(define-public (context-mod-from-music music)
  (let ((warn #t) (mods (ly:make-context-mod)))
    (let loop ((m music))
      (if (music-is-of-type? m 'layout-instruction-event)
          (let ((symbol (ly:music-property m 'symbol)))
            (ly:add-context-mod
             mods
             (case (ly:music-property m 'name)
               ((PropertySet)
                (list 'assign
                      symbol
                      (ly:music-property m 'value)))
               ((PropertyUnset)
                (list 'unset symbol))
               ((OverrideProperty)
                (cons* 'push
                       symbol
                       (ly:music-property m 'grob-value)
                       (cond
                        ((ly:music-property m 'grob-property #f) => list)
                        (else
                         (ly:music-property m 'grob-property-path)))))
               ((RevertProperty)
                (cons* 'pop
                       symbol
                       (cond
                        ((ly:music-property m 'grob-property #f) => list)
                        (else
                         (ly:music-property m 'grob-property-path))))))))
          (case (ly:music-property m 'name)
            ((ApplyContext)
             (ly:add-context-mod mods
                                 (list 'apply
                                       (ly:music-property m 'procedure))))
            ((ContextSpeccedMusic)
             (loop (ly:music-property m 'element)))
            (else
             (let ((callback (ly:music-property m 'elements-callback)))
               (if (procedure? callback)
                   (for-each loop (callback m))
                   (if (and warn (ly:duration? (ly:music-property m 'duration)))
                       (begin
                         (ly:music-warning
                          music
                          (G_ "Music unsuitable for context-mod"))
                         (set! warn #f)))))))))
    mods))

(define-public (context-defs-from-music output-def music)
  (define (checkmods mods)
    (or mods
        (begin
          (ly:music-warning music (G_ "Cannot determine contexts to modify"))
          (ly:make-context-mod))))
  (let ((warn #t))
    (let loop ((m music) (mods #f))
      ;; The parser turns all sets, overrides etc into something
      ;; wrapped in ContextSpeccedMusic which is how we determine the
      ;; contexts to affect.  A set, override etc that is not wrapped
      ;; in ContextSpeccedMusic is likely Scheme-generated and cannot
      ;; really be assigned usefully.  `checkmods' reports this
      ;; problem when it occurs.
      (if (music-is-of-type? m 'layout-instruction-event)
          (ly:add-context-mod
           (checkmods mods)
           (case (ly:music-property m 'name)
             ((PropertySet)
              (list 'assign
                    (ly:music-property m 'symbol)
                    (ly:music-property m 'value)))
             ((PropertyUnset)
              (list 'unset
                    (ly:music-property m 'symbol)))
             ((OverrideProperty)
              (cons* 'push
                     (ly:music-property m 'symbol)
                     (ly:music-property m 'grob-value)
                     (cond
                      ((ly:music-property m 'grob-property #f) => list)
                      (else
                       (ly:music-property m 'grob-property-path)))))
             ((RevertProperty)
              (cons* 'pop
                     (ly:music-property m 'symbol)
                     (cond
                      ((ly:music-property m 'grob-property #f) => list)
                      (else
                       (ly:music-property m 'grob-property-path)))))))
          (case (ly:music-property m 'name)
            ((ApplyContext)
             (ly:add-context-mod (checkmods mods)
                                 (list 'apply
                                       (ly:music-property m 'procedure))))
            ((ContextSpeccedMusic)
             ;; Use let* here to let defs catch up with modifications
             ;; to the context defs made in the recursion
             (let* ((mods (loop (ly:music-property m 'element)
                                (ly:make-context-mod)))
                    (defs (ly:output-find-context-def
                           output-def (ly:music-property m 'context-type))))
               (if (null? defs)
                   (ly:music-warning
                    music
                    (format #f (G_ "Cannot find context-def \\~a")
                            (ly:music-property m 'context-type)))
                   (for-each
                    (lambda (entry)
                      (ly:output-def-set-variable!
                       output-def (car entry)
                       (ly:context-def-modify (cdr entry) mods)))
                    defs))))
            (else
             (let ((callback (ly:music-property m 'elements-callback)))
               (if (procedure? callback)
                   (fold loop mods (callback m))
                   (if (and warn (ly:duration? (ly:music-property m 'duration)))
                       (begin
                         (ly:music-warning
                          music
                          (G_ "Music unsuitable for output-def"))
                         (set! warn #f))))))))
      mods)))


;;;;;;;;;;;;;;;;
;; alist

(define-public assoc-get ly:assoc-get)

(define-public chain-assoc-get ly:chain-assoc-get)

(define-public (uniqued-alist alist acc)
  (if (null? alist) acc
      (if (assoc (caar alist) acc)
          (uniqued-alist (cdr alist) acc)
          (uniqued-alist (cdr alist) (cons (car alist) acc)))))

(define-public (alist<? x y)
  (string<? (symbol->string (car x))
            (symbol->string (car y))))

(define (map-alist-vals func list)
  "map FUNC over the vals of LIST, leaving the keys."
  (if (null?  list)
      '()
      (cons (cons  (caar list) (func (cdar list)))
            (map-alist-vals func (cdr list)))))

(define (map-alist-keys func list)
  "map FUNC over the keys of an alist LIST, leaving the vals."
  (if (null?  list)
      '()
      (cons (cons (func (caar list)) (cdar list))
            (map-alist-keys func (cdr list)))))

(define-public (first-member members lst)
  "Return first successful member (of member) from @var{members} in
@var{lst}."
  (any (lambda (m) (member m lst)) members))

(define-public (first-assoc keys lst)
  "Return first successful assoc of key from @var{keys} in @var{lst}."
  (any (lambda (k) (assoc k lst)) keys))

(define-public (flatten-alist alist)
  (if (null? alist)
      '()
      (cons (caar alist)
            (cons (cdar alist)
                  (flatten-alist (cdr alist))))))

(define-public (map-selected-alist-keys function keys alist)
  "Return @var{alist} with @var{function} applied to all of the values
in list @var{keys}.  Example:

@example
(map-selected-alist-keys - '(a b) '((a . 1) (b . -2) (c . 3) (d . 4)))
   @result{} ((a . -1) (b . 2) (c . 3) (d . 4))
@end example"
  (define (map-selected-alist-keys-helper key alist)
    (map
     (lambda (pair)
       (if (equal? key (car pair))
           (cons key (function (cdr pair)))
           pair))
     alist))
  (fold map-selected-alist-keys-helper alist keys))

;;;;;;;;;;;;;;;;
;; vector

(define-public (vector-for-each proc vec)
  (do
      ((i 0 (1+ i)))
      ((>= i (vector-length vec)) vec)
    (vector-set! vec i (proc (vector-ref vec i)))))

;;;;;;;;;;;;;;;;
;; hash

(define-public (hash-table->alist t)
  (hash-fold acons '() t))

;; UGH! Guile provides a module (ice-9 hash-table), containing an
;; alist->hash-table function, but it's not the same one! Guile is consistent in
;; its naming Scheme, providing alist->hashq-table, alist->hashv-table and
;; alist->hash-table.  This alist->hash-table is actually its
;; alist->hashq-table. Meaning that we can't easily get rid of this definition
;; and import (ice-9 hash-table), because this alist->hash-table is used in user
;; scores, and hashq-ref and friends give inconsistent results on an alist
;; created with alist->hash-table: it would be an ugly backwards compatibility
;; leaving users with files that compile but strangely give wrong results. So
;; we're stuck with a function that will be confusing forever. --JeanAS
(define-public (alist->hash-table lst)
  "Convert alist @var{lst} to a table.

@strong{Warning:} The resulting hash table is hashed by identity.
This actually corresponds to the @code{alist->hashq-table} function
of Guile's @code{(ice-9 hash-table)} module, @strong{not}
@code{alist->hash-table}."
  (let ((m (make-hash-table (length lst))))
    ;; first association wins.
    (for-each (lambda (k-v) (hashq-create-handle! m (car k-v) (cdr k-v))) lst)
    m))

;;;;;;;;;;;;;;;;
;; list

(define (split-list lst n)
  "Split LST in N equal sized parts"

  (define (helper todo acc-vector k)
    (if (null? todo)
        acc-vector
        (begin
          (if (< k 0)
              (set! k (+ n k)))

          (vector-set! acc-vector k (cons (car todo) (vector-ref acc-vector k)))
          (helper (cdr todo) acc-vector (1- k)))))

  (helper lst (make-vector n '()) (1- n)))

(define (list-element-index lst x)
  (list-index (lambda (m) (equal? m x)) lst))

(define-public (count-list lst)
  "Given @var{lst} as @code{(E1 E2 .. )}, return
@code{((E1 . 1) (E2 . 2) ... )}."
  (map cons lst (iota (length lst) 1)))

(define-public (list-join lst intermediate)
  "Put @var{intermediate} between all elements of @var{lst}."

  (fold-right
   (lambda (elem prev)
     (if (pair? prev)
         (cons  elem (cons intermediate prev))
         (list elem)))
   '() lst))

(define-public (flatten-list x)
  "Unnest list."
  (let loop ((x x) (tail '()))
    (cond ((list? x) (fold-right loop tail x))
          ((not (pair? x)) (cons x tail))
          (else (loop (car x) (loop (cdr x) tail))))))

(define-public (uniq-list lst)
  "Remove doublets from list @var{lst} (i.e., make its elements unique),
assuming that it is sorted.  Uses @code{equal?} for comparisons."
  (reverse!
   (fold (lambda (x acc)
           (if (null? acc)
               (list x)
               (if (equal? x (car acc))
                   acc
                   (cons x acc))))
         '() lst) '()))

(define (split-at-predicate pred lst)
  "Split LST into two lists at the first element that returns #f for
  (PRED previous_element element).  Return the two parts as a pair.
  Example: (split-at-predicate < '(1 2 3 2 1)) ==> ((1 2 3) . (2 1))"
  (let ((i (and (pair? lst)
                (list-index (lambda (x y) (not (pred x y)))
                            lst
                            (cdr lst)))))
    (if i
        (call-with-values
            (lambda () (split-at lst (1+ i)))
          cons)
        (list lst))))

(define-public (split-list-by-separator lst pred)
  "Split @var{lst} at each element that satisfies @var{pred}, and return
the parts (with the separators removed) as a list of lists.  Example:

@example
(split-list-by-separator '(a 0 b c 1 d) number?)
   @result{} ((a) (b c) (d))
@end example"
  (call-with-values (lambda () (break pred lst))
    (lambda (head tail)
      (cons head
            (if (null? tail)
                tail
                (split-list-by-separator (cdr tail) pred))))))

(define-public (offset-add a b)
  (cons (+ (car a) (car b))
        (+ (cdr a) (cdr b))))

(define-public (offset-flip-y o)
  (cons (car o) (- (cdr o))))

(define-public (offset-scale o scale)
  (cons (* (car o) scale)
        (* (cdr o) scale)))

(define-public (ly:list->offsets accum coords)
  (if (null? coords)
      accum
      (cons (cons (car coords) (cadr coords))
            (ly:list->offsets accum (cddr coords)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pairs

(define (car-or-identity x)
  (if (pair? x)
      (car x)
      x))

(define (cdr-or-identity x)
  (if (pair? x)
      (cdr x)
      x))

(define-public (ordered-cons a b)
  (cons (min a b)
        (max a b)))

(define (pair-map procs arg1 . rest)
  "Apply the procedures in the @code{car} and @code{cdr} of @var{procs}
to the corresponding elements of the pair @var{arg1} and return the
results in a pair.  Additional pair arguments are accepted as for
@code{map}.  Any argument @var{x} which is not a pair is treated as
@code{(@var{x} . @var{x})}."
  (cons
   (apply (car-or-identity procs) (map car-or-identity (cons arg1 rest)))
   (apply (cdr-or-identity procs) (map cdr-or-identity (cons arg1 rest)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; intervals

(define-public empty-interval '(+inf.0 . -inf.0))

(define-public (symmetric-interval expr)
  (cons (- expr) expr))

(define-public (interval-length x)
  "Length of the number pair @var{x}, if an interval."
  (max 0 (- (cdr x) (car x))))

(define-public (interval-bound interval dir)
  ((if (= dir RIGHT) cdr car) interval))

(define-public (interval-index interval dir)
  "Interpolate @var{interval} between between left (@var{dir}=@code{-1}) and
right (@var{dir}=@code{+1})."

  (* (+  (interval-start interval) (interval-end interval)
         (* dir (- (interval-end interval) (interval-start interval))))
     0.5))

(define-public (interval-center x)
  "Center the number pair @var{x}, if an interval."
  (if (interval-empty? x)
      0.0
      (/ (+ (car x) (cdr x)) 2)))

(define-public interval-start car)

(define-public interval-end cdr)

(define-public (interval-scale iv factor)
  (cons (* (car iv) factor)
        (* (cdr iv) factor)))

(define-public (interval-widen iv amount)
  (cons (- (car iv) amount)
        (+ (cdr iv) amount)))

(define-public (interval-empty? iv)
  (> (car iv) (cdr iv)))

(define-public (interval-contains? iv x)
  ;; TODO: This deals with a point x.  We could also deal with an interval.
  (and (<= (car iv) x) (<= x (cdr iv))))

(define-public (interval-union i1 i2)
  (cons
   (min (car i1) (car i2))
   (max (cdr i1) (cdr i2))))

(define-public (interval-intersection i1 i2)
  (cons
   (max (car i1) (car i2))
   (min (cdr i1) (cdr i2))))

(define-public (interval-sane? i)
  (not (or  (nan? (car i))
            (inf? (car i))
            (nan? (cdr i))
            (inf? (cdr i))
            (> (car i) (cdr i)))))

(define-public (add-point interval p)
  (cons (min (interval-start interval) p)
        (max (interval-end interval) p)))

(define-public (reverse-interval iv)
  (cons (cdr iv) (car iv)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; coordinates

(define-public (other-axis a)
  (remainder (+ a 1) 2))

(define coord-x car)
(define coord-y cdr)

(define (coord-axis coords axis)
  ((if (eqv? axis X) car cdr) coords))

(define-public (coord-translate coordinate amount)
  (pair-map + amount coordinate))

(define-public (coord-scale coordinate amount)
  (pair-map * amount coordinate))

(define-public (coord-rotate coordinate angle-in-radians)
  (coord-rotated coordinate (/ angle-in-radians PI-OVER-180)))

(define-public (coord-rotated coordinate direction)
  ;; Same, in degrees or with a given direction
  (let ((dir (ly:directed direction)))
    (cons (- (* (car dir) (car coordinate))
             (* (cdr dir) (cdr coordinate)))
          (+ (* (car dir) (cdr coordinate))
             (* (cdr dir) (car coordinate))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; trig

(define-public PI (* 4 (atan 1)))

(define-public TWO-PI (* 2 PI))

(define-public PI-OVER-TWO (/ PI 2))

(define-public THREE-PI-OVER-TWO (* 3 PI-OVER-TWO))

(define-public (cyclic-base-value value cycle)
  "Take @var{value} (for example, an angle) and modulo-maps it between 0
and base @var{cycle}."
  (cond ((< value 0)
         (cyclic-base-value (+ value cycle) cycle))
        ((>= value cycle)
         (cyclic-base-value (- value cycle) cycle))
        (else value)))

(define-public (angle-0-2pi angle)
  "Take @var{angle} (in radians) and map it between 0 and 2pi."
  (cyclic-base-value angle TWO-PI))

(define-public (angle-0-360 angle)
  "Take @var{angle} (in degrees) and map it between 0 and 360 degrees."
  (cyclic-base-value angle 360.0))

(define-public PI-OVER-180  (/ PI 180))

(define-public (degrees->radians angle-degrees)
  "Convert the given angle from degrees to radians."
  (* angle-degrees PI-OVER-180))

(define-public (ellipse-radius x-radius y-radius angle)
  (/
   (* x-radius y-radius)
   (sqrt
    (+ (* (expt y-radius 2)
          (* (cos angle) (cos angle)))
       (* (expt x-radius 2)
          (* (sin angle) (sin angle)))))))

(define-public (polar->rectangular radius angle-in-degrees)
  "Return polar coordinates (@var{radius}, @var{angle-in-degrees})
as rectangular coordinates @code{(x-length . y-length)}."
  (ly:directed angle-in-degrees radius))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string

(define-public (string-endswith s suffix)
  (equal? suffix (substring/shared s
                                   (max 0 (- (string-length s) (string-length suffix)))
                                   (string-length s))))

(define-public (string-startswith s prefix)
  (equal? prefix (substring/shared s 0 (min (string-length s) (string-length prefix)))))

(define-public (remove-whitespace strg)
  "Remove characters satisfying @code{char-whitespace?} from string
@var{strg}."
  (string-delete char-whitespace? strg))

(define-public (string-encode-integer i)
  (cond
   ((= i  0) "o")
   ((< i 0)   (string-append "n" (string-encode-integer (- i))))
   (else (string-append
          (make-string 1 (integer->char (+ 65 (modulo i 26))))
          (string-encode-integer (quotient i 26))))))

(define (number->octal-string x)
  (let* ((n (inexact->exact x))
         (n64 (quotient n 64))
         (n8 (quotient (- n (* n64 64)) 8)))
    (string-append
     (number->string n64)
     (number->string n8)
     (number->string (remainder (- n (+ (* n64 64) (* n8 8))) 8)))))

(define-public (ly:number-pair->string c)
  (string-append (ly:number->string (car c)) " "
                 (ly:number->string (cdr c))))

(define-public (dir-basename file . rest)
  "Strip suffixes in @var{rest}, but leave directory component for
@var{file}."
  (define (inverse-basename x y) (basename y x))
  (simple-format #f "~a/~a" (dirname file)
                 (fold inverse-basename file rest)))

(define-public (write-me message x)
  "Return @var{x}.  Display @var{message} and write @var{x}.
Handy for debugging, possibly turned off."
  (display message) (write x) (newline) x)
;;  x)

(define-public (stderr string . rest)
  (apply format (current-error-port) string rest)
  (force-output (current-error-port)))

(define-public (debugf string . rest)
  (if #f
      (apply stderr string rest)))

(define (index-cell cell dir)
  (if (equal? dir 1)
      (cdr cell)
      (car cell)))

(define-public (list-insert-separator lst between)
  "Create new list, inserting @var{between} between elements of @var{lst}."
  (define (conc x y )
    (if (eq? y #f)
        (list x)
        (cons x  (cons between y))))
  (fold-right conc #f lst))

(define-public (string-regexp-substitute a b str)
  (regexp-substitute/global #f a str 'pre b 'post))

(define (regexp-split str regex)
  (define matches '())
  (define end-of-prev-match 0)
  (define (notice match)

    (set! matches (cons (substring (match:string match)
                                   end-of-prev-match
                                   (match:start match))
                        matches))
    (set! end-of-prev-match (match:end match)))

  (regexp-substitute/global #f regex str notice 'post)

  (if (< end-of-prev-match (string-length str))
      (set!
       matches
       (cons (substring str end-of-prev-match (string-length str)) matches)))

  (reverse matches))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; numbering styles

(define-public (number-format number-type num . custom-format)
  "Print @var{num} according to the requested @var{number-type}.
Choices include @code{arabic}, @code{custom}, @code{roman-ij-lower},
@code{roman-ij-upper}, @code{roman-lower} (the default), and
@code{roman-upper}.

For @code{custom}, @var{custom-format} must be present; it gets
applied to @var{num}."
  ;; Be foolproof: avoid an error if trying to format zero or a
  ;; negative number in roman numbers; use arabic numbers in that
  ;; case.
  (case number-type
    ((roman-lower)
     (if (positive? num)
         (ice9-format #f "~(~@r~)" num)
         (ice9-format #f "~d" num)))
    ((roman-upper)
     (if (positive? num)
         (ice9-format #f "~@r" num)
         (ice9-format #f "~d" num)))
    ((arabic)
     (ice9-format #f "~d" num))
    ;; Too bad that we can't make this work with out-of-range numbers and such.
    ;; Guile prints information about the error before raising it.  Maybe we
    ;; should change from accepting 'custom plus a format string to accepting a
    ;; procedure that does the work itself?
    ((custom)
     (ice9-format #f (car custom-format) num))
    ((roman-ij-lower)
     (if (positive? num)
         (let* ((text (ice9-format #f "~(~@r~)" num))
                (text (string-regexp-substitute "i$" "j" text))
                (text (string-regexp-substitute
                       "ij$" (ly:wide-char->utf-8 #x0133) text))) ; ij ligature
           text)
         (ice9-format #f "~d" num)))
    ((roman-ij-upper)
     (if (positive? num)
         (let* ((text (ice9-format #f "~@r" num))
                (text (string-regexp-substitute "I$" "J" text))
                (text (string-regexp-substitute
                       "IJ$" (ly:wide-char->utf-8 #x0132) text))) ; IJ ligature
           text)
         (ice9-format #f "~d" num)))
    (else
     (if (positive? num)
         (ice9-format #f "~(~@r~)" num)
         (ice9-format #f "~d" num)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lilypond version

(define (lexicographic-list-compare? op a b)
  "Lexicographically compare two lists @var{a} and @var{b} using
   the operator @var{op}. The types of the list elements have to
   be comparable with @var{op}. If the lists are of different length
   the trailing elements of the longer list are ignored."
  (let* ((ca (car a))
         (iseql (op ca ca)))
    (let loop ((ca ca) (cb (car b)) (a (cdr a)) (b (cdr b)))
      (let ((axb (op ca cb)))
        (if (and (pair? a) (pair? b)
                 (eq? axb iseql (op cb ca)))
            (loop (car a) (car b) (cdr a) (cdr b))
            axb)))))

(define-public (ly:version? op ver)
  "Use operator @var{op} to compare the currently executed LilyPond
version with a given version @var{ver}, which is passed as a list of
numbers."
  (lexicographic-list-compare? op (ly:version) ver))

(define (parse-lily-version str)
  (let* ((warned #f)
         (output
          (match (string-split str #\.)
            ((major minor patch my-patch)
             (list (string->number major)
                   (string->number minor)
                   (string->number patch)
                   (string->symbol my-patch)))
            ((major minor patch)
             (list (string->number major)
                   (string->number minor)
                   (string->number patch)))
            ((major minor)
             ;; Accept \version "x.y" without third component, but only
             ;; if y is even, i.e. it's a stable release series.  Within
             ;; a development release series, syntax changes with point
             ;; releases so convert-ly needs the specific one.
             (let ((major-int (string->number major))
                   (minor-int (string->number minor)))
               (cond
                ((not (and major-int minor-int))
                 #f)
                ((odd? minor-int)
                 (ly:non-fatal-error (G_ "version with third number omitted is only allowed for stable releases \
(when the second number is even)"))
                 (set! warned #t)
                 #f)
                (else
                 (list major-int minor-int)))))
            (else #f))))
    (cond
     (warned
      #f)
     ((or (not output)
          ;; Are the number components actually numbers?
          (not (every identity output)))
      (ly:non-fatal-error (G_ "Invalid version string \"~a\"")
                          str)
      #f)
     (else output))))

(define (parse-and-check-version str)
  "Parse the given version string and check its validity.  If valid,
return the parsed version as a list, else return @code{#f}.  If the
version is greater than the running version of LilyPond, a warning is
emitted and the version is considered invalid."
  (let ((version (parse-lily-version str)))
    (cond
     ((not version)
      #f)
     ((ly:version? < version)
      (ly:non-fatal-error (G_ "program too old: ~a (file requires: ~a)")
                          (lilypond-version)
                          str)
      ;; Must consider invalid so that the lexer will emit the error
      ;; token necessary to set a nonzero exit code.  The usage for
      ;; the version returned is checking if it's too old (to suggest
      ;; convert-ly), so not recording it if it's too new is no
      ;; problem.
      #f)
     (else
      version))))

(define convert-ly-msg
  (match (ly:version)
    ((major minor . rest)
     (format #f
             (G_ "\

Note: compilation failed and \\version outdated, did you
update input syntax with convert-ly?

  https://lilypond.org/doc/v~a.~a/Documentation/usage/updating-files-with-convert_002dly

")
             major
             minor))))

;; Always used with (ly:version) for lilypond-version, but it's
;; an argument for the sake of regression testing.
(define-public (lilypond-version-outdated? file-version lily-version)
  "Is @var{file-version} outdated compared to @var{lily-version}?
This is defined as a version that is from a lower release
series (corresponding to the first two numbers of the version) or a
version from the same @emph{unstable} release series (odd minor
version number) with a lower patch level (third number).  A stable
version from the same series does not count as outdated because
compatibility is preserved."
  (match
      lily-version
    ((major minor patch . _)
     (match
         file-version
       ((file-major file-minor . rest)
        (or (> major file-major)
            (and (eqv? major file-major)
                 (or (> minor file-minor)
                     (and (eqv? minor file-minor)
                          (odd? minor)
                          (match rest
                            (() #f)
                            ((file-patch . _) (> patch file-patch))))))))))))

(define-public (suggest-convert-ly-message version-seen)
  "Internally used when the file has an error, to suggest usage of
@code{convert-ly} if the @code{\\version} statement is considered
outdated compared to the LilyPond version that is running."
  ;; If version-seen is #t, it means that a \version statement
  ;; was found but the version could not be parsed.
  (if (and (not (eq? version-seen #t))
           (lilypond-version-outdated? version-seen (ly:version)))
      (ly:message convert-ly-msg)))

;;;;;;;;;;;;;;;;
;; broken spanner

;; TODO: maybe we should add in the following functions some sanity checks
;; for unbroken input spanners really being spanners that did
;; not need to be broken, characterized by null ly:spanner-broken-intos,
;; as opposed to original spanners that were broken?  Or maybe not? --JeanAS

(define-public (unbroken-spanner? spanner)
  "Is @var{spanner} unbroken?  A spanner has to be broken if it spans
more than one system, or if one of its bounds is on the limit of the
system.  This function returns @code{#f} on the clones, but @code{#t}
on the originals."
  (eq? spanner (ly:grob-original spanner)))

(define (check-broken-spanner unbroken-val siblings-condition? spanner)
  ;; Generic broken status checker: return `unbroken-val` if `spanner`
  ;; is unbroken, else call `siblings-condition?` on its siblings.
  (if (unbroken-spanner? spanner)
      unbroken-val
      (let ((siblings (ly:spanner-broken-into (ly:grob-original spanner))))
        (if (null? siblings)
            (begin
              ;; Should really not happen because at least `spanner`
              ;; will be in the siblings.
              (ly:programming-error "broken spanner ~a without siblings"
                                    (grob::name spanner))
              #f)
            (siblings-condition? siblings)))))

(define-public (first-broken-spanner? spanner)
  "Is @var{spanner} broken @emph{and} the first of its broken
siblings?  See also @code{unbroken-or-first-broken-spanner?}."
  (check-broken-spanner
   #f
   (lambda (siblings)
     (eq? spanner (first siblings)))
   spanner))

(define-public (middle-broken-spanner? spanner)
  "Is @var{spanner} broken and among the middle broken pieces (i.e.,
neither the first nor the last)?"
  (check-broken-spanner
   #f
   (lambda (siblings)
     (and (not (eq? spanner (first siblings)))
          (not (eq? spanner (last siblings)))))
   spanner))

;; FIXME: *end*-broken-spanner? vs. not-*last*-broken-spanner?
(define-public (end-broken-spanner? spanner)
  "Is @var{spanner} broken @emph{and} the last of its broken siblings?
See also @code{unbroken-or-last-broken-spanner?}."
  (check-broken-spanner
   #f
   (lambda (siblings)
     (eq? spanner (last siblings)))
   spanner))

(define-public (not-first-broken-spanner? spanner)
  "Is @var{spanner} broken @emph{and} not the first of its broken
siblings?  The name is read @qq{(not first) and broken}."
  (check-broken-spanner
   #f
   (lambda (siblings)
     (not (eq? spanner (first siblings))))
   spanner))

(define-public (not-last-broken-spanner? spanner)
  "Is @var{spanner} broken @emph{and} not the last of its broken
siblings?  The name is read @qq{(not last) and broken}."
  (check-broken-spanner
   #f
   (lambda (siblings)
     (not (eq? spanner (last siblings))))
   spanner))

(define-public (unbroken-or-first-broken-spanner? spanner)
  "Is @var{spanner} either unbroken or the first of its broken
siblings?"
  (check-broken-spanner
   #t
   (lambda (siblings)
     (eq? spanner (first siblings)))
   spanner))

(define-public (unbroken-or-last-broken-spanner? spanner)
  "Is @var{spanner} either unbroken or the last of its broken
siblings?"
  (check-broken-spanner
   #t
   (lambda (siblings)
     (eq? spanner (last siblings)))
   spanner))

;;;;;;;;;;;;;;;;
;; other

(define-public (sign x)
  (if (= x 0)
      0
      (if (< x 0) -1 1)))

(define-public (binary-search start end getter target-val)
  (_i "Find the index between @var{start} and @var{end} (an integer)
which produces the closest match to @var{target-val} if
applied to function @var{getter}.")
  (if (<= end start)
      start
      (let* ((compare (quotient (+ start end) 2))
             (get-val (getter compare)))
        (cond
         ((< target-val get-val)
          (set! end (1- compare)))
         ((< get-val target-val)
          (set! start (1+ compare))))
        (binary-search start end getter target-val))))

(define-public ((comparator-from-key key cmp) a b)
  "Return a comparator function that applies @var{key} to the two
elements and compares the results using @var{cmp}.  Especially
useful for sorting."
  (cmp (key a)
       (key b)))

(define-public car<
  (comparator-from-key car <))

(define-public car<=
  (comparator-from-key car <=))

(define-public symbol<?
  (comparator-from-key symbol->string string<?))

(define-public symbol-key<?
  (comparator-from-key car symbol<?))

(define-public (eval-carefully symbol module . default)
  "Check whether all symbols in expression @var{symbol} are reachable
in module @var{module}.  In that case evaluate, otherwise
print a warning and set an optional @var{default}."
  (let* ((unavailable? (lambda (sym)
                         (not (module-defined? module sym))))
         (sym-unavailable
          (filter
           unavailable?
           (filter symbol? (flatten-list symbol)))))
    (if (null? sym-unavailable)
        (eval symbol module)
        (let* ((def (and (pair? default) (car default))))
          (ly:programming-error
           "cannot evaluate ~S in module ~S, setting to ~S"
           (object->string symbol)
           (object->string module)
           (object->string def))
          def))))

(define (self-evaluating? x)
  (or (number? x) (string? x) (procedure? x) (boolean? x)))

(define (ly-type? x)
  (any (lambda (p) ((car p) x)) lilypond-exported-predicates))

(define-public (pretty-printable? val)
  (and (not (self-evaluating? val))
       (not (symbol? val))
       (not (hash-table? val))
       (not (ly-type? val))))

(define-public (scm->string val)
  (let* ((quote-style (if (string? val)
                          'double
                          (if (or (null? val) ; (ly-type? '()) => #t
                                  (and (not (self-evaluating? val))
                                       (not (vector? val))
                                       (not (hash-table? val))
                                       (not (ly-type? val))))
                              'single
                              'none)))
         ;; don't confuse users with #<procedure ...> syntax
         (str (if (and (procedure? val)
                       (symbol? (procedure-name val)))
                  (symbol->string (procedure-name val))
                  (call-with-output-string
                   (if (pretty-printable? val)
                       ;; property values in PDF hit margin after 64 columns
                       (lambda (port)
                         (pretty-print val port #:width (case quote-style
                                                          ((single) 63)
                                                          (else 64))))
                       (lambda (port) (display val port)))))))
    (case quote-style
      ((single) (string-append
                 "'"
                 (string-regexp-substitute "\n " "\n  " str)))
      ((double) (string-append "\"" str "\""))
      (else str))))

(define-public (!= lst r)
  (not (= lst r)))

(define-public lily-unit->bigpoint-factor
  (cond
   ((equal? (ly:unit) "mm") (/ 72.0 25.4))
   ((equal? (ly:unit) "pt") (/ 72.0 72.27))
   (else (ly:error (G_ "unknown unit: ~S") (ly:unit)))))

(define-public lily-unit->mm-factor
  (* 25.4 (/ lily-unit->bigpoint-factor 72)))

;;; FONT may be font smob, or pango font string...
(define-public (font-name-style font)
  (if (string? font)
      (string-downcase font)
      (let* ((font-name (ly:font-name font))
             (full-name (if font-name font-name (ly:font-file-name font))))
        (string-downcase full-name))))

(define-public (modified-font-metric-font-scaling font)
  (let* ((designsize (ly:font-design-size font))
         (magnification (* (ly:font-magnification font)))
         (scaling (* magnification designsize)))
    (debugf "scaling:~S\n" scaling)
    (debugf "magnification:~S\n" magnification)
    (debugf "design:~S\n" designsize)
    scaling))

(define-public (version-not-seen-message input-file-name)
  (ly:warning-located
   (format #f "~a:1" input-file-name)
   (G_ "no \\version statement found, please add~afor future compatibility")
   (format #f "\n\n\\version ~s\n\n" (lilypond-version))))

(define-public (output-module? module)
  "Return @code{#t} if @var{module} belongs to an output module
usually carrying context definitions (@code{\\midi} or
@code{\\layout})."
  (let ((kind (module-ref module 'output-def-kind #f)))
    (or (eq? kind 'midi)
        (eq? kind 'layout))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-engraver helper macro

(define (listener->once-listener listener)
  (let ((last-ev #f)
        (mom #f))
    (lambda (translator event)
      (let ((new-mom (ly:context-current-moment
                      (ly:translator-context translator))))
        (cond
         ;; First event in time step.
         ((not (equal? new-mom mom))
          (set! mom new-mom)
          (set! last-ev event)
          (listener translator event))
         ;; Event repeated; nothing to do.
         ((equal? event last-ev))
         (else
          ;; Sync with stream-event.cc.
          (let ((old-cls (car (ly:event-property last-ev 'class)))
                (new-cls (car (ly:event-property event 'class))))
            (ly:event-warning last-ev (G_ "conflict with event: `~a'") old-cls)
            (ly:event-warning event (G_ "discarding event: `~a'") new-cls))))))))

;; An alist pair of @code{(is-midi . #t)} specifies possible use as a
;; performer, @code{(is-layout . #t)} as an engraver.  If neither is
;; specified, engraver-only use is assumed.
(define-syntax make-translator-component
  (syntax-rules ()
    ;; Special case of listeners.
    ((_ ((name arg ... #:once) body ...))
     (cons 'name (listener->once-listener (lambda (arg ...) body ...))))
    ;; Example: ((process-music engraver) ...) => (lambda (engraver) ...)
    ((_ ((name . args) body ...))
     (cons 'name (lambda args body ...)))
    ;; Example: (listeners ...) => (list 'listeners ...)
    ((_ (name thing ...))
     (cons 'name (make-translator-internal thing ...)))
    ;; Examples: (is-midi . #t) => (cons 'is-midi #t)
    ;;           (process-music . func) => (cons 'process-music func)
    ((_ (name . value))
     (cons 'name value))))

(define-syntax-rule (make-translator-internal thing ...)
  (list (make-translator-component thing) ...))

(define-syntax-rule-public (make-engraver form ...)
  "Like @code{make-translator}, but create an engraver, i.e.,
the resulting translator is only run in layout output and ignored
in MIDI."
  (make-translator-internal form ... (is-layout . #t)))

(define-syntax-rule-public (make-performer form ...)
  "Like @code{make-translator}, but create a performer, i.e.,
the resulting translator is only run in MIDI and ignored in
layout output.  Scheme performers do not support acknowledgers
and @code{process-acknowledged}."
  (make-translator-internal form ... (is-midi . #t)))

(define-syntax-rule-public (make-translator form ...)
  "Helper macro for creating Scheme translators usable in
both @samp{\\midi} and @samp{\\layout}.

The usual form for a translator is an association list (or alist)
mapping symbols to either anonymous functions or to another such
alist.

@code{make-translator} accepts forms where the first element is either
an argument list starting with the respective symbol, followed by the
function body (comparable to the way @code{define} is used for
defining functions), or a single symbol followed by subordinate forms
in the same manner.  You can also just make an alist pair
literally (the @samp{car} is quoted automatically) as long as the
unevaluated @samp{cdr} is not a pair.  This is useful if you already
have defined your engraver functions separately.

Symbols mapping to a function would be @code{initialize},
@code{start-translation-timestep}, @code{pre-process-music},
@code{process-music}, @code{stop-translation-timestep}, and
@code{finalize}.  Symbols mapping to another alist specified in the
same manner are @code{listeners} with the subordinate symbols being
event classes.

A template for writing a translator with all methods is:

@example
(lambda (context)
  (let (local-variables ...)
    (make-translator
     ((initialize translator)
      ...)
     ((start-translation-timestep translator)
      ...)
     (listeners
      ((event-class-1 translator event)
       ...)
      ((event-class-2 translator event #:once)
       ...))
     ((process-music translator)
      ...)
     (acknowledgers
      ((grob-interface-1 translator grob source-translator)
       ...)
      ((grob-interface-2 translator grob source-translator)
       ...))
     ((process-acknowledged translator)
      ...)
     ((stop-translation-timestep translator)
      ...)
     ((finalize translator)
      ...))))
@end example

This can be used as the argument to @code{\\consists}.

For @code{listeners}, a special feature is available: the argument
list of a listener can be terminated with the keyword @code{#:once}.
This makes for a listener that is only ever triggered once per time
step.  If it receives several events in the same time step, it emits a
warning, except if they are all equal (where equality is checked
recursively, with @code{equal?})."
  (make-translator-internal form ... (is-layout . #t) (is-midi . #t)))
