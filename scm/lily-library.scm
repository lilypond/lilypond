;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 1998--2012 Jan Nieuwenhuizen <janneke@gnu.org>
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

;; for take, drop, take-while, list-index, and find-tail:
(use-modules (srfi srfi-1))

;; for define-safe-public when byte-compiling using Guile V2
(use-modules (scm safe-utility-defs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants.

(define-public X 0)
(define-public Y 1)
(define-safe-public START -1)
(define-safe-public STOP 1)
(define-public LEFT -1)
(define-public RIGHT 1)
(define-public UP 1)
(define-public DOWN -1)
(define-public CENTER 0)

(define-safe-public DOUBLE-FLAT-QTS -4)
(define-safe-public THREE-Q-FLAT-QTS -3)
(define-safe-public FLAT-QTS -2)
(define-safe-public SEMI-FLAT-QTS -1)
(define-safe-public NATURAL-QTS 0)
(define-safe-public SEMI-SHARP-QTS 1)
(define-safe-public SHARP-QTS 2)
(define-safe-public THREE-Q-SHARP-QTS 3)
(define-safe-public DOUBLE-SHARP-QTS 4)
(define-safe-public SEMI-TONE-QTS 2)

(define-safe-public DOUBLE-FLAT  -1)
(define-safe-public THREE-Q-FLAT -3/4)
(define-safe-public FLAT -1/2)
(define-safe-public SEMI-FLAT -1/4)
(define-safe-public NATURAL 0)
(define-safe-public SEMI-SHARP 1/4)
(define-safe-public SHARP 1/2)
(define-safe-public THREE-Q-SHARP 3/4)
(define-safe-public DOUBLE-SHARP 1)
(define-safe-public SEMI-TONE 1/2)

(define-safe-public INFINITY-INT 1000000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; moments

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; durations

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
notes.  (Not to be confused with ly:duration-length, which returns a
less-useful moment object.)"
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; arithmetic
(define-public (average x . lst)
  (/ (+ x (apply + lst)) (1+ (length lst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parser <-> output hooks.

(define-public (collect-bookpart-for-book parser book-part)
  "Toplevel book-part handler."
  (define (add-bookpart book-part)
    (ly:parser-define!
     parser 'toplevel-bookparts
     (cons book-part (ly:parser-lookup parser 'toplevel-bookparts))))
  ;; If toplevel scores have been found before this \bookpart,
  ;; add them first to a dedicated bookpart
  (if (pair? (ly:parser-lookup parser 'toplevel-scores))
      (begin
        (add-bookpart (ly:make-book-part
                       (ly:parser-lookup parser 'toplevel-scores)))
        (ly:parser-define! parser 'toplevel-scores (list))))
  (add-bookpart book-part))

(define-public (collect-scores-for-book parser score)
  (ly:parser-define!
   parser 'toplevel-scores
   (cons score (ly:parser-lookup parser 'toplevel-scores))))

(define-public (collect-music-aux score-handler parser music)
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
         (score-handler (scorify-music music parser)))))

(define-public (collect-music-for-book parser music)
  "Top-level music handler."
  (collect-music-aux (lambda (score)
                       (collect-scores-for-book parser score))
                     parser
                     music))

(define-public (collect-book-music-for-book parser book music)
  "Book music handler."
  (collect-music-aux (lambda (score)
                       (ly:book-add-score! book score))
                     parser
                     music))

(define-public (scorify-music music parser)
  "Preprocess @var{music}."
  (ly:make-score
   (fold (lambda (f m) (f m parser))
         music
         toplevel-music-functions)))

(define (get-current-filename parser book)
  "return any suffix value for output filename allowing for settings by
calls to bookOutputName function"
  (or (paper-variable parser book 'output-filename)
      (ly:parser-output-name parser)))

(define (get-current-suffix parser book)
  "return any suffix value for output filename allowing for settings by calls to
bookoutput function"
  (let ((book-output-suffix (paper-variable parser book 'output-suffix)))
    (if (not (string? book-output-suffix))
        (ly:parser-lookup parser 'output-suffix)
        book-output-suffix)))

(define-public current-outfile-name #f)  ; for use by regression tests

(define (get-outfile-name parser book)
  "return current filename for generating backend output files"
  ;; user can now override the base file name, so we have to use
  ;; the file-name concatenated with any potential output-suffix value
  ;; as the key to out internal a-list
  (let* ((base-name (get-current-filename parser book))
         (output-suffix (get-current-suffix parser book))
         (alist-key (format #f "~a~a" base-name output-suffix))
         (counter-alist (ly:parser-lookup parser 'counter-alist))
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

    (ly:parser-define!
     parser 'counter-alist
     (assoc-set! counter-alist alist-key (1+ output-count)))
    (set! current-outfile-name result)
    result))

(define (print-book-with parser book process-procedure)
  (let* ((paper (ly:parser-lookup parser '$defaultpaper))
         (layout (ly:parser-lookup parser '$defaultlayout))
         (outfile-name (get-outfile-name parser book)))
    (process-procedure book paper layout outfile-name)))

(define-public (print-book-with-defaults parser book)
  (print-book-with parser book ly:book-process))

(define-public (print-book-with-defaults-as-systems parser book)
  (print-book-with parser book ly:book-process-to-systems))

;; Add a score to the current bookpart, book or toplevel
(define-public (add-score parser score)
  (cond
   ((ly:parser-lookup parser '$current-bookpart)
    ((ly:parser-lookup parser 'bookpart-score-handler)
     (ly:parser-lookup parser '$current-bookpart) score))
   ((ly:parser-lookup parser '$current-book)
    ((ly:parser-lookup parser 'book-score-handler)
     (ly:parser-lookup parser '$current-book) score))
   (else
    ((ly:parser-lookup parser 'toplevel-score-handler) parser score))))

(define-public paper-variable
  (let
      ((get-papers
        (lambda (parser book)
          (append (if (and book (ly:output-def? (ly:book-paper book)))
                      (list (ly:book-paper book))
                      '())
                  (ly:parser-lookup parser '$papers)
                  (list (ly:parser-lookup parser '$defaultpaper))))))
    (make-procedure-with-setter
     (lambda (parser book symbol)
       (any (lambda (p) (ly:output-def-lookup p symbol #f))
            (get-papers parser book)))
     (lambda (parser book symbol value)
       (ly:output-def-set-variable!
        (car (get-papers parser book))
        symbol value)))))

(define-public (add-text parser text)
  (add-score parser (list text)))

(define-public (add-music parser music)
  (collect-music-aux (lambda (score)
                       (add-score parser score))
                     parser
                     music))

(define-public (context-mod-from-music parser music)
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
                          (_ "Music unsuitable for context-mod"))
                         (set! warn #f)))))))))
    mods))

(define-public (context-defs-from-music parser output-def music)
  (let ((warn #t))
    (let loop ((m music) (mods #f))
      ;; The parser turns all sets, overrides etc into something
      ;; wrapped in ContextSpeccedMusic.  If we ever get a set,
      ;; override etc that is not wrapped in ContextSpeccedMusic, the
      ;; user has created it in Scheme himself without providing the
      ;; required wrapping.  In that case, using #f in the place of a
      ;; context modification results in a reasonably recognizable
      ;; error.
      (if (music-is-of-type? m 'layout-instruction-event)
          (ly:add-context-mod
           mods
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
             (ly:add-context-mod mods
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
                    (ly:format (_ "Cannot find context-def \\~a")
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
                          (_ "Music unsuitable for output-def"))
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
in list @var{keys}.

For example:
@example
@code{guile> (map-selected-alist-keys - '(a b) '((a . 1) (b . -2) (c . 3) (d . 4)))}
@code{((a . -1) (b . 2) (c . 3) (d . 4)}
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

;; todo: code dup with C++.
(define-safe-public (alist->hash-table lst)
  "Convert alist to table"
  (let ((m (make-hash-table (length lst))))
    (for-each (lambda (k-v) (hashq-set! m (car k-v) (cdr k-v))) lst)
    m))

;;;;;;;;;;;;;;;;
;; list

(define (functional-or . rest)
  (any identity rest))

(define (functional-and . rest)
  (every identity rest))

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
  "Put @var{intermediate} between all elts of @var{lst}."

  (fold-right
   (lambda (elem prev)
     (if (pair? prev)
         (cons  elem (cons intermediate prev))
         (list elem)))
   '() lst))

(define-public filtered-map filter-map)

(define-public (flatten-list x)
  "Unnest list."
  (let loop ((x x) (tail '()))
    (cond ((list? x) (fold-right loop tail x))
          ((not (pair? x)) (cons x tail))
          (else (loop (car x) (loop (cdr x) tail))))))

(define (list-minus a b)
  "Return list of elements in A that are not in B."
  (lset-difference eq? a b))

(define-public (uniq-list lst)
  "Uniq @var{lst}, assuming that it is sorted.  Uses @code{equal?}
for comparisons."

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
the parts (with the separators removed) as a list of lists.  For example,
executing @samp{(split-list-by-separator '(a 0 b c 1 d) number?)} returns
@samp{((a) (b c) (d))}."
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
;; intervals

(define-public empty-interval '(+inf.0 . -inf.0))

(define-public (symmetric-interval expr)
  (cons (- expr) expr))

(define-public (interval-length x)
  "Length of the number-pair @var{x}, if an interval."
  (max 0 (- (cdr x) (car x))))

(define-public (ordered-cons a b)
  (cons (min a b)
        (max a b)))

(define-public (interval-bound interval dir)
  ((if (= dir RIGHT) cdr car) interval))

(define-public (interval-index interval dir)
  "Interpolate @var{interval} between between left (@var{dir}=-1) and
right (@var{dir}=+1)."

  (* (+  (interval-start interval) (interval-end interval)
         (* dir (- (interval-end interval) (interval-start interval))))
     0.5))

(define-public (interval-center x)
  "Center the number-pair @var{x}, if an interval."
  (if (interval-empty? x)
      0.0
      (/ (+ (car x) (cdr x)) 2)))

(define-public interval-start car)

(define-public interval-end cdr)

(define (other-axis a)
  (remainder (+ a 1) 2))

(define-public (interval-scale iv factor)
  (cons (* (car iv) factor)
        (* (cdr iv) factor)))

(define-public (interval-widen iv amount)
  (cons (- (car iv) amount)
        (+ (cdr iv) amount)))

(define-public (interval-empty? iv)
  (> (car iv) (cdr iv)))

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

(define coord-x car)
(define coord-y cdr)

(define (coord-operation operator operand coordinate)
  (if (pair? operand)
      (cons (operator (coord-x operand) (coord-x coordinate))
            (operator (coord-y operand) (coord-y coordinate)))
      (cons (operator operand (coord-x coordinate))
            (operator operand (coord-y coordinate)))))

(define (coord-apply function coordinate)
  (if (pair? function)
      (cons
       ((coord-x function) (coord-x coordinate))
       ((coord-y function) (coord-y coordinate)))
      (cons
       (function (coord-x coordinate))
       (function (coord-y coordinate)))))

(define-public (coord-translate coordinate amount)
  (coord-operation + amount coordinate))

(define-public (coord-scale coordinate amount)
  (coord-operation * amount coordinate))

(define-public (coord-rotate coordinate degrees-in-radians)
  (let*
      ((coordinate
        (cons
         (exact->inexact (coord-x coordinate))
         (exact->inexact (coord-y coordinate))))
       (radius
        (sqrt
         (+ (* (coord-x coordinate) (coord-x coordinate))
            (* (coord-y coordinate) (coord-y coordinate)))))
       (angle (angle-0-2pi (atan (coord-y coordinate) (coord-x coordinate)))))
    (cons
     (* radius (cos (+ angle degrees-in-radians)))
     (* radius (sin (+ angle degrees-in-radians))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; trig

(define-public PI (* 4 (atan 1)))

(define-public TWO-PI (* 2 PI))

(define-public PI-OVER-TWO (/ PI 2))

(define-public THREE-PI-OVER-TWO (* 3 PI-OVER-TWO))

(define-public (cyclic-base-value value cycle)
  "Take @var{value} and modulo-maps it between 0 and base @var{cycle}."
  (if (< value 0)
      (cyclic-base-value (+ value cycle) cycle)
      (if (>= value cycle)
          (cyclic-base-value (- value cycle) cycle)
          value)))

(define-public (angle-0-2pi angle)
  "Take @var{angle} (in radians) and maps it between 0 and 2pi."
  (cyclic-base-value angle TWO-PI))

(define-public (angle-0-360 angle)
  "Take @var{angle} (in degrees) and maps it between 0 and 360 degrees."
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
as rectangular coordinates @ode{(x-length . y-length)}."

  (let ((complex (make-polar
                  radius
                  (degrees->radians angle-in-degrees))))
    (cons
     (real-part complex)
     (imag-part complex))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string

(define-public (string-endswith s suffix)
  (equal? suffix (substring s
                            (max 0 (- (string-length s) (string-length suffix)))
                            (string-length s))))

(define-public (string-startswith s prefix)
  (equal? prefix (substring s 0 (min (string-length s) (string-length prefix)))))

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

(define-public (ly:inexact->string x radix)
  (let ((n (inexact->exact x)))
    (number->string n radix)))

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
  (apply format (cons (current-error-port) (cons string rest)))
  (force-output (current-error-port)))

(define-public (debugf string . rest)
  (if #f
      (apply stderr (cons string rest))))

(define (index-cell cell dir)
  (if (equal? dir 1)
      (cdr cell)
      (car cell)))

(define (cons-map f x)
  "map F to contents of X"
  (cons (f (car x)) (f (cdr x))))

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

;;;;;;;;;;;;;;;;
;; other

(define (sign x)
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

(define-public (car< a b)
  (< (car a) (car b)))

(define-public (car<= a b)
  (<= (car a) (car b)))

(define-public (symbol<? lst r)
  (string<? (symbol->string lst) (symbol->string r)))

(define-public (symbol-key<? lst r)
  (string<? (symbol->string (car lst)) (symbol->string (car r))))

(define-public (eval-carefully symbol module . default)
  "Check whether all symbols in expr @var{symbol} are reachable
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

;;
;; don't confuse users with #<procedure .. > syntax.
;;
(define-public (scm->string val)
  (if (and (procedure? val)
           (symbol? (procedure-name val)))
      (symbol->string (procedure-name val))
      (string-append
       (if (self-evaluating? val)
           (if (string? val)
               "\""
               "")
           "'")
       (call-with-output-string (lambda (port) (display val port)))
       (if (string? val)
           "\""
           ""))))

(define-public (!= lst r)
  (not (= lst r)))

(define-public lily-unit->bigpoint-factor
  (cond
   ((equal? (ly:unit) "mm") (/ 72.0 25.4))
   ((equal? (ly:unit) "pt") (/ 72.0 72.27))
   (else (ly:error (_ "unknown unit: ~S") (ly:unit)))))

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
   (ly:format "~a:1" input-file-name)
   (_ "no \\version statement found, please add~afor future compatibility")
   (format #f "\n\n\\version ~s\n\n" (lilypond-version))))
