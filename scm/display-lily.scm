;;; display-lily.scm -- Display music expressions using LilyPond notation
;;;
;;;
;;;
;;; Copyright (C) 2005--2022 Nicolas Sceaux  <nicolas.sceaux@free.fr>
;;;

;;; - This file defines the procedures used to define display methods for each
;;; music type: define-display-method and define-extra-display-method.
;;; See scm/define-music-display-methods.scm
;;; Display methods are stored in the `display-methods' property of each music
;;; type.
;;;
;;; - `music->lily-string' return a string describing a music
;;; expression using LilyPond notation. The special variables *indent*
;;; and *omit-duration* influence the indentation level and the
;;; display of music durations.
;;;
;;; - `with-music-match' can be used to destructure a music expression, extracting
;;; some interesting music properties.


(define-module (lily display-lily)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-39)
  #:use-module (lily))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Display method definition and call
;;;


(define-syntax-rule (define-display-method music-type (param) form ...)
  "Define a display method for a music type and store it in the
`display-methods' property of the music type entry found in the
`music-name-to-property-table' hash table.  Print methods previously
defined for that music type are lost.
Syntax: (define-display-method MusicType (expression)
          ...body...))"
  (let ((type-props (hashq-ref music-name-to-property-table
                               'music-type '()))
        (method (lambda (param)
                  form
                  ...)))
    (set! type-props
          (assoc-set! type-props 'display-methods (list method)))
    (hashq-set! music-name-to-property-table
                'music-type
                type-props)
    method))

(define-syntax-rule (define-extra-display-method music-type (param) form ...)
  "Add a display method for a music type.  A primary display method
is supposed to have been previously defined with `define-display-method'.
This new method should return a string or #f.  If #f is returned, the next
display method will be called."
  (let* ((type-props (hashq-ref music-name-to-property-table
                                'music-type '()))
         (methods (assoc-ref type-props 'display-methods))
         (new-method (lambda (param)
                       form
                       ...)))
    (set! type-props
          (assoc-set! type-props
                      'display-methods
                      (cons new-method methods)))
    (hashq-set! music-name-to-property-table
                'music-type
                type-props)
    new-method))

(define* (tag->lily-string expr #:optional (post-event? #f))
  (format #f "~{~a ~}"
          (map (lambda (tag)
                 (format #f "~a\\tag #'~a" (if post-event? "-" "") tag))
               (ly:music-property expr 'tags))))

(define* (tweaks->lily-string expr #:optional (post-event? #f))
  (format #f "~{~a ~}"
          (map (lambda (tweak)
                 (let ((addr (car tweak))
                       (val (cdr tweak)))
                   (format #f "~a\\tweak ~a #~a"
                           (if post-event? "-" "")
                           (cond
                            ((symbol? addr)
                             (format #f "~a" addr))
                            ((symbol? (cdr addr))
                             (format #f "~a.~a" (car addr) (cdr addr)))
                            (else
                             (format #f "~{~a~^.~}"
                                     (if (symbol? (car addr))
                                         addr
                                         (cdr addr)))))
                           (scheme-expr->lily-string val))))
               (ly:music-property expr 'tweaks))))

(define-public (music->lily-string expr)
  "Print @var{expr}, a music expression, in LilyPond syntax."
  (if (ly:music? expr)
      (let* ((music-type (ly:music-property expr 'name))
             (procs (assoc-ref (hashq-ref music-name-to-property-table
                                          music-type '())
                               'display-methods))
             (result-string (and procs (any (lambda (proc)
                                              (proc expr))
                                            procs))))
        (if result-string
            (format #f "~a~a~a"
                    (tag->lily-string expr (post-event? expr))
                    (tweaks->lily-string expr (post-event? expr))
                    result-string)
            (format #f "%{ Print method not implemented for music type ~a %}"
                    music-type)))
      (format #f "%{ expecting a music expression: ~a %}" expr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Music pattern matching
;;;

(define (var? x)
  (and (symbol? x) (char=? #\? (string-ref (symbol->string x) 0))))

(define (music? x)
  (and (pair? x) (eqv? (car x) 'music)))

(define (music-list? x)
  (and (pair? x)
       (every music? x)))

(define (music-or-var-list? x)
  (and (pair? x)
       (every (lambda (e)
                (or (music? e) (var? e)))
              x)))

(define (key-val-list->alist lst)
  (define (key-val-list->alist-aux lst prev-result)
    (if (null? lst)
        prev-result
        (key-val-list->alist-aux (cddr lst)
                                 (cons (cons (first lst) (second lst))
                                       prev-result))))
  (reverse! (key-val-list->alist-aux lst (list))))

(define (gen-condition expr pattern)
  "Helper function for `with-music-match'.
Generate an form that checks if the properties of `expr'
match thoses described in `pattern'."
  (let* (;; all (property . value) found at the first depth in pattern,
         ;; including a (name . <Musictype>) pair.
         (pat-all-props (cons (cons 'name (second pattern))
                              (key-val-list->alist (cddr pattern))))
         ;; all (property . value) pairs found in pattern, where value is not
         ;; a ?var, a music expression or a music list.
         (prop-vals (remove (lambda (kons)
                              (or (var? (cdr kons))
                                  (music? (cdr kons))
                                  (music-or-var-list? (cdr kons))))
                            pat-all-props))
         ;; list of (property . element) pairs, where element is a music expression
         (element-list (filter (lambda (kons) (music? (cdr kons)))
                               pat-all-props))
         ;; list of (property . (e1 e2 ..)) pairs, where (e1 e2 ...) is a
         ;; list a music expressions
         (elements-list (filter (lambda (kons) (music-or-var-list? (cdr kons)))
                                pat-all-props)))
    `(and
      ;; a form that checks that `expr' is a music expression
      ;; before actually accessing its properties...
      (ly:music? ,expr)
      ;; a form that checks that `expr' properties have the same
      ;; values as those given in `pattern'
      ,@(map (lambda (prop-val)
               (let ((prop (car prop-val))
                     (val (cdr prop-val)))
                 `(and (not (null? (ly:music-property ,expr ',prop)))
                       (equal? (ly:music-property ,expr ',prop) ,val))))
             prop-vals)
      ;; build the test condition for each element found in a (property . element) pair.
      ;; (typically, property will be 'element)
      ,@(map (lambda (prop-element)
               (gen-condition `(ly:music-property ,expr ',(car prop-element)) (cdr prop-element)))
             element-list)
      ;; build the test conditions for each element found in a (property . (e1 e2 ...)) pair.
      ;; this requires accessing to an element of a list, hence the index.
      ;; (typically, property will be 'elements)
      ,@(map
         (lambda (prop-elements)
           (let ((ges (gensym))
                 (len (length (cdr prop-elements))))
             `(let ((,ges (ly:music-property ,expr ',(car prop-elements))))
                (and (eqv? (length+ ,ges) ,len)
                     ,@(filter-map
                        (lambda (e index)
                          (and (music? e)
                               (gen-condition `(list-ref ,ges ,index) e)))
                        (cdr prop-elements) (iota len))))))
         elements-list))))

(define (gen-bindings expr pattern)
  "Helper function for `with-music-match'.
Generate binding forms by looking for ?var symbol in pattern."
  (let* (;; all (property . value) found at the first depth of pattern,
         ;; including a (name . <Musictype>) pair.
         (pat-all-props (cons (cons 'name (second pattern))
                              (key-val-list->alist (cddr pattern))))
         ;; all (property . ?var) pairs
         (prop-vars (filter (lambda (kons) (var? (cdr kons)))
                            pat-all-props))
         ;; list of (property . element) pairs, where element is a music expression
         (element-list (filter (lambda (kons) (music? (cdr kons)))
                               pat-all-props))
         ;; list of (property . (e1 e2 ..)) pairs, where (e1 e2 ...) is a
         ;; list a music expressions
         (elements-list (filter (lambda (kons) (music-or-var-list? (cdr kons)))
                                pat-all-props)))
    (append
     ;; the binding form for the ?var variable found in pattern (first depth).
     ;; ?var is bound to the value of `expr' property
     (map (lambda (prop-var)
            `(,(cdr prop-var) (ly:music-property ,expr ',(car prop-var))))
          prop-vars)
     ;; generate bindings for each element found in a (property . element) pair.
     ;; (typically, property will be 'element)
     (append-map (lambda (prop-element)
                   (gen-bindings `(ly:music-property ,expr ',(car prop-element))
                                 (cdr prop-element)))
                 element-list)
     ;; generate bindings for each element found in a (property . (e1 e2 ...)) pair
     ;; (typically, property will be 'elements)
     (append-map (lambda (prop-elements)
                   (let ((index -1))
                     (append-map (lambda (e)
                                   (set! index (1+ index))
                                   (if (var? e)
                                       `((,e (list-ref (ly:music-property ,expr ',(car prop-elements)) ,index)))
                                       (gen-bindings `(list-ref (ly:music-property ,expr ',(car prop-elements))
                                                                ,index)
                                                     e)))
                                 (cdr prop-elements))))
                 elements-list))))

(defmacro-public with-music-match (music-expr+pattern . body)
  "If `music-expr' matches `pattern', call `body'.  `pattern' should look like:
  '(music <MusicType>
     property value
     property ?var1
     element (music <MusicType> ...)
     elements ((music <MusicType> ...)
               ?var2
               (music <MusicType> ...)))
The properties of `music-expr' are checked against the values given in the
pattern (the name property being the <MusicType> symbol after the `music'
keyword), then all music expression found in its properties (such as 'element
or 'elements).
When ?var is found instead of a property value, ?var is bound that property value,
as read inside `music-expr'.  ?var may also be used to refere to a whole music
expression inside an elements list for instance.  These bindings are accessible
inside body."
  (let ((music-expr (first music-expr+pattern))
        (pattern (second music-expr+pattern))
        (expr-sym (gensym)))
    `(let ((,expr-sym ,music-expr))
       (and ,(gen-condition expr-sym pattern)
            (let ,(gen-bindings expr-sym pattern)
              ,@body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Special parameters
;;;

;;; indentation
(define-public *indent* (make-parameter 0))

;;; set to #t to omit duration printing
(define-public *omit-duration* (make-parameter #f))

;;; Set to #t to force a line break with some kinds of expressions (eg sequential music)
(define *force-line-break* (make-parameter #t))
(define *max-element-number-before-break* (make-parameter 6))

;; \times factor (used in durations)
(define *time-scale* (make-parameter 1))

(define *current-context* (make-parameter 'Bottom))

(define *explicit-mode* (make-parameter #t))

(define (new-line->lily-string)
  (format #f "~%~v_" (max 0 (1- (*indent*)))))

;;;
;;; music type predicate maker
;;;

(define (make-music-type-predicate . music-types)
  (define make-music-type-predicate-aux
    (lambda (mtypes)
      (lambda (expr)
        (if (null? mtypes)
            #f
            (or (eqv? (car mtypes) (ly:music-property expr 'name))
                ((make-music-type-predicate-aux (cdr mtypes)) expr))))))
  (make-music-type-predicate-aux music-types))

(ly:load "define-music-display-methods")
