;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2003--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

"
Internally markup is stored as lists, whose head is a function.

  (FUNCTION ARG1 ARG2 ... )

When the markup is formatted, then FUNCTION is called as follows

  (FUNCTION GROB PROPS ARG1 ARG2 ... ) 

GROB is the current grob, PROPS is a list of alists, and ARG1.. are
the rest of the arguments.

The function should return a stencil (i.e. a formatted, ready to
print object).


To add a builtin markup command, use the define-builtin-markup-command
utility. In a user file, the define-markup-command macro shall be used
(see ly/markup-init.ly).

  (define-markup-command (mycommand layout prop arg1 ...) (arg1-type? ...)
    \"my command usage and description\"
    ...function body...)

The command is now available in markup mode, e.g.

  \\markup { .... \\MYCOMMAND #1 argument ... }

" ; "

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; markup definer utilities

;; For documentation purposes
;; category -> markup functions
(define-public markup-functions-by-category (make-hash-table 150))
;; markup function -> used properties
(define-public markup-functions-properties (make-hash-table 150))
;; List of markup list functions
(define-public markup-list-function-list (list))

(define-macro (define-builtin-markup-command command-and-args signature
                category properties-or-copied-function . body)
  "
* Define a COMMAND-markup function after command-and-args and body,
register COMMAND-markup and its signature,

* add COMMAND-markup to markup-functions-by-category,

* sets COMMAND-markup markup-signature and markup-keyword object properties,

* define a make-COMMAND-markup function.

Syntax:
  (define-builtin-markup-command (COMMAND layout props . arguments)
                                 argument-types
                                 category
                                 properties
    \"documentation string\"
    ...command body...)
 or:
  (define-builtin-markup-command COMMAND
                                 argument-types
                                 category
                                 function)

where:
  argument-types is a list of type predicates for arguments
  category is either a symbol or a symbol list
  properties a list of (property default-value) lists or COMMANDx-markup elements
    (when a COMMANDx-markup is found, the properties of the said commandx are
    added instead). No check is performed against cyclical references!
"
  (let* ((command (if (pair? command-and-args) (car command-and-args) command-and-args))
         (args (if (pair? command-and-args) (cdr command-and-args) '()))
         (command-name (string->symbol (format #f "~a-markup" command)))
         (make-markup-name (string->symbol (format #f "make-~a-markup" command))))
    `(begin
       ;; define the COMMAND-markup function
       ,(if (pair? args)
            (let ((documentation (car body))
                  (real-body (cdr body))
                  (properties properties-or-copied-function))
              `(define-public (,command-name ,@args)
                 ,documentation
                 (let ,(filter identity
                               (map (lambda (prop-spec)
                                      (if (pair? prop-spec)
                                          (let ((prop (car prop-spec))
                                                (default-value (if (null? (cdr prop-spec))
                                                                   #f
                                                                   (cadr prop-spec)))
                                                (props (cadr args)))
                                            `(,prop (chain-assoc-get ',prop ,props ,default-value)))
                                          #f))
                                    properties))
                   ,@real-body)))
            (let ((args (gensym "args"))
                  (markup-command properties-or-copied-function))
              `(define-public (,command-name . ,args)
                 ,(format #f "Copy of the ~a command." markup-command)
                 (apply ,markup-command ,args))))
       (set! (markup-command-signature ,command-name) (list ,@signature))
       ;; Register the new function, for markup documentation
       ,@(map (lambda (category)
                `(hashq-set! markup-functions-by-category ',category
                             (cons ,command-name
                                   (or (hashq-ref markup-functions-by-category ',category)
                                       (list)))))
              (if (list? category) category (list category)))
       ;; Used properties, for markup documentation
       (hashq-set! markup-functions-properties
                   ,command-name
                   (list ,@(map (lambda (prop-spec)
                                  (cond ((symbol? prop-spec)
                                         prop-spec)
                                         ((not (null? (cdr prop-spec)))
                                          `(list ',(car prop-spec) ,(cadr prop-spec)))
                                         (else
                                          `(list ',(car prop-spec)))))
                                (if (pair? args)
                                    properties-or-copied-function
                                    (list)))))
       ;; define the make-COMMAND-markup function
       (define-public (,make-markup-name . args)
         (let ((sig (list ,@signature)))
           (make-markup ,command-name ,(symbol->string make-markup-name) sig args))))))

(define-macro (define-builtin-markup-list-command command-and-args signature
                properties . body)
  "Same as `define-builtin-markup-command, but defines a command that, when
interpreted, returns a list of stencils instead os a single one"
  (let* ((command (if (pair? command-and-args) (car command-and-args) command-and-args))
         (args (if (pair? command-and-args) (cdr command-and-args) '()))
         (command-name (string->symbol (format #f "~a-markup-list" command)))
         (make-markup-name (string->symbol (format #f "make-~a-markup-list" command))))
    `(begin
       ;; define the COMMAND-markup-list function
       ,(if (pair? args)
            (let ((documentation (car body))
                  (real-body (cdr body)))
              `(define-public (,command-name ,@args)
                 ,documentation
                 (let ,(filter identity
                               (map (lambda (prop-spec)
                                      (if (pair? prop-spec)
                                          (let ((prop (car prop-spec))
                                                (default-value (if (null? (cdr prop-spec))
                                                                   #f
                                                                   (cadr prop-spec)))
                                                (props (cadr args)))
                                            `(,prop (chain-assoc-get ',prop ,props ,default-value)))
                                          #f))
                                    properties))
                   ,@body)))
            (let ((args (gensym "args"))
                  (markup-command (car body)))
            `(define-public (,command-name . ,args)
               ,(format #f "Copy of the ~a command." markup-command)
               (apply ,markup-command ,args))))
       (set! (markup-command-signature ,command-name) (list ,@signature))
       ;; add the command to markup-list-function-list, for markup documentation
       (if (not (member ,command-name markup-list-function-list))
           (set! markup-list-function-list (cons ,command-name
                                                 markup-list-function-list)))
       ;; Used properties, for markup documentation
       (hashq-set! markup-functions-properties
                   ,command-name
                   (list ,@(map (lambda (prop-spec)
                                  (cond ((symbol? prop-spec)
                                         prop-spec)
                                         ((not (null? (cdr prop-spec)))
                                          `(list ',(car prop-spec) ,(cadr prop-spec)))
                                         (else
                                          `(list ',(car prop-spec)))))
                                (if (pair? args)
                                    properties
                                    (list)))))
       ;; it's a markup-list command:
       (set-object-property! ,command-name 'markup-list-command #t)
       ;; define the make-COMMAND-markup-list function
       (define-public (,make-markup-name . args)
         (let ((sig (list ,@signature)))
           (list (make-markup ,command-name
                              ,(symbol->string make-markup-name) sig args)))))))

(define-public (make-markup markup-function make-name signature args)
  " Construct a markup object from MARKUP-FUNCTION and ARGS. Typecheck
against SIGNATURE, reporting MAKE-NAME as the user-invoked function.
"
  (let* ((arglen (length args))
         (siglen (length signature))
         (error-msg (if (and (> siglen 0) (> arglen 0))
                        (markup-argument-list-error signature args 1)
                        #f)))
    (if (or (not (= arglen siglen)) (< siglen 0) (< arglen 0))
        (ly:error (string-append make-name ": "
                   (_ "Wrong number of arguments.  Expect: ~A, found ~A: ~S"))
		  siglen arglen args))
    (if error-msg
        (ly:error
	 (string-append
	  make-name ": "
	  (_ "Invalid argument in position ~A.  Expect: ~A, found: ~S."))
	  (car error-msg) (cadr error-msg)(caddr error-msg))
	(cons markup-function args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; markup constructors
;;; lilypond-like syntax for markup construction in scheme.

(use-modules (ice-9 optargs)
             (ice-9 receive))

(defmacro*-public markup (#:rest body)
  "The `markup' macro provides a lilypond-like syntax for building markups.

 - #:COMMAND is used instead of \\COMMAND
 - #:line ( ... ) is used instead of \\line { ... }
 - etc.

Example:
  \\markup { foo
            \\raise #0.2 \\hbracket \\bold bar
            \\override #'(baseline-skip . 4)
            \\bracket \\column { baz bazr bla }
  }
         <==>
  (markup \"foo\"
          #:raise 0.2 #:hbracket #:bold \"bar\"
          #:override '(baseline-skip . 4) 
          #:bracket #:column (\"baz\" \"bazr\" \"bla\"))
Use `markup*' in a \\notemode context."
  
  (car (compile-all-markup-expressions `(#:line ,body))))

(defmacro*-public markup* (#:rest body)
  "Same as `markup', for use in a \\notes block."
  `(ly:export (markup ,@body)))
  
  
(define (compile-all-markup-expressions expr)
  "Return a list of canonical markups expressions, e.g.:
  (#:COMMAND1 arg11 arg12 #:COMMAND2 arg21 arg22 arg23)
  ===>
  ((make-COMMAND1-markup arg11 arg12)
   (make-COMMAND2-markup arg21 arg22 arg23) ...)"
  (do ((rest expr rest)
       (markps '() markps))
      ((null? rest) (reverse markps))
    (receive (m r) (compile-markup-expression rest)
             (set! markps (cons m markps))
             (set! rest r))))

(define (keyword->make-markup key)
  "Transform a keyword, e.g. #:COMMAND, in a make-COMMAND-markup symbol."
  (string->symbol (string-append "make-" (symbol->string (keyword->symbol key)) "-markup")))

(define (compile-markup-expression expr)
  "Return two values: the first complete canonical markup expression
   found in `expr', e.g. (make-COMMAND-markup arg1 arg2 ...),
   and the rest expression."
  (cond ((and (pair? expr)
              (keyword? (car expr)))
         ;; expr === (#:COMMAND arg1 ...)
         (let ((command (symbol->string (keyword->symbol (car expr)))))
            (if (not (pair? (lookup-markup-command command)))
                (ly:error (_ "Not a markup command: ~A") command))
            (let* ((sig (markup-command-signature
                         (car (lookup-markup-command command))))
                   (sig-len (length sig)))
              (do ((i 0 (1+ i))
                   (args '() args)
                   (rest (cdr expr) rest))
                  ((>= i sig-len)
                   (values (cons (keyword->make-markup (car expr)) (reverse args)) rest))
                (cond ((eqv? (list-ref sig i) markup-list?)
                       ;; (car rest) is a markup list
                       (set! args (cons `(list ,@(compile-all-markup-expressions (car rest))) args))
                       (set! rest (cdr rest)))
                      (else
                       ;; pick up one arg in `rest'
                       (receive (a r) (compile-markup-arg rest)
                         (set! args (cons a args))
                         (set! rest r))))))))
        ((and (pair? expr)
              (pair? (car expr))
              (keyword? (caar expr)))
         ;; expr === ((#:COMMAND arg1 ...) ...)
         (receive (m r) (compile-markup-expression (car expr))
                  (values m (cdr expr))))
        ((and (pair? expr)
              (string? (car expr))) ;; expr === ("string" ...)
         (values `(make-simple-markup ,(car expr)) (cdr expr)))
        (else
         ;; expr === (symbol ...) or ((funcall ...) ...)
         (values (car expr)
                 (cdr expr)))))

(define (compile-all-markup-args expr)
  "Transform `expr' into markup arguments"
  (do ((rest expr rest)
       (args '() args))
      ((null? rest) (reverse args))
    (receive (a r) (compile-markup-arg rest)
             (set! args (cons a args))
             (set! rest r))))

(define (compile-markup-arg expr)
  "Return two values: the desired markup argument, and the rest arguments"
  (cond ((null? expr)
         ;; no more args
         (values '() '()))
        ((keyword? (car expr))
         ;; expr === (#:COMMAND ...)
         ;; ==> build and return the whole markup expression
         (compile-markup-expression expr))
        ((and (pair? (car expr))
              (keyword? (caar expr)))
         ;; expr === ((#:COMMAND ...) ...)
         ;; ==> build and return the whole markup expression(s)
         ;; found in (car expr)
         (receive (markup-expr rest-expr) (compile-markup-expression (car expr))
                  (if (null? rest-expr)
                      (values markup-expr (cdr expr))
                      (values `(list ,markup-expr ,@(compile-all-markup-args rest-expr))
                              (cdr expr)))))
        ((and (pair? (car expr))
              (pair? (caar expr)))
         ;; expr === (((foo ...) ...) ...)
         (values (cons 'list (compile-all-markup-args (car expr))) (cdr expr)))
        (else (values (car expr) (cdr expr)))))

;;;;;;;;;;;;;;;
;;; Utilities for storing and accessing markup commands signature
;;; and keyword.
;;; Examples:
;;;
;;; (set! (markup-command-signature raise-markup) (list number? markup?))
;;; ==> ((#<primitive-procedure number?> #<procedure markup? (obj)>) . scheme0-markup1)
;;;
;;; (markup-command-signature raise-markup)
;;; ==> (#<primitive-procedure number?> #<procedure markup? (obj)>)
;;;
;;; (markup-command-keyword raise-markup) ==> "scheme0-markup1"
;;; 

(define-public (markup-command-keyword markup-command)
  "Return markup-command's argument keyword, ie a string describing the command
  arguments, eg. \"scheme0markup1\""
  (object-property markup-command 'markup-keyword))

(define-public (markup-command-signature-ref markup-command)
  "Return markup-command's signature (the 'markup-signature object property)"
  (object-property markup-command 'markup-signature))

(define-public (markup-command-signature-set! markup-command signature)
  "Set markup-command's signature and keyword (as object properties)"
  (set-object-property! markup-command 'markup-signature signature)
  (set-object-property! markup-command 'markup-keyword 
                        (markup-signature-to-keyword signature))
  signature)

(define-public markup-command-signature
  (make-procedure-with-setter markup-command-signature-ref
                              markup-command-signature-set!))

(define-public (markup-signature-to-keyword sig)
  " (A B C) -> a0-b1-c2 "
  (if (null? sig)
      'empty
      (string->symbol (string-join (map
                                    (let* ((count 0))
                                      (lambda (func)
                                        (set! count (+ count 1))
                                        (string-append
                                         ;; for reasons I don't get,
                                         ;; (case func ((markup?) .. )
                                         ;; doesn't work.
                                         (cond 
                                          ((eq? func markup?) "markup")
                                          ((eq? func markup-list?) "markup-list")
                                          (else "scheme"))
                                         (number->string (- count 1)))))
                                    sig)
                         "-"))))

(define (lookup-markup-command-aux symbol)
  (let ((proc (catch 'misc-error
                (lambda ()
                  (module-ref (current-module) symbol))
                (lambda (key . args) #f))))
    (and (procedure? proc) proc)))

(define-public (lookup-markup-command code)
  (let ((proc (lookup-markup-command-aux
	       (string->symbol (format #f "~a-markup" code)))))
    (and proc (markup-function? proc)
	 (cons proc (markup-command-keyword proc)))))

(define-public (lookup-markup-list-command code)
  (let ((proc (lookup-markup-command-aux
	       (string->symbol (format #f "~a-markup-list" code)))))
     (and proc (markup-list-function? proc)
	  (cons proc (markup-command-keyword proc)))))

;;;;;;;;;;;;;;;;;;;;;;
;;; used in parser.yy to map a list of markup commands on markup arguments
(define-public (map-markup-command-list commands markups)
  "`markups' being a list of markups, eg (markup1 markup2 markup3),
and `commands' a list of commands with their scheme arguments, in reverse order,
eg: ((italic) (raise 4) (bold)), maps the commands on each markup argument, eg:
 ((bold (raise 4 (italic markup1)))
  (bold (raise 4 (italic markup2)))
  (bold (raise 4 (italic markup3))))
"
  (map-in-order (lambda (arg)
                  (let ((result arg))
                    (for-each (lambda (cmd)
                                (set! result (append cmd (list result))))
                              commands)
                    result))
                markups))

;;;;;;;;;;;;;;;;;;;;;;
;;; markup type predicates

(define (markup-function? x)
  (and (markup-command-signature x)
       (not (object-property x 'markup-list-command))))

(define (markup-list-function? x)
  (and (markup-command-signature x)
       (object-property x 'markup-list-command)))

(define-public (markup-command-list? x)
  "Determine if `x' is a markup command list, ie. a list composed of
a markup list function and its arguments."
  (and (pair? x) (markup-list-function? (car x))))

(define-public (markup-list? arg)
  "Return a true value if `x' is a list of markups or markup command lists."
  (define (markup-list-inner? lst)
    (or (null? lst)
	(and (or (markup? (car lst)) (markup-command-list? (car lst)))
             (markup-list-inner? (cdr lst)))))
  (not (not (and (list? arg) (markup-list-inner? arg)))))

(define (markup-argument-list? signature arguments)
  "Typecheck argument list."
  (if (and (pair? signature) (pair? arguments))
      (and ((car signature) (car arguments))
           (markup-argument-list? (cdr signature) (cdr arguments)))
      (and (null? signature) (null? arguments))))


(define (markup-argument-list-error signature arguments number)
  "return (ARG-NR TYPE-EXPECTED ARG-FOUND) if an error is detected, or
#f is no error found.
"
  (if (and (pair? signature) (pair? arguments))
      (if (not ((car signature) (car arguments)))
          (list number (type-name (car signature)) (car arguments))
          (markup-argument-list-error (cdr signature) (cdr arguments) (+ 1 number)))
      #f))

;;
;; full recursive typecheck.
;;
(define (markup-typecheck? arg)
  (or (string? arg)
      (and (pair? arg)
           (markup-function? (car arg))
           (markup-argument-list? (markup-command-signature (car arg))
                                  (cdr arg)))))

;; 
;; 
;;
;; 
(define (markup-thrower-typecheck arg)
  "typecheck, and throw an error when something amiss.

Uncovered - cheap-markup? is used."

  (cond ((string? arg) #t)
        ((not (pair? arg))
         (throw 'markup-format "Not a pair" arg))
        ((not (markup-function? (car arg)))
         (throw 'markup-format "Not a markup function " (car arg)))
        ((not (markup-argument-list? (markup-command-signature (car arg))
                                     (cdr arg)))
         (throw 'markup-format "Arguments failed  typecheck for " arg)))
  #t)

;;
;; good enough if you only  use make-XXX-markup functions.
;; 
(define (cheap-markup? x)
  (or (string? x)
      (and (pair? x)
           (markup-function? (car x)))))

;;
;; replace by markup-thrower-typecheck for more detailed diagnostics.
;; 
(define-public markup? cheap-markup?)

;; utility

(define (markup-join markups sep)
  "Return line-markup of MARKUPS, joining them with markup SEP"
  (if (pair? markups)
      (make-line-markup (list-insert-separator markups sep))
      empty-markup))


(define-public interpret-markup ly:text-interface::interpret-markup)

(define-public (interpret-markup-list layout props markup-list)
  (let ((stencils (list)))
    (for-each (lambda (m)
		(set! stencils
		      (if (markup-command-list? m)
			  (append! (reverse! (apply (car m) layout props (cdr m)))
				   stencils)
			  (cons (interpret-markup layout props m) stencils))))
	      markup-list)
    (reverse! stencils)))

(define-public (prepend-alist-chain key val chain)
  (cons (acons key val (car chain)) (cdr chain)))

(define-public (stack-stencil-line space stencils)
  "DOCME"
  (if (and (pair? stencils)
	   (ly:stencil? (car stencils)))
      
      (if (and (pair? (cdr stencils))
	       (ly:stencil? (cadr stencils)))
          (let* ((tail (stack-stencil-line space (cdr stencils)))
                 (head (car stencils))
                 (xoff (+ space (cdr (ly:stencil-extent head X)))))
            (ly:stencil-add head
                             (ly:stencil-translate-axis tail xoff X)))
          (car stencils))
      (ly:make-stencil '() '(0 . 0) '(0 . 0))))

