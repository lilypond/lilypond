;;;; markup.scm -- Implement a user extensible markup scheme.
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 2003--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>

"
Internally markup is stored as lists, whose head is a function.

  (FUNCTION ARG1 ARG2 ... )

When the markup is formatted, then FUNCTION is called as follows

  (FUNCTION GROB PROPS ARG1 ARG2 ... ) 

GROB is the current grob, PROPS is a list of alists, and ARG1.. are
the rest of the arguments.

The function should return a stencil (i.e. a formatted, ready to
print object).


To add a function, use the define-markup-command utility.

  (define-markup-command (mycommand layout prop arg1 ...) (arg1-type? ...)
    \"my command usage and description\"
    ...function body...)

The command is now available in markup mode, e.g.


  \\markup { .... \\MYCOMMAND #1 argument ... }

" ; "

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; markup definer utilities
;;; `define-markup-command' can be used both for built-in markup
;;; definitions and user defined markups.

(defmacro-public define-markup-command (command-and-args signature . body)
  "

* Define a COMMAND-markup function after command-and-args and body,
register COMMAND-markup and its signature,

* add COMMAND-markup to markup-function-list,

* sets COMMAND-markup markup-signature and markup-keyword object properties,

* define a make-COMMAND-markup function.

Syntax:
  (define-markup-command (COMMAND layout props arg1 arg2 ...) (arg1-type? arg2-type? ...)
    \"documentation string\"
    ...command body...)
 or:
  (define-markup-command COMMAND (arg1-type? arg2-type? ...)
    function)
"
  (let* ((command (if (pair? command-and-args) (car command-and-args) command-and-args))
         (args (if (pair? command-and-args) (cdr command-and-args) '()))
         (command-name (string->symbol (string-append (symbol->string command) "-markup")))
         (make-markup-name (string->symbol (string-append "make-" (symbol->string command-name)))))
    `(begin
       (define-public ,(if (pair? args)
                           (cons command-name args)
                           command-name)
         ,@body)
       (set! (markup-command-signature ,command-name) (list ,@signature))
       (if (not (member ,command-name markup-function-list))
           (set! markup-function-list (cons ,command-name markup-function-list)))
       (define-public (,make-markup-name . args)
         (let ((sig (list ,@signature)))
           (make-markup ,command-name ,(symbol->string make-markup-name) sig args))))))

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
	  (_ "Invalid argument in position ~A.  Expect: ~A, found: ~S.")
	  error-msg))
	(cons markup-function args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; markup constructors
;;; lilypond-like syntax for markup construction in scheme.

(use-modules (ice-9 optargs)
             (ice-9 receive))

(defmacro*-public markup (#:rest body)
  "The `markup' macro provides a lilypond-like syntax for building markups.

 - #:COMMAND is used instead of \\COMMAND
 - #:lines ( ... ) is used instead of { ... }
 - #:center-align ( ... ) is used instead of \\center-align < ... >
 - etc.

Example:
  \\markup { foo
            \\raise #0.2 \\hbracket \\bold bar
            \\override #'(baseline-skip . 4)
            \\bracket \\column < baz bazr bla >
  }
         <==>
  (markup \"foo\"
          #:raise 0.2 #:hbracket #:bold \"bar\"
          #:override '(baseline-skip . 4) 
          #:bracket #:column (\"baz\" \"bazr\" \"bla\"))
Use `markup*' in a \\notes block."
  
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
         (let* ((command (symbol->string (keyword->symbol (car expr))))
                (sig (markup-command-signature
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
                             (set! rest r)))))))
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

(define markup-command-signatures (make-hash-table 50))

(define (markup-command-signature-ref markup-command)
  "Return markup-command's signature, e.g. (number? markup?).
markup-command may be a procedure."
  (let ((sig-key (hashq-ref markup-command-signatures
                            markup-command)))
    (if sig-key (car sig-key) #f)))

(define-public (markup-command-keyword markup-command)
  "Return markup-command's keyword, e.g. \"scheme0markup1\".
markup-command may be a procedure."
  (let ((sig-key (hashq-ref markup-command-signatures
                            markup-command)))
    (if sig-key (cdr sig-key) #f)))

(define (markup-command-signatureset! markup-command signature)
  "Set markup-command's signature. markup-command must be a named procedure.
Also set markup-signature and markup-keyword object properties."
  (hashq-set! markup-command-signatures
              markup-command
              (cons signature (markup-signature-to-keyword signature)))
  ;; these object properties are still in use somewhere
  (set-object-property! markup-command 'markup-signature signature)
  (set-object-property! markup-command 'markup-keyword (markup-signature-to-keyword signature)))
  
(define-public markup-command-signature
  (make-procedure-with-setter markup-command-signature-ref markup-command-signatureset!))

(define (markup-symbol-to-proc markup-sym)
  "Return the markup command procedure which name is `markup-sym', if any."
  (hash-fold (lambda (key val prev)
                            (or prev
                                (if (eqv? (procedure-name key) markup-sym) key #f)))
             #f
             markup-command-signatures))

(define-public markup-function-list '())

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

(define-public (lookup-markup-command code)
  (let ((proc (markup-symbol-to-proc (string->symbol (string-append code "-markup")))))
    (and proc (cons proc (markup-command-keyword proc)))))

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
  (not (not (markup-command-signature x))))

(define-public (markup-list? arg)
  (define (markup-list-inner? lst)
    (or (null? lst)
        (and (markup? (car lst)) (markup-list-inner? (cdr lst)))))
  (and (list? arg) (markup-list-inner? arg)))

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
;; typecheck, and throw an error when something amiss.
;; 
(define (markup-thrower-typecheck arg)
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







