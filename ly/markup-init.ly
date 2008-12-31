%% -*- Mode: Scheme -*-

\version "2.12.0"

%%;; to be define later, in a closure
#(define-public toplevel-module-define-public! #f)
#(define-public toplevel-module-ref #f)
#(let ((toplevel-module (current-module)))
   (set! toplevel-module-define-public!
         (lambda (symbol value)
           (module-define! toplevel-module symbol value)
           (module-export! toplevel-module (list symbol))))
   (set! toplevel-module-ref
         (lambda (symbol)
           (module-ref toplevel-module symbol))))

#(defmacro-public define-public-toplevel
   (first-arg . rest)
  "Define a public variable or function in the toplevel module:
  (define-public-toplevel variable-name value)
or:
  (define-public-toplevel (function-name . args)
    ..body..)"
  (if (symbol? first-arg)
      ;; (define-public-toplevel symbol value)
      (let ((symbol first-arg)
            (value (car rest)))
        `(toplevel-module-define-public! ',symbol ,value))
      ;; (define-public-toplevel (function-name . args) . body)
      (let ((function-name (car first-arg))
            (arg-list (cdr first-arg))
            (body rest))
        `(toplevel-module-define-public!
          ',function-name
          (let ((proc (lambda ,arg-list
                        ,@body)))
            (set-procedure-property! proc
                                     'name
                                     ',function-name)
            proc)))))

#(defmacro-public define-markup-command (command-and-args signature . body)
  "
* Define a COMMAND-markup function after command-and-args and body,
register COMMAND-markup and its signature,

* add COMMAND-markup to markup-function-list,

* sets COMMAND-markup markup-signature and markup-keyword object properties,

* define a make-COMMAND-markup function.

Syntax:
  (define-markup-command (COMMAND layout props arg1 arg2 ...)
                         (arg1-type? arg2-type? ...)
    \"documentation string\"
    ...command body...)
or:
  (define-markup-command COMMAND (arg1-type? arg2-type? ...) function)
"
  (let* ((command (if (pair? command-and-args)
                      (car command-and-args)
                      command-and-args))
         (command-name (string->symbol (format #f "~a-markup" command)))
         (make-markup-name (string->symbol (format #f "make-~a-markup" command))))
    `(begin
       ;; define the COMMAND-markup procedure in toplevel module
       ,(if (pair? command-and-args)
            ;; 1/ (define (COMMAND-markup layout props arg1 arg2 ...)
            ;;      ..command body))
            `(define-public-toplevel (,command-name ,@(cdr command-and-args))
               ,@body)
            ;; 2/ (define (COMMAND-markup . args) (apply function args))
            (let ((args (gensym "args"))
                  (command (car body)))
            `(define-public-toplevel (,command-name . ,args)
               (apply ,command ,args))))
       (let ((command-proc (toplevel-module-ref ',command-name)))
         ;; register its command signature
         (set! (markup-command-signature command-proc)
               (list ,@signature))
         ;; define the make-COMMAND-markup procedure in the toplevel module
         (define-public-toplevel (,make-markup-name . args)
           (make-markup command-proc
                        ,(symbol->string make-markup-name)
                        (list ,@signature)
                        args))))))

#(defmacro-public define-markup-list-command (command-and-args signature . body)
  "Same as `define-markup-command', but defines a command that, when interpreted,
returns a list of stencils, instead of a single one."
  (let* ((command (if (pair? command-and-args)
		      (car command-and-args)
		      command-and-args))
	 (command-name (string->symbol (format #f "~a-markup-list" command)))
	 (make-markup-name (string->symbol (format #f "make-~a-markup-list" command))))
    `(begin
       ;; define the COMMAND-markup-list procedure in toplevel module
       ,(if (pair? command-and-args)
	    ;; 1/ (define (COMMAND-markup-list layout props arg1 arg2 ...)
	    ;;	    ..command body))
	    `(define-public-toplevel (,command-name ,@(cdr command-and-args))
	       ,@body)
	    ;; 2/ (define (COMMAND-markup-list . args) (apply function args))
	    (let ((args (gensym "args"))
		  (command (car body)))
	    `(define-public-toplevel (,command-name . ,args)
	       (apply ,command ,args))))
       (let ((command-proc (toplevel-module-ref ',command-name)))
	 ;; register its command signature
	 (set! (markup-command-signature command-proc)
	       (list ,@signature))
	 ;; it's a markup-list command:
	 (set-object-property! command-proc 'markup-list-command #t)
	 ;; define the make-COMMAND-markup-list procedure in the toplevel module
	 (define-public-toplevel (,make-markup-name . args)
	   (list (make-markup command-proc
			      ,(symbol->string make-markup-name)
			      (list ,@signature)
			      args)))))))
