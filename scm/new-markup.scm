"
Internally markup is stored as lists, whose head is a function.

  (FUNCTION ARG1 ARG2 ... )

When the markup is formatted, then FUNCTION is called as follows

  (FUNCTION GROB PROPS ARG1 ARG2 ... ) 

GROB is the current grob, PROPS is a list of alists, and ARG1.. are
the rest of the arguments.

The function should return a molecule (i.e. a formatted, ready to
print object).


To add a function, use the def-markup-command utility.

  (def-markup-command (mycommand paper prop arg1 ...) (arg1-type? ...)
    \"my command usage and description\"
    ...function body...)

The command is now available in markup mode, e.g.


  \\markup { .... \\MYCOMMAND #1 argument ... }

" ; "

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; markup definer utilities
;;; `def-markup-command' can be used both for built-in markup
;;; definitions and user defined markups.

(defmacro-public def-markup-command (command-and-args signature . body)
  "Define a COMMAND-markup function after command-and-args and body,
register COMMAND-markup and its signature,
add COMMAND-markup to markup-function-list,
sets COMMAND-markup markup-signature and markup-keyword object properties,
define a make-COMMAND-markup function.
Syntax:
  (def-markup-command (COMMAND paper props arg1 arg2 ...) (arg1-type? arg2-type? ...)
    \"documentation string\"
    ...command body...)
 or:
  (def-markup-command COMMAND (arg1-type? arg2-type? ...)
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
        (scm-error 'markup-format make-name
                   "Expect ~A arguments for ~A. Found ~A: ~S"
                   (list siglen make-name arglen args)
                   #f))
    (if error-msg
        (scm-error 'markup-format make-name
                   "Invalid argument in position ~A\nExpect: ~A\nFound: ~S."
                   error-msg #f)
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
 - #:center ( ... ) is used instead of \\center < ... >
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
  "Return a list of canonical markups expressions, eg:
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
  "Transform a keyword, eg. #:COMMAND, in a make-COMMAND-markup symbol."
  (string->symbol (string-append "make-" (symbol->string (keyword->symbol key)) "-markup")))

(define (compile-markup-expression expr)
  "Return two values: the first complete canonical markup expression found in `expr',
eg (make-COMMAND-markup arg1 arg2 ...), and the rest expression."
  (cond ((and (pair? expr)
              (keyword? (car expr)))
         ;; expr === (#:COMMAND arg1 ...)
         (let* ((command (symbol->string (keyword->symbol (car expr))))
                (sig (markup-command-signature (car (lookup-markup-command command))))
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
        (else
         ;; expr === (symbol ...) or ("string" ...) or ((funcall ...) ...)
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
;;; markup type predicates

(define (markup-function? x)
  (not (not (markup-command-signature x))))

(define (markup-list? arg)
  (define (markup-list-inner? l)
    (or (null? l)
        (and (markup? (car l)) (markup-list-inner? (cdr l)))))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; markup commands
;; TODO:
;; each markup function should have a doc string with
;; syntax, description and example. 
;;

(define-public brew-new-markup-molecule Text_item::brew_molecule)

(define-public interpret-markup Text_item::interpret_markup)

(def-markup-command (simple paper props str) (string?)
  "A simple text-string; @code{\\markup @{ foo @}} is equivalent with
@code{\\markup @{ \\simple #\"foo\" @}}.
"
  (interpret-markup paper props str))

(define-public empty-markup (make-simple-markup ""))

(define-public (stack-molecule-line space molecules)
  (if (pair? molecules)
      (if (pair? (cdr molecules))
          (let* ((tail (stack-molecule-line  space (cdr molecules)))
                 (head (car molecules))
                 (xoff (+ space (cdr (ly:molecule-get-extent head X)))))
            (ly:molecule-add head
                             (ly:molecule-translate-axis tail xoff X)))
          (car molecules))
      '()))

(def-markup-command (line paper props markps) (markup-list?)
  "A horizontal line of markups. Syntax:
\\line << MARKUPS >>
"
  (stack-molecule-line
   (cdr (chain-assoc 'word-space props))
   (map (lambda (m) (interpret-markup paper props m)) markps)))

(def-markup-command (combine paper props m1 m2) (markup? markup?)
  "Overstrike two markups."
  (ly:molecule-add
   (interpret-markup paper props m1)
   (interpret-markup paper props m2)))

(def-markup-command (finger paper props arg) (markup?)
  (interpret-markup paper
                    (cons '((font-size . -4) (font-family . number)) props)
                    arg))

(define-public (set-property-markup qualifier)
  (lambda (paper props qualifier-val markp)
    (interpret-markup paper
                      (cons (cons `(,qualifier . ,qualifier-val) (car props)) (cdr props))
                      markp)))

(def-markup-command fontsize (number? markup?)
  (set-property-markup 'font-size))

(def-markup-command magnify (number? markup?)
  (set-property-markup 'font-magnification))

(define (font-markup qualifier value)
  (lambda (paper props markp)
    (interpret-markup paper
                      (cons (cons `(,qualifier . ,value) (car props)) (cdr props))
                      markp)))

(def-markup-command bold (markup?)
  (font-markup 'font-series 'bold))

(def-markup-command sans (markup?)
  (font-markup 'font-family 'sans))

(def-markup-command number (markup?)
  (font-markup 'font-family 'number))

(def-markup-command roman (markup?)
  (font-markup 'font-family 'roman))

(def-markup-command huge (markup?)
  (font-markup 'font-size 2))

(def-markup-command large (markup?)
  (font-markup 'font-size 1))

(def-markup-command normalsize (markup?)
  (font-markup 'font-size 0))

(def-markup-command small (markup?)
  (font-markup 'font-size -1))

(def-markup-command tiny (markup?)
  (font-markup 'font-size -2))

(def-markup-command teeny (markup?)
  (font-markup 'font-size -3))

(def-markup-command dynamic (markup?)
  (font-markup 'font-family 'dynamic))

(def-markup-command italic (markup?)
  (font-markup 'font-shape 'italic))

(def-markup-command typewriter (markup?)
  (font-markup 'font-family 'typewriter))

(def-markup-command (doublesharp paper props) ()
  (interpret-markup paper props (markup #:musicglyph "accidentals-4")))
(def-markup-command (threeqsharp paper props) ()
  (interpret-markup paper props (markup #:musicglyph "accidentals-3")))
(def-markup-command (sharp paper props) ()
  (interpret-markup paper props (markup #:musicglyph "accidentals-2")))
(def-markup-command (semisharp paper props) ()
  (interpret-markup paper props (markup #:musicglyph "accidentals-1")))
(def-markup-command (natural paper props) ()
  (interpret-markup paper props (markup #:musicglyph "accidentals-0")))
(def-markup-command (semiflat paper props) ()
  (interpret-markup paper props (markup #:musicglyph "accidentals--1")))
(def-markup-command (flat paper props) ()
  (interpret-markup paper props (markup #:musicglyph "accidentals--2")))
(def-markup-command (threeqflat paper props) ()
  (interpret-markup paper props (markup #:musicglyph "accidentals--3")))
(def-markup-command (doubleflat paper props) ()
  (interpret-markup paper props (markup #:musicglyph "accidentals--4")))


(def-markup-command (column paper props mrkups) (markup-list?)
  (stack-lines
   -1 0.0 (cdr (chain-assoc 'baseline-skip props))
   (map (lambda (m) (interpret-markup paper props m)) mrkups)))

(def-markup-command (dir-column paper props mrkups) (markup-list?)
  "Make a column of args, going up or down, depending on the setting
of the #'direction layout property."
  (let* ((dir (cdr (chain-assoc 'direction props))))
    (stack-lines
     (if (number? dir) dir -1)
     0.0
     (cdr (chain-assoc 'baseline-skip props))
     (map (lambda (x) (interpret-markup paper props x)) mrkups))))

(def-markup-command (center paper props mrkups) (markup-list?)
  (let* ((mols (map (lambda (x) (interpret-markup paper props x)) mrkups))
         (cmols (map (lambda (x) (ly:molecule-align-to! x X CENTER)) mols)))
    (stack-lines -1 0.0 (cdr (chain-assoc 'baseline-skip props)) mols)))

(def-markup-command (right-align paper props mrkup) (markup?)
  (let* ((m (interpret-markup paper props mrkup)))
    (ly:molecule-align-to! m X RIGHT)
    m))

(def-markup-command (left-align paper props mrkup) (markup?)
  (let* ((m (interpret-markup paper props mrkup)))
    (ly:molecule-align-to! m X LEFT)
    m))

(def-markup-command (halign paper props dir mrkup) (number? markup?)
  "Set horizontal alignment. Syntax: halign A MARKUP. A=-1 is LEFT,
A=1 is right, values in between vary alignment accordingly."
  (let* ((m (interpret-markup paper props mrkup)))
    (ly:molecule-align-to! m X dir)
    m))

(def-markup-command (musicglyph paper props glyph-name) (string?)
  (ly:find-glyph-by-name
   (ly:paper-get-font paper (cons '((font-name . ())
                                    (font-shape . *)
                                    (font-series . *)
                                    (font-family . music))
                                  props))
   glyph-name))


(def-markup-command (lookup paper props glyph-name) (string?)
  "Lookup a glyph by name."
  (ly:find-glyph-by-name (ly:paper-get-font paper props)
                         glyph-name))

(def-markup-command (char paper props num) (integer?)
  "Syntax: \\char NUMBER. "
  (ly:get-glyph (ly:paper-get-font paper props) num))

(def-markup-command (raise paper props amount mrkup) (number? markup?)
  "Syntax: \\raise AMOUNT MARKUP. "
  (ly:molecule-translate-axis (interpret-markup paper props mrkup)
                              amount Y))

(def-markup-command (fraction paper props mrkup1 mrkup2) (markup? markup?)
  "Make a fraction of two markups.

Syntax: \\fraction MARKUP1 MARKUP2."
  (let* ((m1 (interpret-markup paper props mrkup1))
         (m2 (interpret-markup paper props mrkup2)))
    (ly:molecule-align-to! m1 X CENTER)
    (ly:molecule-align-to! m2 X CENTER)    
    (let* ((x1 (ly:molecule-get-extent m1 X))
           (x2 (ly:molecule-get-extent m2 X))
           (line (ly:round-filled-box (interval-union x1 x2) '(-0.05 . 0.05) 0.0))
           ;; should stack mols separately, to maintain LINE on baseline
           (stack (stack-lines -1 0.2 0.6 (list m1 line m2))))
      (ly:molecule-align-to! stack Y CENTER)
      (ly:molecule-align-to! stack X LEFT)
      ;; should have EX dimension
      ;; empirical anyway
      (ly:molecule-translate-axis stack 0.75 Y))))


;; TODO: better syntax.

(def-markup-command (note-by-number paper props log dot-count dir) (number? number? number?)
  "Syntax: \\note-by-number #LOG #DOTS #DIR.  By using fractional values
for DIR, you can obtain longer or shorter stems."
  (let* ((font (ly:paper-get-font paper (cons '((font-family .  music)) props)))
         (stemlen (max 3 (- log 1)))
         (headgl (ly:find-glyph-by-name
                  font
                  (string-append "noteheads-" (number->string (min log 2)))))
         (stemth 0.13)
         (stemy (* dir stemlen))
         (attachx (if (> dir 0)
                      (- (cdr (ly:molecule-get-extent headgl X)) stemth)
                      0))
         (attachy (* dir 0.28))
         (stemgl (and (> log 0)
                      (ly:round-filled-box
                       (cons attachx (+ attachx  stemth))
                       (cons (min stemy attachy)
                             (max stemy attachy))
                       (/ stemth 3))))
         (dot (ly:find-glyph-by-name font "dots-dot"))
         (dotwid (interval-length (ly:molecule-get-extent dot X)))
         (dots (and (> dot-count 0)
                    (apply ly:molecule-add
                           (map (lambda (x)
                                  (ly:molecule-translate-axis
                                   dot  (* (+ 1 (* 2 x)) dotwid) X) )
                                (iota dot-count 1)))))
         (flaggl (and (> log 2)
                      (ly:molecule-translate
                       (ly:find-glyph-by-name font
                                              (string-append "flags-"
                                                             (if (> dir 0) "u" "d")
                                                             (number->string log)))
                       (cons (+ attachx (/ stemth 2)) stemy)))))
    (if flaggl
        (set! stemgl (ly:molecule-add flaggl stemgl)))
    (if (ly:molecule? stemgl)
        (set! stemgl (ly:molecule-add stemgl headgl))
        (set! stemgl headgl))
    (if (ly:molecule? dots)
        (set! stemgl
              (ly:molecule-add
               (ly:molecule-translate-axis dots
                                           (+ (if (and (> dir 0) (> log 2))
                                                  (* 1.5 dotwid)
                                                  0)
                                              ;; huh ? why not necessary?
                                              ;;(cdr (ly:molecule-get-extent headgl X))
                                              dotwid)
                                           X)
               stemgl)))
    stemgl))

(use-modules (ice-9 regex))

(define-public log2 
  (let ((divisor (log 2)))
    (lambda (z) (inexact->exact (/ (log z) divisor)))))

(define (parse-simple-duration duration-string)
  "Parse the `duration-string', eg ''4..'' or ''breve.'', and return a (log dots) list."
  (let ((match (regexp-exec (make-regexp "(breve|longa|maxima|[0-9]+)(\\.*)") duration-string)))
    (if (and match (string=? duration-string (match:substring match 0)))
        (let ((len  (match:substring match 1))
              (dots (match:substring match 2)))
          (list (cond ((string=? len "breve")  -1)
                      ((string=? len "longa")  -2)
                      ((string=? len "maxima") -3)
                      (else (log2 (string->number len))))
                (if dots (string-length dots) 0)))
        (error "This is not a valid duration string:" duration-string))))

(def-markup-command (note paper props duration-string dir) (string? number?)
  "This produces a note with a stem pointing in @var{dir} direction, with
the @var{duration} for the note head type and augmentation dots. For
example, @code{\note #\"4.\" #-0.75} creates a dotted quarter note, with
a shortened down stem."
  (let ((parsed (parse-simple-duration duration-string)))
    (note-by-number-markup paper props (car parsed) (cadr parsed) dir)))

(def-markup-command (normal-size-super paper props mrkup) (markup?)
  (ly:molecule-translate-axis (interpret-markup
                               paper
                               props mrkup)
                              (* 0.5 (cdr (chain-assoc 'baseline-skip props)))
                              Y))

(def-markup-command (super paper props mrkup) (markup?)
  "Syntax: \\super MARKUP. "
  (ly:molecule-translate-axis
   (interpret-markup
    paper
    (cons `((font-size . ,(- (chain-assoc-get 'font-size props 0) 3))) props)
    mrkup)
   (* 0.5 (cdr (chain-assoc 'baseline-skip props)))
   Y))

(def-markup-command (translate paper props offset mrkup) (number-pair? markup?)
  "Syntax: \\translate OFFSET MARKUP. "
  (ly:molecule-translate (interpret-markup  paper props mrkup)
                         offset))

(def-markup-command (sub paper props mrkup) (markup?)
  "Syntax: \\sub MARKUP."
  (ly:molecule-translate-axis
   (interpret-markup
    paper
    (cons `((font-size . ,(- (chain-assoc-get 'font-size props 0) 3))) props)
    mrkup)
   (* -0.5 (cdr (chain-assoc 'baseline-skip props)))
   Y))

(def-markup-command (normal-size-sub paper props mrkup) (markup?)
  (ly:molecule-translate-axis
   (interpret-markup paper props mrkup)
   (* -0.5 (cdr (chain-assoc 'baseline-skip props)))
   Y))

(def-markup-command (hbracket paper props mrkup) (markup?)
  "Horizontal brackets around its single argument. Syntax \\hbracket MARKUP."  
  (let ((th 0.1) ;; todo: take from GROB.
        (m (interpret-markup paper props mrkup)))
    (bracketify-molecule m X th (* 2.5 th) th)))

(def-markup-command (bracket paper props mrkup) (markup?)
  "Vertical brackets around its single argument. Syntax \\bracket MARKUP."  
  (let ((th 0.1) ;; todo: take from GROB.
        (m (interpret-markup paper props mrkup)))
    (bracketify-molecule m Y th (* 2.5 th) th)))

;; todo: fix negative space
(def-markup-command (hspace paper props amount) (number?)
  "Syntax: \\hspace NUMBER."
  (if (> amount 0)
      (ly:make-molecule "" (cons 0 amount) '(-1 . 1) )
      (ly:make-molecule "" (cons amount amount) '(-1 . 1))))

(def-markup-command (override paper props new-prop mrkup) (pair? markup?)
  "Add the first argument in to the property list.  Properties may be
any sort of property supported by @ref{font-interface} and
@ref{text-interface}, for example

\\override #'(font-family . married) \"bla\"
"
  (interpret-markup paper (cons (list new-prop) props) mrkup))

(def-markup-command (smaller paper props mrkup) (markup?)
  "Syntax: \\smaller MARKUP"
  (let* ((fs (chain-assoc-get 'font-size props 0))
         (entry (cons 'font-size (- fs 1))))
    (interpret-markup paper (cons (list entry) props) mrkup)))


(def-markup-command (bigger paper props mrkup) (markup?)
  "Syntax: \\bigger MARKUP"
  (let* ((fs (chain-assoc-get 'font-size props 0))
         (entry (cons 'font-size (+ fs 1))))
    (interpret-markup paper (cons (list entry) props) mrkup)))

(def-markup-command larger (markup?)
  bigger-markup)

(def-markup-command (box paper props mrkup) (markup?)
  "Syntax: \\box MARKUP"
  (let ((th 0.1)
        (pad 0.2)
        (m (interpret-markup paper props mrkup)))
    (box-molecule m th pad)))

(def-markup-command (strut paper props) ()
  "Syntax: \\strut

 A box of the same height as the space.
"
  (let ((m (Text_item::interpret_markup paper props " ")))
    (ly:molecule-set-extent! m X '(1000 . -1000))
    m))

(define number->mark-letter-vector (make-vector 25 #\A))

(do ((i 0 (1+ i))
     (j 0 (1+ j)))
    ((>= i 26))
  (if (= i (- (char->integer #\I) (char->integer #\A)))
      (set! i (1+ i)))
  (vector-set! number->mark-letter-vector j
               (integer->char (+ i (char->integer #\A)))))

(define (number->markletter-string n)
  "Double letters for big marks."
  (let*
      ((l (vector-length number->mark-letter-vector)))
    
  (if (>= n l)
      (string-append (number->markletter-string (1- (quotient n l)))
                     (number->markletter-string (remainder n l)))
      (make-string 1 (vector-ref number->mark-letter-vector n)))))


(def-markup-command (markletter paper props num) (number?)
  "Markup letters: skip I and do double letters for big marks.
Syntax: \\markletter #25"
  (Text_item::interpret_markup paper props (number->markletter-string num)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if #f
    (define (typecheck-with-error x)
      (catch
       'markup-format
       (lambda () (markup? x))
       (lambda (key message arg)
         (display "\nERROR: markup format error: \n")
         (display message)
         (newline)
         (write arg (current-output-port))))))

;; test make-foo-markup functions
(if #f
    (begin
      (newline)
      (newline)
      (display (make-line-markup (list (make-simple-markup "FOO"))))
      
      (make-line-markup (make-simple-markup "FOO"))
      (make-line-markup (make-simple-markup "FOO") (make-simple-markup "foo"))
      (make-raise-markup "foo" (make-simple-markup "foo"))))

;;
;; test typecheckers. Not wholly useful, because errors are detected
;; in other places than they're made.
;;
(if #f
    (begin
      ;; To get error messages, see above to install the alternate
      ;; typecheck routine for markup?.
      (display (typecheck-with-error `(,simple-markup "foobar")))
      (display (typecheck-with-error `(,simple-markup "foobar")))
      (display (typecheck-with-error `(,simple-markup 1)))
      (display
       (typecheck-with-error `(,line-markup ((,simple-markup "foobar"))
                                            (,simple-markup 1))))
      (display
       (typecheck-with-error `(,line-markup (,simple-markup "foobar")
                                            (,simple-markup "bla"))))))
