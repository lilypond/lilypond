"
Internally markup is stored as lists, whose head is a function.

  (FUNCTION ARG1 ARG2 ... )

When the markup is formatted, then FUNCTION is called as follows

  (FUNCTION GROB PROPS ARG1 ARG2 ... ) 

GROB is the current grob, PROPS is a list of alists, and ARG1.. are
the rest of the arguments.

The function should return a molecule (i.e. a formatted, ready to
print object).



To add a function,

1. It should be named  COMMAND-markup

2. It should have an object property set that describes it's
signature. This is to allow the parser to figure out how many
arguments to expect:

  (set-object-property! COMMAND-markup  scm0-markup1)

(insert in the list below).

3. The command is now available in markup mode, e.g.


  \\markup { .... \\COMMAND #1 argument ... }


BUGS:

At present, markup functions must be defined in this
file. Implementing user-access for markup functions is an excercise
for the reader.


 

" ; " 


;;;;;;;;;;;;;;;;;
;; TODO:
;; each markup function should have a doc string with
;; syntax, description and example. 
;;

(define-public (simple-markup paper props . rest)
  (Text_item::interpret_markup paper props (car rest)))

(define-public (stack-molecule-line space molecules)
  (if (pair? molecules)
      (if (pair? (cdr molecules))
	  (let* (
		 (tail (stack-molecule-line  space (cdr molecules)))
		 (head (car molecules))
		 (xoff (+ space (cdr (ly:molecule-get-extent head X))))
		 )
	    
	    (ly:molecule-add
	     head
	     (ly:molecule-translate-axis tail xoff X))
	  )
	  (car molecules))
      '())
  )

(define-public (line-markup paper props . rest)
  "A horizontal line of markups. Syntax:
\\line << MARKUPS >>
"
  
  (stack-molecule-line
   (cdr (chain-assoc 'word-space props))
   (map (lambda (x) (interpret-markup paper props x)) (car rest)))
  )


(define-public (combine-markup paper props . rest)
  (ly:molecule-add
   (interpret-markup paper props (car rest))
   (interpret-markup paper props (cadr rest))))
  
(define (font-markup qualifier value)
  (lambda (paper props . rest)
    (interpret-markup paper (cons (cons `(,qualifier . ,value) (car props)) (cdr props)) (car rest))
  
  ))


(define-public (set-property-markup qualifier)
  (lambda (paper props . rest  )
    (interpret-markup paper
		      (cons (cons `(,qualifier . ,(car rest))
				  (car props)) (cdr props))
		      (cadr rest))
    ))

(define-public (finger-markup paper props . rest)
  (interpret-markup paper
		    (cons (list '(font-relative-size . -3)
				'(font-family . number))
				props)
		    (car rest)))

(define-public fontsize-markup (set-property-markup 'font-relative-size))
(define-public magnify-markup (set-property-markup 'font-magnification))

(define-public bold-markup
  (font-markup 'font-series 'bold))
(define-public number-markup
  (font-markup 'font-family 'number))
(define-public roman-markup
  (font-markup 'font-family 'roman))


(define-public huge-markup
  (font-markup 'font-relative-size 2))
(define-public large-markup
  (font-markup 'font-relative-size 1))
(define-public small-markup
  (font-markup 'font-relative-size -1))
(define-public tiny-markup
  (font-markup 'font-relative-size -2))
(define-public teeny-markup
  (font-markup 'font-relative-size -3))
(define-public dynamic-markup
  (font-markup 'font-family 'dynamic))
(define-public italic-markup
  (font-markup 'font-shape 'italic))
(define-public typewriter-markup
  (font-markup 'font-family 'typewriter))


;; TODO: baseline-skip should come from the font.
(define-public (column-markup paper props . rest)
  (stack-lines
   -1 0.0 (cdr (chain-assoc 'baseline-skip props))
   (map (lambda (x) (interpret-markup paper props x)) (car rest)))
  )

(define-public (dir-column-markup paper props . rest)
  "Make a column of args, going up or down, depending on DIRECTION."
  (let*
      (
       (dir (cdr (chain-assoc 'direction props)))
       )
    (stack-lines
     (if (number? dir) dir -1)
     0.0 (cdr (chain-assoc 'baseline-skip props))
     (map (lambda (x) (interpret-markup paper props x)) (car rest)))
    ))

(define-public (center-markup paper props . rest)
  (let*
    (
     (mols (map (lambda (x) (interpret-markup paper props x)) (car rest)))
     (cmols (map (lambda (x) (ly:molecule-align-to! x X CENTER)) mols))
     )
    
    (stack-lines
     -1 0.0 (cdr (chain-assoc 'baseline-skip props))
     mols)
    ))

(define-public (right-align-markup paper props . rest)
  (let* ((m (interpret-markup paper props (car rest))))
    (ly:molecule-align-to! m X RIGHT)
    m))

(define-public (halign-markup paper props . rest)
  "Set horizontal alignment. Syntax: haling A MARKUP. A=-1 is LEFT,
A=1 is right, values in between vary alignment accordingly."
  (let* ((m (interpret-markup paper props (cadr rest))))
    (ly:molecule-align-to! m X (car rest))
    m))

(define-public (left-align-markup paper props . rest)
  (let* ((m (interpret-markup paper props (car rest))))
    (ly:molecule-align-to! m X LEFT)
    m))

(define-public (musicglyph-markup paper props . rest)
  (ly:find-glyph-by-name
   (ly:paper-get-font paper (cons '((font-name . ())
				    (font-shape . *)
				    (font-series . *)
				    (font-family . music)) props))
   (car rest)))


(define-public (lookup-markup paper props . rest)
  "Lookup a glyph by name."
  (ly:find-glyph-by-name
   (ly:paper-get-font paper  props)
   (car rest))
  )

(define-public (char-markup paper props . rest)
  "Syntax: \\char NUMBER. "
  (ly:get-glyph  (ly:paper-get-font paper props) (car rest))
  )

(define-public (raise-markup paper props  . rest)
  "Syntax: \\raise AMOUNT MARKUP. "
  (ly:molecule-translate-axis (interpret-markup
			       paper
			       props
			       (cadr rest))
			      (car rest) Y))

(define-public (fraction-markup paper props . rest)
  "Make a fraction of two markups.

Syntax: \\fraction MARKUP1 MARKUP2."

  (let*
      ((m1 (interpret-markup paper props (car rest)))
       (m2 (interpret-markup paper props (cadr rest))))

    (ly:molecule-align-to! m1 X CENTER)
    (ly:molecule-align-to! m2 X CENTER)
    
    (let*
	((x1 (ly:molecule-get-extent m1 X))
	 (x2 (ly:molecule-get-extent m2 X))
	 (line (ly:round-filled-box (interval-union x1 x2) '(-0.05 . 0.05) 0.0))

	 ;; should stack mols separately, to maintain LINE on baseline
	 (stack (stack-lines -1 0.2 0.6 (list m1 line m2))))

      (ly:molecule-align-to! stack Y CENTER)
      (ly:molecule-align-to! stack X LEFT)
      ;; should have EX dimension
      ;; empirical anyway
      (ly:molecule-translate-axis stack 0.75 Y) 
      )))


(define-public (note-markup paper props . rest)
  "Syntax: \\note #LOG #DOTS #DIR.  By using fractional values
for DIR, you can obtain longer or shorter stems."
 
  (let*
      (
       (log (car rest))
       (dot-count (cadr rest))
       (dir (caddr rest))
       (font (ly:paper-get-font paper (cons '((font-family .  music)) props)))
       (stemlen (max 3 (- log 1)))
       (headgl
	(ly:find-glyph-by-name font (string-append "noteheads-" (number->string (min log 2)))))

       (stemth 0.13)
       (stemy (* dir stemlen))
       (attachx (if (> dir 0) (- (cdr (ly:molecule-get-extent headgl X)) stemth)
		    0))
       (attachy (* dir 0.28))
       (stemgl (if (> log 0)
		   (ly:round-filled-box
				     (cons attachx (+ attachx  stemth))
				     (cons (min stemy attachy)
					   (max stemy attachy))
				    (/ stemth 3)
				    ) #f))
       (dot (ly:find-glyph-by-name font "dots-dot"))
       (dotwid  (interval-length (ly:molecule-get-extent dot X)))
       (dots (if (> dot-count 0)
		 (apply ly:molecule-add
		  (map (lambda (x)
			 (ly:molecule-translate-axis
			  dot  (* (+ 1 (* 2 x)) dotwid) X) )
		       (iota dot-count 1)))
		 #f))
       
       (flaggl (if (> log 2)
		   (ly:molecule-translate
		    (ly:find-glyph-by-name
		     font
		     (string-append "flags-"
				    (if (> dir 0) "u" "d")
				    (number->string log)
				    ))
		    (cons (+ attachx (/ stemth 2)) stemy))

		    #f)))
    
    (if flaggl
	(set! stemgl (ly:molecule-add flaggl stemgl)))

    (if (ly:molecule? stemgl)
	(set! stemgl (ly:molecule-add stemgl headgl))
        (set! stemgl headgl)
	)
    
    (if (ly:molecule? dots)
	(set! stemgl
	      (ly:molecule-add
	       (ly:molecule-translate-axis
		dots
		(+
		 (if (and (> dir 0) (> log 2))
		     (* 1.5 dotwid) 0)
		 ;; huh ? why not necessary?
		;(cdr (ly:molecule-get-extent headgl X))
		      dotwid
		 )
		X)
	       stemgl 
	       )
	      ))

    stemgl
    ))

(define-public (normal-size-super-markup paper props . rest)
  (ly:molecule-translate-axis (interpret-markup
			       paper
			       props (car rest))
			      (* 0.5 (cdr (chain-assoc 'baseline-skip props)))
			      Y)
  )

(define-public (super-markup paper props  . rest)
  "Syntax: \\super MARKUP. "
  (ly:molecule-translate-axis (interpret-markup
			       paper
			       (cons '((font-relative-size . -2)) props) (car rest))
			      (* 0.5 (cdr (chain-assoc 'baseline-skip props)))
			      Y)
  )

(define-public (translate-markup paper props . rest)
  "Syntax: \\translate OFFSET MARKUP. "
  (ly:molecule-translate (interpret-markup  paper props (cadr rest))
			 (car rest))

  )

(define-public (sub-markup paper props  . rest)
  "Syntax: \\sub MARKUP."
  (ly:molecule-translate-axis (interpret-markup
			       paper
			       (cons '((font-relative-size . -2)) props)
			       (car rest))
			      (* -0.5 (cdr (chain-assoc 'baseline-skip props)))
			      Y)
  )

(define-public (normal-size-sub-markup paper props . rest)
  (ly:molecule-translate-axis (interpret-markup
			       paper
			       props (car rest))
			      (* -0.5 (cdr (chain-assoc 'baseline-skip props)))
			      Y)
  )

(define-public (hbracket-markup paper props . rest)
  "Horizontal brackets around its single argument. Syntax \\hbracket MARKUP."  
  
  (let*
      (
       (th 0.1) ;; todo: take from GROB.
       (m (interpret-markup paper props (car rest)))
       )

    (bracketify-molecule m X th (* 2.5 th) th)  
))

(define-public (bracket-markup paper props . rest)
  "Vertical brackets around its single argument. Syntax \\bracket MARKUP."  
  (let*
      (
       (th 0.1) ;; todo: take from GROB.
       (m (interpret-markup paper props (car rest)))
       )

    (bracketify-molecule m Y th (* 2.5 th) th)  
))

;; todo: fix negative space
(define (hspace-markup paper props . rest)
  "Syntax: \\hspace NUMBER."
  (let*
      ((amount (car rest)))
    (if (> amount 0)
	(ly:make-molecule "" (cons 0 amount) '(-1 . 1) )
	(ly:make-molecule "" (cons amount amount) '(-1 . 1)))
  ))

(define-public (override-markup paper props . rest)
  "Tack the 1st arg in REST onto PROPS, e.g.

\\override #'(font-family . married) \"bla\"

"
  
  (interpret-markup paper (cons (list (car rest)) props)
		    (cadr rest)))

(define-public (smaller-markup  paper props . rest)
  "Syntax: \\smaller MARKUP"
  (let*
      (
       (fs (cdr (chain-assoc 'font-relative-size props)))
       (entry (cons 'font-relative-size (- fs 1)))
       )
    (interpret-markup
     paper (cons (list entry) props)
     (car rest))
    ))

(define-public (bigger-markup  paper props . rest)
  "Syntax: \\bigger MARKUP"
  (let*
      (
       (fs (cdr (chain-assoc 'font-relative-size props)))
       (entry (cons 'font-relative-size (+ fs 1)))
       )
  (interpret-markup
   paper (cons (list entry) props)
   (car rest))
  ))

(define-public (box-markup paper props . rest)
  "Syntax: \\box MARKUP"
  (let*
      (
       (th 0.1)
       (pad 0.2)
       (m (interpret-markup paper props (car rest)))
       )
    (box-molecule m th pad)
  ))


(define-public (strut-markup paper props . rest)
  "Syntax: \\strut

 A box of the same height as the space.
"

  (let*
      ((m (Text_item::interpret_markup paper props " ")))

    (ly:molecule-set-extent! m 0 '(1000 . -1000))
    m))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (markup-signature-to-keyword sig)
  " (A B C) -> a0-b1-c2 "
  
  (let* ((count  0))
    (string->symbol (string-join
     
     (map
     (lambda (func)
       (set! count (+ count 1))
       (string-append

	;; for reasons I don't get,
	;; (case func ((markup?) .. )
	;; doesn't work.
	(cond 
	  ((eq? func markup?) "markup")
	  ((eq? func markup-list?) "markup-list")
	  (else "scheme")
	  )
	(number->string (- count 1))
	))
     
     sig)
     "-"))

  ))


(define (markup-function? x)
  (object-property x 'markup-signature)
  )

(define (markup-list? arg)
  (define (markup-list-inner? l)
    (if (null? l)
	#t
	(and (markup? (car l)) (markup-list-inner? (cdr l)))
    )
  )
  (and (list? arg) (markup-list-inner? arg)))

(define (markup-argument-list? signature arguments)
  "Typecheck argument list."
  (if (and (pair? signature) (pair? arguments))
      (and ((car signature) (car arguments))
	   (markup-argument-list? (cdr signature) (cdr arguments)))
      (and (null? signature) (null? arguments)))
  )


(define (markup-argument-list-error signature arguments number)
  "return (ARG-NR TYPE-EXPECTED ARG-FOUND) if an error is detected, or
#f is no error found.
"
  (if (and (pair? signature) (pair? arguments))
      (if (not ((car signature) (car arguments)))
	  (list number (type-name (car signature)) (car arguments))
	  (markup-argument-list-error (cdr signature) (cdr arguments) (+ 1 number)))
      #f
  ))

;;
;; full recursive typecheck.
;;
(define (markup-typecheck? arg)
  (or (string? arg)
      (and (pair? arg)
       (markup-function? (car arg))
       (markup-argument-list?
	(object-property (car arg) 'markup-signature)
	(cdr arg))
  ))
)

;; 
;; typecheck, and throw an error when something amiss.
;; 
(define (markup-thrower-typecheck arg)
  (cond
   ((string? arg) #t)
   ((not (pair? arg))
    (throw 'markup-format "Not a pair" arg)
    )
   ((not (markup-function? (car arg)))
    (throw 'markup-format "Not a markup function " (car arg)))
   
  
   ((not (markup-argument-list? 
	  (object-property (car arg) 'markup-signature)
	  (cdr arg)))
    (throw 'markup-format "Arguments failed  typecheck for " arg)))
   #t
  )

;;
;; good enough if you only  use make-XXX-markup functions.
;; 
(define (cheap-markup? x)
  (or (string? x)
      (and (pair? x)
	   (markup-function? (car x))))
)

;;
;; replace by markup-thrower-typecheck for more detailed diagnostics.
;; 
(define markup?  cheap-markup?)

(define markup-functions-and-signatures
  (list

   ;; abs size
   (cons teeny-markup (list markup?))
   (cons tiny-markup (list markup?))
   (cons small-markup (list markup?))
   (cons dynamic-markup (list markup?))
   (cons large-markup (list markup?)) 
   
   (cons huge-markup (list markup?))

   ;; size
   (cons smaller-markup (list markup?))
   (cons bigger-markup (list markup?))
;   (cons char-number-markup (list string?))
   
   ;; 
   (cons sub-markup (list markup?))
   (cons normal-size-sub-markup (list markup?))
   
   (cons super-markup (list markup?))
   (cons normal-size-super-markup (list markup?))

   (cons finger-markup (list markup?))
   (cons bold-markup (list markup?))
   (cons italic-markup (list markup?))
   (cons typewriter-markup (list markup?))
   (cons roman-markup (list markup?))
   (cons number-markup (list markup?))
   (cons hbracket-markup  (list markup?))
   (cons bracket-markup  (list markup?))
   (cons note-markup (list integer? integer? ly:dir?))
   (cons fraction-markup (list markup? markup?))
   
   (cons column-markup (list markup-list?))
   (cons dir-column-markup (list markup-list?))
   (cons center-markup (list markup-list?))
   (cons line-markup  (list markup-list?))

   (cons right-align-markup (list markup?))
   (cons left-align-markup (list markup?))   
   (cons halign-markup (list number? markup?))
   
   (cons combine-markup (list markup? markup?))
   (cons simple-markup (list string?))
   (cons musicglyph-markup (list scheme?))
   (cons translate-markup (list number-pair? markup?))
   (cons override-markup (list pair? markup?))
   (cons char-markup (list integer?))
   (cons lookup-markup (list string?))
   
   (cons hspace-markup (list number?))

   (cons raise-markup (list number? markup?))
   (cons magnify-markup (list number? markup?))
   (cons fontsize-markup (list number? markup?))

   (cons box-markup  (list markup?))
   (cons strut-markup '())
   ))


(define markup-module (current-module))

(map (lambda (x)
       (set-object-property! (car x) 'markup-signature (cdr x))
       (set-object-property! (car x) 'markup-keyword (markup-signature-to-keyword (cdr x)))
       )
     markup-functions-and-signatures)

(define-public markup-function-list (map car markup-functions-and-signatures))


;; construct a
;;
;; make-FOO-markup function that typechecks its arguments.
;;
;; TODO: should construct a message says
;; Invalid argument 4 : expecting a BLADIBLA, found: (list-ref 4 args)
;;
;; right now, you get the entire argument list.


(define (make-markup-maker  entry)
  (let*
	((foo-markup (car entry))
	 (signature (cons 'list (cdr entry)))
	 (name (symbol->string (procedure-name foo-markup)))
	 (make-name  (string-append "make-" name))
	 )
      
      `(define (,(string->symbol make-name) . args)
	 (let*
	     (
	      (arglen (length  args))
	      (siglen (length ,signature))
	      (error-msg
	       (if (and (> 0 siglen) (> 0 arglen))
		   (markup-argument-list-error ,signature args 1)))
	      
	      )
	 
	 (if (or (not (= arglen siglen)) (< siglen 0) (< 0 arglen))
	     (scm-error 'markup-format ,make-name "Expect ~A arguments for ~A. Found ~A: ~S"
			(list (length ,signature)
			      ,make-name
			      (length args)
			      args) #f))
	 (if error-msg
	     (scm-error 'markup-format ,make-name "Invalid argument in position ~A\n Expect: ~A\nFound: ~S." error-msg #f)
	     
	     (cons ,foo-markup args)
	     )))
    )
)



(define (make-markup markup-function make-name signature args)
  
  " Construct a markup object from MARKUP-FUNCTION and ARGS. Typecheck
against SIGNATURE, reporting MAKE-NAME as the user-invoked function.
"

  (let*
      ((arglen (length args))
       (siglen (length signature))
       (error-msg
	(if (and (> siglen 0) (> arglen 0))
	    (markup-argument-list-error signature args 1)
	    #f)))


    (if (or (not (= arglen siglen)) (< siglen 0) (< arglen 0))
	(scm-error 'markup-format make-name "Expect ~A arguments for ~A. Found ~A: ~S"
		   (list siglen
			 make-name
			 arglen
			 args) #f))

    (if error-msg
	(scm-error 'markup-format make-name "Invalid argument in position ~A\nExpect: ~A\nFound: ~S." error-msg #f)
	
	(cons markup-function  args)
	)))

(define (make-markup-maker entry)
  (let* (
	 (name (symbol->string (procedure-name (car entry))))
	 (make-name  (string-append "make-" name))
	 (signature (object-property (car entry) 'markup-signature))
	 )
  
    `(define-public (,(string->symbol make-name) . args)
       (make-markup ,(car entry) ,make-name ,(cons 'list signature)  args)
       ))
  )

(eval
 (cons 'begin (map make-markup-maker markup-functions-and-signatures))
 markup-module
 )

;;
;; TODO: add module argument so user-defined markups can also be 
;; processed.
;;
(define-public (lookup-markup-command code)
  (let*
      ((sym (string->symbol (string-append code "-markup")))
       (var (module-local-variable markup-module sym))
       )
    (if (eq? var #f)
	#f   
	(cons (variable-ref var) (object-property  (variable-ref var) 'markup-keyword))
    )
  ))


(define-public brew-new-markup-molecule Text_item::brew_molecule)

(define-public empty-markup (make-simple-markup ""))

(define-public interpret-markup Text_item::interpret_markup)


;;;;;;;;;;;;;;;;
;; utility

(define (markup-join markups sep)
  "Return line-markup of MARKUPS, joining them with markup SEP"
  (if (pair? markups)
      (make-line-markup (list-insert-separator markups sep))
      empty-markup))


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
	(write arg (current-output-port))
	)
      )))

;; test make-foo-markup functions
(if #f
    (begin
      (newline)
      (newline)
      (display (make-line-markup (list (make-simple-markup "FOO"))))
      
      (make-line-markup (make-simple-markup "FOO"))
      (make-line-markup (make-simple-markup "FOO") (make-simple-markup "foo"))
      (make-raise-markup "foo" (make-simple-markup "foo"))
      )
    )


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
    (typecheck-with-error  `(,line-markup ((,simple-markup "foobar"))
					  (,simple-markup 1))))
   (display
    (typecheck-with-error  `(,line-markup (,simple-markup "foobar")
					 (,simple-markup "bla"))))
   
   ))
