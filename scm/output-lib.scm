;;;; output-lib.scm -- implement Scheme output helper functions
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 1998--2001 Jan Nieuwenhuizen <janneke@gnu.org>
;;;; Han-Wen Nienhuys <hanwen@cs.uu.nl>

; Tablature functions, by Jiba (jiba@tuxfamily.org)

; The TabNoteHead stem attachment function.
(define (tablature-stem-attachment-function style duration)
  (cons 0.0 0.5) ;; UGH!
)

; The TabNoteHead molecule callback.
; Create a text molecule
(define-public (tablature-molecule-callback grob)
  (let ((molecule (fontify-text
                   (ly-get-default-font grob)
                   (ly-get-grob-property grob 'text)
                   )))
    molecule ; return the molecule.
    )
  )

; The TabNoteHead tablatureFormat callback.
; Compute the text grob-property
(define-public (fret-number-tablature-format string tuning pitch)
  (number->string
   (- (pitch-semitones pitch)
      (list-ref tuning
                (- string 1) ; remove 1 because list index starts at 0 and guitar string at 1.
                )
      )
   )
  )

(define-public (hammer-molecule-callback grob)
  (let* ((note-collums (ly-get-grob-property grob 'note-columns))
         (note-column1 (cadr note-collums))
         (note-column2 (car  note-collums))
         (note1        (car (ly-get-grob-property note-column1 'note-heads)))
         (note2        (car (ly-get-grob-property note-column2 'note-heads)))
         (fret1        (string->number (ly-get-grob-property note1 'text)))
         (fret2        (string->number (ly-get-grob-property note2 'text)))
         (letter       (if (< fret1 fret2) "H"
                       (if (> fret1 fret2) "P"
                                           "")))
         )
    (let ((slur (Slur::brew_molecule grob))
          (text (fontify-text (ly-get-default-font grob) letter)))
    
      (let ((x (/ (- (cdr (ly-get-molecule-extent slur 0)) 
                     (/ (cdr (ly-get-molecule-extent text 0)) 2.0)
                     )
                  -2.0)))
      
        (ly-set-molecule-extent! text 0 (cons x x))
        (ly-align-to! text 0 1)
        )
      
      (ly-combine-molecule-at-edge slur 1 1 text -0.6)
      )
    )
  )



(define-public guitar-tunings '(4 -1 -5 -10 -15 -20))

; end of tablature functions


(define-public (make-molecule-boxer line-thick x-padding y-padding callback)
   "Makes a routine that adds a box around the grob parsed as argument"
  (define (molecule-boxer grob)
  (let*
   (
    (mol    (callback grob))
    (x-ext (widen-interval (ly-get-molecule-extent mol 0) x-padding))
    (y-ext (widen-interval (ly-get-molecule-extent mol 1) y-padding))
    (x-rule (box-molecule (widen-interval x-ext line-thick)
                              (cons 0 line-thick)))
    (y-rule (box-molecule (cons 0 line-thick) y-ext))
    )
    
    (set! mol (ly-combine-molecule-at-edge mol 0 1 y-rule x-padding))
    (set! mol (ly-combine-molecule-at-edge mol 0 -1  y-rule x-padding))
    (set! mol (ly-combine-molecule-at-edge mol 1 1  x-rule 0))  
    (set! mol (ly-combine-molecule-at-edge mol 1 -1 x-rule 0))
    
    mol
 ))
 molecule-boxer
 )


(define (arg->string arg)
  (cond ((number? arg) (inexact->string arg 10))
	((string? arg) (string-append "\"" arg "\""))
	((symbol? arg) (string-append "\"" (symbol->string arg) "\""))))

;; ugh: naming.
(define (func name . args)
  (string-append 
   "(" name 
   (if (null? args) 
       ""
       (apply string-append 
	      (map (lambda (x) (string-append " " (arg->string x))) args)))
   ")\n"))


;;(define (mm-to-pt x)
;;  (* (/ 72.27 25.40) x))

;; do nothing in .scm output
(define (comment s) "")

(define-public (numbers->string l)
  (apply string-append (map ly-number->string l)))

; (define (chop-decimal x) (if (< (abs x) 0.001) 0.0 x))

(define (number->octal-string x)
  (let* ((n (inexact->exact x))
         (n64 (quotient n 64))
         (n8 (quotient (- n (* n64 64)) 8)))
    (string-append
     (number->string n64)
     (number->string n8)
     (number->string (remainder (- n (+ (* n64 64) (* n8 8))) 8)))))

(define-public (inexact->string x radix)
  (let ((n (inexact->exact x)))
    (number->string n radix)))


(define-public (number-pair->string c)
  (string-append (number->string (car c)) " "
		 (number->string (cdr c)) " "))

(define (font i)
  (string-append
   "font"
   (make-string 1 (integer->char (+ (char->integer #\A) i)))))

(define (scm-scm action-name)
  1)


;; silly, use alist? 
(define-public (find-notehead-symbol duration style)
  (case style
   ((xcircle) "2xcircle")
   ((harmonic) "0neo_mensural")
   ((baroque) 
    ;; Oops, I actually would not call this "baroque", but, for
    ;; backwards compatibility to 1.4, this is supposed to take
    ;; brevis, longa and maxima from the neo-mensural font and all
    ;; other note heads from the default font.  -- jr
    (if (< duration 0)
	(string-append (number->string duration) "neo_mensural")
	(number->string duration)))
   ((mensural)
    (string-append (number->string duration) (symbol->string style)))
   ((neo_mensural)
    (string-append (number->string duration) (symbol->string style)))
   ((default)
    ;; The default font in mf/feta-bolletjes.mf defines a brevis, but
    ;; neither a longa nor a maxima.  Hence let us, for the moment,
    ;; take these from the neo-mensural font.  TODO: mf/feta-bolletjes
    ;; should define at least a longa for the default font.  The longa
    ;; should look exactly like the brevis of the default font, but
    ;; with a stem exactly like that of the quarter note. -- jr
    (if (< duration -1)
	(string-append (number->string duration) "neo_mensural")
	(number->string duration)))
   (else
    (if (string-match "vaticana*|hufnagel*|medicaea*" (symbol->string style))
	(symbol->string style)
	(string-append (number->string (max 0 duration))
		       (symbol->string style))))))


(define (note-head-style->attachment-coordinates style duration)
  "Return pair (X . Y), containing multipliers for the note head
bounding box, where to attach the stem. e.g.: X==0 means horizontally
centered, X==1 is at the right, X == -1 is at the left."

  (case style
    ((default)
     (if (< duration -1)
	 '(0.0 . 0.6) ;; neo-mensural
	 '(1.0 . 0.5) ;; default
	 ))
    ((cross) '(1.0 . 0.75))
    ((mensural) '(0.0 . 0.6))
    ((neo_mensural) '(0.0 . 0.6))
    ((diamond) '(1.0 . 0.8))
    ((transparent) '(1.0 . 1.0))
    ((slash) '(1.0 . 1.0))
    ((harmonic) '(1.0 0.0))
    ((triangle) '(0.75 . 0.15))
    ((baroque)
     (if (< duration 0)
	 '(0.0 . 0.6) ;; neo-mensural
	 '(1.0 . 0.5) ;; default
	 ))
    (else

     ;; this also works for easy notation.
     '(1.0 . 0.0)
     )))

(define-public (string-encode-integer i)
  (cond
   ((= i  0) "o")
   ((< i 0)   (string-append "n" (string-encode-integer (- i))))
   (else (string-append
	  (make-string 1 (integer->char (+ 65 (modulo i 26))))
	  (string-encode-integer (quotient i 26))))))


(define ((every-nth-bar-number-visible n) barnum) (= 0 (modulo barnum n)))

(define-public (default-bar-number-visibility barnum) (> barnum 1))

;; See documentation of Item::visibility_lambda_
(define-public (begin-of-line-visible d) (if (= d 1) '(#f . #f) '(#t . #t)))
(define-public (end-of-line-visible d) (if (= d -1) '(#f . #f) '(#t . #t)))
(define-public (spanbar-begin-of-line-invisible d) (if (= d -1) '(#t . #t) '(#f . #f)))

(define-public (all-visible d) '(#f . #f))
(define-public (all-invisible d) '(#t . #t))
(define-public (begin-of-line-invisible d) (if (= d 1) '(#t . #t) '(#f . #f)))
(define-public (end-of-line-invisible d) (if (= d -1) '(#t . #t) '(#f . #f)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bar lines.

;
; How should a  bar line behave at a break? 
;
;; Why prepend `default-' to every scm identifier?
(define-public (default-break-barline glyph dir)
   (let ((result (assoc glyph 
			'((":|:" . (":|" . "|:"))
			  ("||:" . ("||" . "|:"))
			  ("|" . ("|" . ()))
			  ("||:" . ("||" . "|:"))
			  ("|s" . (() . "|"))
			  ("|:" . ("|" . "|:"))
			  ("|." . ("|." . ()))

			  ;; hmm... should we end with a barline here?
			  (".|" . ("|" . ".|"))
			  (":|" . (":|" . ()))
			  ("||" . ("||" . ()))
			  (".|." . (".|." . ()))
			  ("" . ("" . ""))
			  ("empty" . (() . ()))
			  ("brace" . (() . "brace"))
			  ("bracket" . (() . "bracket"))  
			  )
			)))

     (if (equal? result #f)
	 (ly-warn (string-append "Unknown bar glyph: `" glyph "'"))
	 (index-cell (cdr result) dir))
     )
   )
     
