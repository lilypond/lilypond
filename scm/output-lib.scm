;;;; output-lib.scm -- implement Scheme output helper functions
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 1998--2001 Jan Nieuwenhuizen <janneke@gnu.org>
;;;; Han-Wen Nienhuys <hanwen@cs.uu.nl>

; Tablature functions, by Jiba (jiba@tuxfamily.org)

; The TabNoteHead stem attachment function.
(define (tablature-stem-attachment-function style duration)
  (cons 0.0 1.0)
)

; The TabNoteHead molecule callback.
; Create a text molecule
(define (tablature-molecule-callback grob)
  (let ((molecule (fontify-text
                   (ly-get-default-font grob)
                   (ly-get-grob-property grob 'text)
                   )))
    molecule ; return the molecule.
    )
  )

; The TabNoteHead tablatureFormat callback.
; Compute the text grob-property
(define (fret-number-tablature-format string tuning pitch)
  (number->string
   (- (pitch-semitones pitch)
      (list-ref tuning
                (- string 1) ; remove 1 because list index starts at 0 and guitar string at 1.
                )
      )
   )
  )

(define (hammer-molecule-callback grob)
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




; end of tablature functions


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

(define (numbers->string l)
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

(define (inexact->string x radix)
  (let ((n (inexact->exact x)))
    (number->string n radix)))


(define (control->string c)
  (string-append (number->string (car c)) " "
		 (number->string (cdr c)) " "))

(define (font i)
  (string-append
   "font"
   (make-string 1 (integer->char (+ (char->integer #\A) i)))))

(define (scm-scm action-name)
  1)


;; silly, use alist? 
(define (find-notehead-symbol duration style)
  (case style
   ((xcircle) (cons "2xcircle" "music"))
   ((harmonic) (cons "0neo_mensural" "music"))
   ((baroque) 
    ;; Oops, I actually would not call this "baroque", but, for
    ;; backwards compatibility to 1.4, this is supposed to take
    ;; brevis, longa and maxima from the neo-mensural font and all
    ;; other note heads from the default font.  -- jr
    (if (< duration 0)
	(cons (string-append (number->string duration) "neo_mensural") "music")
	(cons (number->string duration) "music")))
   ((mensural)
    (cons (string-append (number->string duration) (symbol->string style))
     "music"))
   ((neo_mensural)
    (cons (string-append (number->string duration) (symbol->string style))
     "music"))
   ((default)
    ;; The default font in mf/feta-bolletjes.mf defines a brevis, but
    ;; neither a longa nor a maxima.  Hence let us, for the moment,
    ;; take these from the neo-mensural font.  TODO: mf/feta-bolletjes
    ;; should define at least a longa for the default font.  The longa
    ;; should look exactly like the brevis of the default font, but
    ;; with a stem exactly like that of the quarter note. -- jr
    (if (< duration -1)
	(cons (string-append (number->string duration) "neo_mensural") "music")
	(cons (number->string duration) "music")))
   (else
    (if (string-match "vaticana*|hufnagel*|medicaea*" (symbol->string style))
	(cons (symbol->string style) "music")
	(cons (string-append (number->string (max 0 duration))
			     (symbol->string style))
	      "music")))))


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

(define (find-timesig-symbol nom denom style)
  (case style
   ((mensural)
    (cons (string-append
	     "mensural"
	     (number->string nom)
	     "/"
	     (number->string denom))
	  "ancient"))
   ((neo_mensural)
    (cons (string-append
	     "neo_mensural"
	     (number->string nom)
	     "/"
	     (number->string denom))
	  "ancient"))
   ((numbered)
    (cons (string-append
	   (number->string nom)
	   "/"
	   (number->string denom))
	  "music"))
   (else
    ;; default: use "C" style when possible, otherwise return ""
    (cons
     (case nom
       ((2)
	(case denom
	  ((2) "C2/2")
	  (else "")))
       ((4)
	(case denom
	  ((4) "C4/4")
	  (else "")))
       (else ""))
     "music"))))

(define (string-encode-integer i)
  (cond
   ((= i  0) "o")
   ((< i 0)   (string-append "n" (string-encode-integer (- i))))
   (else (string-append
	  (make-string 1 (integer->char (+ 65 (modulo i 26))))
	  (string-encode-integer (quotient i 26))))))





