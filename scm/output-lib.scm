;;;; output-lib.scm -- implement Scheme output helper functions
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c)  1998--2004 Jan Nieuwenhuizen <janneke@gnu.org>
;;;; Han-Wen Nienhuys <hanwen@cs.uu.nl>


;;; Tablature functions, by Jiba (jiba@tuxfamily.org)

;; The TabNoteHead stem attachment function.
(define (tablature-stem-attachment-function style duration)
  (cons 0.0 0.5))

;; The TabNoteHead tablatureFormat callback.
;; Compute the text grob-property
(define-public (fret-number-tablature-format string tuning pitch)
  (number->string
   (- (ly:pitch-semitones pitch)
      (list-ref tuning
		;; remove 1 because list index starts at 0 and guitar string at 1. 
                (- string 1)))))

;; The 5-string banjo has got a extra string, the fifth (duh), wich
;; starts at the fifth fret on the neck. Frets on the fifth string
;; are referred to relative to the other frets:
;;   the "first fret" on the fifth string is really the sixth fret
;;   on the banjo neck.
;; We solve this by defining a new fret-number-tablature function:
(define-public (fret-number-tablature-format-banjo string tuning pitch)
    (let ((fret (- (ly:pitch-semitones pitch) (list-ref tuning (- string 1)))))
        (number->string (cond
            ((and (> fret 0) (= string 5))
                (+ fret 5))
            (else fret)))))

(define-public (hammer-print-function grob)
  (let* ((note-collums (ly:grob-property grob 'note-columns))
         (note-column1 (cadr note-collums))
         (note-column2 (car  note-collums))
         (note1 (car (ly:grob-property note-column1 'note-heads)))
         (note2 (car (ly:grob-property note-column2 'note-heads)))
	 (text1 (ly:grob-property note1 'text))
	 (text2 (ly:grob-property note2 'text))
         (fret1 (if (string? text1) (string->number text1) 0))
         (fret2 (if (string? text2) (string->number text2) 0))
         (letter (cond
		  ((< fret1 fret2) "H")
		  ((> fret1 fret2) "P")
		  (else ""))))
    (let* ((slur
	    ;; (Slur::print grob)

	    ;; 
	    ;; FIXME: a hammer is not a slur.
	    ;; 
	    (ly:make-stencil '() '(0 . 0) '(0 . 0)))
	   (layout (ly:grob-layout grob))
	   (text (interpret-markup
		  layout
		  (ly:grob-alist-chain grob (ly:output-def-lookup layout 'text-font-defaults))
		  letter)))
      
      (let ((x (/ (- (cdr (ly:stencil-extent slur 0)) 
                     (/ (cdr (ly:stencil-extent text 0)) 2.0))
                  -2.0)))
	
        (ly:stencil-set-extent! text 0 (cons x x))
        (ly:stencil-align-to! text 0 1)))))

;; (ly:stencil-combine-at-edge slur 1 1 text -0.6)



(define-public guitar-tuning '(4 -1 -5 -10 -15 -20))
(define-public bass-tuning '(-17 -22 -27 -32))

;; tunings for 5-string banjo
(define-public banjo-open-g-tuning '(2 -1 -5 -10 7))
(define-public banjo-c-tuning '(2 -1 -5 -12 7))
(define-public banjo-modal-tuning '(2 0 -5 -10 7))
(define-public banjo-open-d-tuning '(2 -3 -6 -10 9))
(define-public banjo-open-dm-tuning '(2 -3 -6 -10 9))
;; convert 5-string banjo tunings to 4-string tunings by
;; removing the 5th string
;;
;; example:
;; \set TabStaff.stringTunings = #(four-string-banjo banjo-open-g-tuning)
(define-public (four-string-banjo tuning)
  (reverse (cdr (reverse tuning))))

;;; end of tablature functions


(define-public (make-stencil-boxer line-thick x-padding y-padding callback)
  "Makes a routine that adds a box around the grob parsed as argument"
  (define (stencil-boxer grob)
    (let* ((mol (callback grob))
	   (x-ext (interval-widen (ly:stencil-extent mol 0) x-padding))
	   (y-ext (interval-widen (ly:stencil-extent mol 1) y-padding))
	   (x-rule (make-filled-box-stencil (interval-widen x-ext line-thick)
					    (cons 0 line-thick)))
	   (y-rule (make-filled-box-stencil (cons 0 line-thick) y-ext)))
      
      (set! mol (ly:stencil-combine-at-edge mol 0 1 y-rule x-padding))
      (set! mol (ly:stencil-combine-at-edge mol 0 -1 y-rule x-padding))
      (set! mol (ly:stencil-combine-at-edge mol 1 1 x-rule 0))  
      (set! mol (ly:stencil-combine-at-edge mol 1 -1 x-rule 0))
      mol))
  stencil-boxer)

(define-public (arg->string arg)
  (cond ((number? arg) (ly:inexact->string arg 10))
	((string? arg) (string-append "\"" arg "\""))
	((symbol? arg) (string-append "\"" (symbol->string arg) "\""))))

(define-public (func name . args)
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

(define-public (ly:numbers->string lst)
  (string-join (map ly:number->string lst) " "))

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
    ((harmonic) "0harmonic")
    ((baroque) 
     ;; Oops, I actually would not call this "baroque", but, for
     ;; backwards compatibility to 1.4, this is supposed to take
     ;; brevis, longa and maxima from the neo-mensural font and all
     ;; other note heads from the default font.  -- jr
     (if (< duration 0)
	 (string-append (number->string duration) "neomensural")
	 (number->string duration)))
    ((mensural)
     (string-append (number->string duration) (symbol->string style)))
    ((neomensural)
     (string-append (number->string duration) (symbol->string style)))
    ((default)
     ;; The default font in mf/feta-bolletjes.mf defines a brevis, but
     ;; neither a longa nor a maxima.  Hence let us, for the moment,
     ;; take these from the neo-mensural font.  TODO: mf/feta-bolletjes
     ;; should define at least a longa for the default font.  The longa
     ;; should look exactly like the brevis of the default font, but
     ;; with a stem exactly like that of the quarter note. -- jr
     (if (< duration -1)
	 (string-append (number->string duration) "neomensural")
	 (number->string duration)))
    (else
     (if (string-match "vaticana*|hufnagel*|medicaea*" (symbol->string style))
	 (symbol->string style)
	 (string-append (number->string (max 0 duration))
			(symbol->string style))))))

;; TODO junk completely?
(define (note-head-style->attachment-coordinates grob axis)
  "Return pair (X . Y), containing multipliers for the note head
bounding box, where to attach the stem. e.g.: X==0 means horizontally
centered, X==1 is at the right, X == -1 is at the left."

  '(1.0 . 0.0))

(define-public (string-encode-integer i)
  (cond
   ((= i  0) "o")
   ((< i 0)   (string-append "n" (string-encode-integer (- i))))
   (else (string-append
	  (make-string 1 (integer->char (+ 65 (modulo i 26))))
	  (string-encode-integer (quotient i 26))))))

(define-public ((every-nth-bar-number-visible n) barnum) (= 0 (modulo barnum n)))

(define-public ((modulo-bar-number-visible n m) barnum) (and (> barnum 1) (= m (modulo barnum n))))

(define-public ((set-bar-number-visibility n) tr)
  (let* ((bn (ly:context-property tr 'currentBarNumber)))
    (ly:context-set-property! tr 'barNumberVisibility (modulo-bar-number-visible n (modulo bn n)))))

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

;;
;; How should a  bar line behave at a break? 
;;
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

			 ;; hmm... should we end with a bar line here?
			 (".|" . ("|" . ".|"))
			 (":|" . (":|" . ()))
			 ("||" . ("||" . ()))
			 (".|." . (".|." . ()))
			 ("" . ("" . ""))
			 (":" . (":" . ""))
			 ("empty" . (() . ()))
			 ("brace" . (() . "brace"))
			 ("bracket" . (() . "bracket"))  ))))

    (if (equal? result #f)
	(ly:warn "Unknown bar glyph: `~S'" glyph)
	(index-cell (cdr result) dir))))

(define-public (shift-right-at-line-begin g)
  "Shift an item to the right, but only at the start of the line."
  (if (and (ly:item? g)  (equal? (ly:item-break-dir g) RIGHT))
      (ly:grob-translate-axis! g 3.5 X)))
