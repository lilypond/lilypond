;;;; output-lib.scm -- implement Scheme output helper functions
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 1998--2005 Jan Nieuwenhuizen <janneke@gnu.org>
;;;; Han-Wen Nienhuys <hanwen@cs.uu.nl>


;;; Tablature functions, by Jiba (jiba@tuxfamily.org)

;; The TabNoteHead stem attachment function.
(define (tablature-stem-attachment-function style duration)
  (cons 0.0 1.35))

;; The TabNoteHead tablatureFormat callback.
;; Compute the text grob-property
(define-public (fret-number-tablature-format string tuning pitch)
  (make-whiteout-markup
   (make-vcenter-markup  
    (number->string
     (- (ly:pitch-semitones pitch)
	(list-ref tuning
		  ;; remove 1 because list index starts at 0 and guitar string at 1. 
		  (- string 1)))))))

;; The 5-string banjo has got a extra string, the fifth (duh), wich
;; starts at the fifth fret on the neck. Frets on the fifth string
;; are referred to relative to the other frets:
;;   the "first fret" on the fifth string is really the sixth fret
;;   on the banjo neck.
;; We solve this by defining a new fret-number-tablature function:
(define-public (fret-number-tablature-format-banjo string tuning pitch)
  (make-whiteout-markup
   (make-vcenter-markup  
    (let ((fret (- (ly:pitch-semitones pitch) (list-ref tuning (- string 1)))))
      (number->string (cond
		       ((and (> fret 0) (= string 5))
			(+ fret 5))
		       (else fret)))))))

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

	(set! text
	      (ly:make-stencil (ly:stencil-expr text)
			       (cons x x)
			       (ly:stencil-extent text Y)))
	
        (ly:stencil-aligned-to text X RIGHT)))))


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

(define-public (make-stencil-boxer thickness padding callback)
  "Return function that adds a box around the grob passed as argument."
  (lambda (grob) (box-stencil (callback grob) thickness padding)))

(define-public (make-stencil-circler thickness padding callback)
  "Return function that adds a circle around the grob passed as argument."
  (lambda (grob) (circle-stencil (callback grob) thickness padding)))

(define-public (arg->string arg)
  (cond ((number? arg) (ly:inexact->string arg 10))
	((string? arg) (string-append "\"" arg "\""))
	((symbol? arg) (string-append "\"" (symbol->string arg) "\""))))

(define-public (print-circled-text-callback grob)
  (let* ((text (ly:grob-property grob 'text))
	 (layout (ly:grob-layout grob))
	 (defs (ly:output-def-lookup layout 'text-font-defaults))
	 (props (ly:grob-alist-chain grob defs))
	 (circle (Text_interface::interpret_markup
		  layout props (make-draw-circle-markup 0.8 0.1 #f)))
	 (text-stencil (Text_interface::interpret_markup layout props text)))
    
    (ly:stencil-add (centered-stencil text-stencil) circle)))


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
    ((petrucci)
     (if (< duration 0)
	 (string-append (number->string duration) "mensural")
	 (string-append (number->string duration) (symbol->string style))))
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
(define-safe-public (begin-of-line-visible d) (if (= d 1) '(#f . #f) '(#t . #t)))
(define-safe-public (end-of-line-visible d) (if (= d -1) '(#f . #f) '(#t . #t)))
(define-safe-public (spanbar-begin-of-line-invisible d) (if (= d -1) '(#t . #t) '(#f . #f)))

(define-safe-public (all-visible d) '(#f . #f))
(define-safe-public (all-invisible d) '(#t . #t))
(define-safe-public (begin-of-line-invisible d) (if (= d 1) '(#t . #t) '(#f . #f)))
(define-safe-public (center-invisible d) (if (= d 0) '(#t . #t) '(#f . #f)))
(define-safe-public (end-of-line-invisible d) (if (= d -1) '(#t . #t) '(#f . #f)))


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
	(ly:warning (_ "unknown bar glyph: `~S'" glyph))
	(index-cell (cdr result) dir))))

(define-public (shift-right-at-line-begin g)
  "Shift an item to the right, but only at the start of the line."
  (if (and (ly:item? g)  (equal? (ly:item-break-dir g) RIGHT))
      (ly:grob-translate-axis! g 3.5 X)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Color

(define-public color? list?)

; predefined colors
(define-public black       '(0.0 0.0 0.0))
(define-public white       '(1.0 1.0 1.0))
(define-public red         '(1.0 0.0 0.0))
(define-public green       '(0.0 1.0 0.0))
(define-public blue        '(0.0 0.0 1.0))
(define-public cyan        '(1.0 1.0 0.0))
(define-public magenta     '(1.0 0.0 1.0))
(define-public yellow      '(0.0 1.0 1.0))

(define-public grey        '(0.5 0.5 0.5))
(define-public darkred     '(0.5 0.0 0.0))
(define-public darkgreen   '(0.0 0.5 0.0))
(define-public darkblue    '(0.0 0.0 0.5))
(define-public darkcyan    '(0.5 0.5 0.0))
(define-public darkmagenta '(0.5 0.0 0.5))
(define-public darkyellow  '(0.0 0.5 0.5))
