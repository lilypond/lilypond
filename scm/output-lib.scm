;;;; output-lib.scm -- implement Scheme output helper functions
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 1998--2007 Jan Nieuwenhuizen <janneke@gnu.org>
;;;; Han-Wen Nienhuys <hanwen@xs4all.nl>




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general
(define-public (grob::has-interface grob iface)
  (memq iface (ly:grob-interfaces grob)))

(define-public (make-stencil-boxer thickness padding callback)

  "Return function that adds a box around the grob passed as argument."
  (lambda (grob)
    
    (box-stencil (callback grob) thickness padding)))

(define-public (make-stencil-circler thickness padding callback)
  "Return function that adds a circle around the grob passed as argument."
  
  (lambda (grob) (circle-stencil (callback grob) thickness padding)))

(define-public (print-circled-text-callback grob)
  (grob-interpret-markup grob (make-circle-markup
		   (ly:grob-property grob 'text))
	     ))

(define-public (event-cause grob)
  (let*
      ((cause (ly:grob-property  grob 'cause)))
    
    (cond
     ((ly:stream-event? cause) cause)
     ((ly:grob? cause) (event-cause cause))
     (else #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tablature

;; The TabNoteHead tablatureFormat callback.
;; Compute the text grob-property
(define-public (fret-number-tablature-format string
					     context event)
  (let*
      ((tuning (ly:context-property context 'stringTunings))
       (pitch (ly:event-property event 'pitch))
       (is-harmonic (apply
		     functional-or
		     (map
		      (lambda (ev)
			(eq? 'harmonic-event (ly:event-property ev 'class)))
		      (ly:event-property event 'articulations)))))

    
    (make-whiteout-markup
     (make-vcenter-markup
      (format
       "~a"
       (- (ly:pitch-semitones pitch)
	  (list-ref tuning
		    ;; remove 1 because list index starts at 0 and guitar string at 1. 
		    (- string 1))))))
    ))

;; The 5-string banjo has got a extra string, the fifth (duh), wich
;; starts at the fifth fret on the neck. Frets on the fifth string
;; are referred to relative to the other frets:
;;   the "first fret" on the fifth string is really the sixth fret
;;   on the banjo neck.
;; We solve this by defining a new fret-number-tablature function:
(define-public (fret-number-tablature-format-banjo string 
					     context event)
  (let*
      ((tuning (ly:context-property context 'stringTunings))
       (pitch (ly:event-property event 'pitch))
	)
  (make-whiteout-markup
   (make-vcenter-markup  
    (let ((fret (- (ly:pitch-semitones pitch) (list-ref tuning (- string 1)))))
      (number->string (cond
		       ((and (> fret 0) (= string 5))
			(+ fret 5))
		       (else fret))))))
  ))


; default tunings for common string instruments
(define-public guitar-tuning '(4 -1 -5 -10 -15 -20))
(define-public guitar-open-g-tuning '(2 -1 -5 -10 -17 -22))
(define-public bass-tuning '(-17 -22 -27 -32))
(define-public mandolin-tuning '(16 9 2 -5))

;; tunings for 5-string banjo
(define-public banjo-open-g-tuning '(2 -1 -5 -10 7))
(define-public banjo-c-tuning '(2 -1 -5 -12 7))
(define-public banjo-modal-tuning '(2 0 -5 -10 7))
(define-public banjo-open-d-tuning '(2 -3 -6 -10 9))
(define-public banjo-open-dm-tuning '(2 -3 -6 -10 9))
;; convert 5-string banjo tuning to 4-string by removing the 5th string
(define-public (four-string-banjo tuning)
  (reverse (cdr (reverse tuning))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; note heads


(define-public (stem::calc-duration-log grob)
  (ly:duration-log
   (ly:event-property (event-cause grob) 'duration)))

(define-public (note-head::calc-duration-log grob)
  (min 2 
       (ly:duration-log
	(ly:event-property (event-cause grob) 'duration))))

(define-public (dots::calc-dot-count grob)
  (ly:duration-dot-count
   (ly:event-property (event-cause grob) 'duration)))

(define-public (dots::calc-staff-position grob)
  (let*
      ((head (ly:grob-parent grob Y))
       (log (ly:grob-property head 'duration-log)))

    (cond
     ((or (not (grob::has-interface head 'rest-interface))
	 (not (integer? log))) 0)
     ((= log 7) 4)
     ((> log 4) 3)
     ((= log 0) -1)
     ((= log 1) 1)
     ((= log -1) 1)
     (else 0))))

(define (note-head::calc-tablature-stem-attachment grob)
  (cons 0.0 1.35))



;; silly, use alist? 
(define-public (note-head::calc-glyph-name grob)
  (let*
      ((style (ly:grob-property grob 'style))
       (log (min 2 (ly:grob-property grob 'duration-log))))
    
    (case style
      ;; "default" style is directly handled in note-head.cc as a
      ;; special case (HW says, mainly for performance reasons).
      ;; Therefore, style "default" does not appear in this case
      ;; statement.  -- jr
      ((xcircle) "2xcircle")
      ((harmonic) "0harmonic")
      ((harmonic-black) "2harmonic")
      ((harmonic-mixed) (if (<= log 1) "0harmonic"
                                       "2harmonic"))
      ((baroque) 
       ;; Oops, I actually would not call this "baroque", but, for
       ;; backwards compatibility to 1.4, this is supposed to take
       ;; brevis, longa and maxima from the neo-mensural font and all
       ;; other note heads from the default font.  -- jr
       (if (< log 0)
	   (string-append (number->string log) "neomensural")
	   (number->string log)))
      ((mensural)
       (string-append (number->string log) (symbol->string style)))
      ((petrucci)
       (if (< log 0)
	   (string-append (number->string log) "mensural")
	   (string-append (number->string log) (symbol->string style))))
      ((neomensural)
       (string-append (number->string log) (symbol->string style)))
      (else
       (if (string-match "vaticana*|hufnagel*|medicaea*" (symbol->string style))
	   (symbol->string style)
	   (string-append (number->string (max 0 log))
			  (symbol->string style)))))))

;; TODO junk completely?
(define (note-head-style->attachment-coordinates grob axis)
  "Return pair (X . Y), containing multipliers for the note head
bounding box, where to attach the stem. e.g.: X==0 means horizontally
centered, X==1 is at the right, X == -1 is at the left."

  '(1.0 . 0.0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bar numbers

(define-public ((every-nth-bar-number-visible n) barnum) (= 0 (modulo barnum n)))

(define-public ((modulo-bar-number-visible n m) barnum) (and (> barnum 1) (= m (modulo barnum n))))

(define-public ((set-bar-number-visibility n) tr)
  (let* ((bn (ly:context-property tr 'currentBarNumber)))
    (ly:context-set-property! tr 'barNumberVisibility (modulo-bar-number-visible n (modulo bn n)))))

(define-public (first-bar-number-invisible barnum) (> barnum 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; break visibility

(define-public all-visible             #(#t #t #t))
(define-public begin-of-line-invisible #(#t #t #f))
(define-public center-invisible        #(#t #f #t))
(define-public end-of-line-invisible   #(#f #t #t))
(define-public begin-of-line-visible   #(#f #f #t))
(define-public center-visible          #(#f #t #f))
(define-public end-of-line-visible     #(#t #f #f))
(define-public all-invisible           #(#f #f #f))

(define-public spanbar-begin-of-line-invisible
  #(#t #f #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bar lines.

;;
;; How should a  bar line behave at a break? 
(define bar-glyph-alist
  '((":|:" . (":|" . "|:"))
    (":|.|:" . (":|" . "|:"))
    (":|.:" . (":|" . "|:"))
    ("||:" . ("||" . "|:"))
    ("dashed" . ("dashed" . '())) 
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
    ("|.|" . ("|.|" . ()))
    ("" . ("" . ""))
    (":" . (":" . ""))
    ("." . ("." . ()))
    ("empty" . (() . ()))
    ("brace" . (() . "brace"))
    ("bracket" . (() . "bracket")) 
    ))

(define-public (bar-line::calc-glyph-name grob)
  (let* (
	 (glyph (ly:grob-property grob 'glyph))
	 (dir (ly:item-break-dir grob))
	 (result (assoc glyph  bar-glyph-alist))
	 (glyph-name (if (= dir CENTER)
			 glyph
		         (if (and result (string? (index-cell (cdr result) dir)))
			     (index-cell (cdr result) dir)
			     #f)))
	 )
    glyph-name))

(define-public (bar-line::calc-break-visibility grob)
  (let* ((glyph (ly:grob-property grob 'glyph))
	 (result (assoc glyph bar-glyph-alist)))
    (if result
	(vector (string? (cadr result)) #t (string? (cddr result)))
	#(#f #f #f))))


(define-public (shift-right-at-line-begin g)
  "Shift an item to the right, but only at the start of the line."
  (if (and (ly:item? g)
	   (equal? (ly:item-break-dir g) RIGHT))
      (ly:grob-translate-axis! g 3.5 X)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tuplets

(define-public (tuplet-number::calc-denominator-text grob)
  (number->string (ly:event-property (event-cause grob) 'denominator)))

(define-public (tuplet-number::calc-fraction-text grob)
  (let*
      ((ev (event-cause grob)))

    (format "~a:~a" 
	    (ly:event-property ev 'denominator)
	    (ly:event-property ev 'numerator))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Color

(define-public color? list?)
(define-public (rgb-color r g b) (list r g b))

; predefined colors
(define-public black       '(0.0 0.0 0.0))
(define-public white       '(1.0 1.0 1.0))
(define-public red         '(1.0 0.0 0.0))
(define-public green       '(0.0 1.0 0.0))
(define-public blue        '(0.0 0.0 1.0))
(define-public cyan        '(0.0 1.0 1.0))
(define-public magenta     '(1.0 0.0 1.0))
(define-public yellow      '(1.0 1.0 0.0))

(define-public grey        '(0.5 0.5 0.5))
(define-public darkred     '(0.5 0.0 0.0))
(define-public darkgreen   '(0.0 0.5 0.0))
(define-public darkblue    '(0.0 0.0 0.5))
(define-public darkcyan    '(0.0 0.5 0.5))
(define-public darkmagenta '(0.5 0.0 0.5))
(define-public darkyellow  '(0.5 0.5 0.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; key signature


(define-public (key-signature-interface::alteration-position step alter c0-position)
  ;; TODO: memoize - this is mostly constant.
  
  ;; fes, ges, as and bes typeset in lower octave
  (define FLAT_TOP_PITCH 2)
  
  ;; ais and bis typeset in lower octave
  (define SHARP_TOP_PITCH 4)

  (if (pair? step)
      (+ (cdr step) (* (car step) 7)  c0-position)
      (let*
	  ((from-bottom-pos (modulo (+ 4 49 c0-position)  7))
	   (p step)
	   (c0 (- from-bottom-pos  4)))
	
	(if
	 (or (and (< alter 0) (or (> p FLAT_TOP_PITCH) (> (+ p c0) 4)) (> (+ p c0) 1))
	     (and (> alter 0) (or (> p SHARP_TOP_PITCH) (> (+ p c0) 5)) (> (+ p c0) 2))
	     )

	 ;; Typeset below c_position 
	 (set! p (- p 7)))

	;; Provide for the four cases in which there's a glitch
	;; it's a hack, but probably not worth
	;; the effort of finding a nicer solution.
	;; --dl. 
	(cond
	 ((and (= c0 2) (= p 3) (> alter 0))
	  (set! p (- p 7)))
	 ((and (= c0 -3) (= p -1) (> alter 0))
	  (set! p (+ p 7)))
	 ((and (= c0 -4) (= p -1) (< alter 0))
	  (set! p (+ p 7)))
	 ((and (= c0 -2) (= p -3) (< alter 0))
	  (set! p (+ p 7))))

	(+ c0 p))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; accidentals

(define-public (accidental-interface::calc-alteration grob)
  (ly:pitch-alteration  (ly:event-property (event-cause grob) 'pitch)))


(define-public cancellation-glyph-name-alist
  '((0 . "accidentals.natural")))

(define-public standard-alteration-glyph-name-alist
     '(
       ;; ordered for optimal performance.
       (0 . "accidentals.natural")
       (-1/2 . "accidentals.flat")
       (1/2 . "accidentals.sharp")

       (1 . "accidentals.doublesharp")
       (-1 . "accidentals.flatflat")
       
       (3/4 . "accidentals.sharp.slashslash.stemstemstem")
       (1/4 . "accidentals.sharp.slashslash.stem")
       (-1/4 . "accidentals.mirroredflat")
       (-3/4 . "accidentals.mirroredflat.flat")
       ))

;; FIXME: standard vs default, alteration-FOO vs FOO-alteration
(define-public alteration-default-glyph-name-alist standard-alteration-glyph-name-alist)

(define-public makam-alteration-glyph-name-alist
     '((1 . "accidentals.doublesharp")
       (8/9 . "accidentals.sharp.slashslashslash.stemstem")
       (5/9 . "accidentals.sharp.slashslashslash.stem")
       (4/9 . "accidentals.sharp")
       (1/9 . "accidentals.sharp.slashslash.stem")
       (0 . "accidentals.natural")
       (-1/9 . "accidentals.mirroredflat")
       (-4/9 . "accidentals.flat.slash")
       (-5/9 . "accidentals.flat")
       (-8/9 . "accidentals.flat.slashslash")
       (-1 . "accidentals.flatflat")
       ))
  
(define-public alteration-hufnagel-glyph-name-alist
   '((-1/2 . "accidentals.hufnagelM1")
     (0 . "accidentals.vaticana0")
     (1/2 . "accidentals.mensural1")))

(define-public alteration-medicaea-glyph-name-alist
   '((-1/2 . "accidentals.medicaeaM1")
     (0 . "accidentals.vaticana0")
     (1/2 . "accidentals.mensural1")))

(define-public alteration-vaticana-glyph-name-alist
   '((-1/2 . "accidentals.vaticanaM1")
     (0 . "accidentals.vaticana0")
     (1/2 . "accidentals.mensural1")))

(define-public alteration-mensural-glyph-name-alist
   '((-1/2 . "accidentals.mensuralM1")
     (0 . "accidentals.vaticana0")
     (1/2 . "accidentals.mensural1")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Pitch Trill Heads
;; * Parentheses

(define-public (parentheses-item::calc-parenthesis-stencils grob)
  (let* ((font (ly:grob-default-font grob))
	 (lp (ly:font-get-glyph font "accidentals.leftparen"))
	 (rp (ly:font-get-glyph font "accidentals.rightparen")))

    (list lp rp)))


(define-public (grob-interpret-markup grob text)
  (let*
      ((layout (ly:grob-layout grob))
       (defs (ly:output-def-lookup layout 'text-font-defaults))
       (props (ly:grob-alist-chain grob defs)))

    (ly:text-interface::interpret-markup
     layout props text)))

(define-public (parentheses-item::calc-angled-bracket-stencils grob)
  (let* (
	 (font (ly:grob-default-font grob))
	 (lp (ly:stencil-aligned-to (ly:stencil-aligned-to (grob-interpret-markup grob (ly:wide-char->utf-8 #x2329))
							   Y CENTER)  X RIGHT))
	 (rp (ly:stencil-aligned-to (ly:stencil-aligned-to (grob-interpret-markup grob (ly:wide-char->utf-8 #x232A))
							   Y CENTER) X LEFT))
	 )

    (list (stencil-whiteout lp)
	  (stencil-whiteout rp))))

(define (parenthesize-elements grob . rest)
  (let*
      ((refp (if (null? rest)
		 grob
		 (car rest)))
       (elts (ly:grob-object grob 'elements))
       (x-ext (ly:relative-group-extent elts refp X))
       (stencils (ly:grob-property grob 'stencils))
       (lp (car stencils))
       (rp (cadr stencils))
       (padding (ly:grob-property grob 'padding 0.1)))
    
    (ly:stencil-add
     (ly:stencil-translate-axis lp (- (car x-ext) padding) X)
     (ly:stencil-translate-axis rp (+ (cdr x-ext) padding) X))
  ))


(define (parentheses-item::print me)
  (let*
      ((elts (ly:grob-object me 'elements))
       (y-ref (ly:grob-common-refpoint-of-array me elts Y))
       (x-ref (ly:grob-common-refpoint-of-array me elts X))
       (stencil (parenthesize-elements me x-ref))
       (elt-y-ext  (ly:relative-group-extent elts y-ref Y))
       (y-center (interval-center elt-y-ext)))

    (ly:stencil-translate
     stencil
     (cons
      (-
       (ly:grob-relative-coordinate me x-ref X))
      (-
       y-center
       (ly:grob-relative-coordinate me y-ref Y))))
    ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 

(define-public (chain-grob-member-functions grob value . funcs)
  (for-each
   (lambda (func)
     (set! value (func grob value)))
   funcs)

  value)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; falls/doits

(define-public (bend::print spanner)
  (define (close  a b)
    (< (abs (- a b)) 0.01))
  
  (let*
      ((delta-y (* 0.5 (ly:grob-property spanner 'delta-position)))
       (left-span (ly:spanner-bound spanner LEFT))
       (dots (if (and (grob::has-interface left-span 'note-head-interface)
		      (ly:grob? (ly:grob-object left-span 'dot)))
		 (ly:grob-object left-span 'dot) #f))
		 
       (right-span (ly:spanner-bound spanner RIGHT))
       (thickness (* (ly:grob-property spanner 'thickness)
		     (ly:output-def-lookup (ly:grob-layout spanner)
					   'line-thickness)))
       (padding (ly:grob-property spanner 'padding 0.5))
       (common (ly:grob-common-refpoint right-span
					(ly:grob-common-refpoint spanner
								 left-span X)
					X))
       (common-y (ly:grob-common-refpoint spanner left-span Y))
       (left-x (+ padding
		  (max (interval-end (ly:grob-robust-relative-extent
				      left-span common X))
		        (if (and
			     dots
			     (close (ly:grob-relative-coordinate dots common-y Y)
					(ly:grob-relative-coordinate spanner common-y Y)))
			    (interval-end (ly:grob-robust-relative-extent dots common X))
			    -10000) ;; TODO: use real infinity constant.
			)))
       (right-x (- (interval-start
		    (ly:grob-robust-relative-extent right-span common X))
		   padding))
       (self-x (ly:grob-relative-coordinate spanner common X))
       (dx (- right-x left-x))
       (exp (list 'path thickness 
		  `(quote
		    (rmoveto
		     ,(- left-x self-x) 0

		     rcurveto		     
		     ,(/ dx 3)
		     0
		     ,dx ,(* 0.66 delta-y)
		     ,dx ,delta-y
		     )))))

    (ly:make-stencil
     exp
     (cons (- left-x self-x) (- right-x self-x))
     (cons (min 0 delta-y)
	   (max 0 delta-y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grace spacing


(define-public (grace-spacing::calc-shortest-duration grob)
  (let*
     ((cols (ly:grob-object grob 'columns))
      (get-difference
       (lambda (idx)
	 (ly:moment-sub (ly:grob-property
			 (ly:grob-array-ref cols (1+ idx)) 'when)
			(ly:grob-property
			 (ly:grob-array-ref cols idx) 'when))))
      
      (moment-min (lambda (x y)
		    (cond
		     ((and x y)
		      (if (ly:moment<? x y)
			    x
			    y))
		     (x x)
		     (y y)))))
		     
    (fold moment-min #f (map get-difference
			     (iota (1- (ly:grob-array-length cols)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fingering

(define-public (fingering::calc-text grob)
  (let*
      ((event (event-cause grob))
       (digit (ly:event-property event 'digit)))
    
    (if (> digit 5)
	(ly:input-message (ly:event-property event 'origin)
			  "Warning: Fingering notation for finger number ~a" digit))

    (number->string digit 10)
  ))

(define-public (string-number::calc-text grob)
  (let*
      ((digit (ly:event-property (event-cause  grob) 'string-number)))
    
    (number->string digit 10)
  ))


(define-public (stroke-finger::calc-text grob)
  (let*
      ((digit (ly:event-property (event-cause grob) 'digit))
       (text (ly:event-property (event-cause grob) 'text)))

    (if (string? text)
	text
	(vector-ref (ly:grob-property grob 'digit-names)  (1- (max (min 5 digit) 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dynamics
(define-public (hairpin::calc-grow-direction grob)
  (if (eq? (ly:event-property (event-cause grob) 'class) 'decrescendo-event)
      START
      STOP
      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lyrics

(define-public (lyric-text::print grob)
  "Allow interpretation of tildes as lyric tieing marks."
  
  (let*
      ((text (ly:grob-property grob 'text)))

    (grob-interpret-markup grob 
	       (if (string? text)
		   (make-tied-lyric-markup text)
		   text))))

(define-public ((grob::calc-property-by-copy prop) grob)
  (ly:event-property (event-cause grob) prop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fret boards

(define-public (fret-board::calc-stencil grob)
    (grob-interpret-markup 
      grob
      (make-fret-diagram-verbose-markup
        (ly:grob-property grob 'dot-placement-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scripts

(define-public (script-interface::calc-x-offset grob)
  (ly:grob-property grob 'positioning-done)
  (ly:self-alignment-interface::centered-on-x-parent grob))
