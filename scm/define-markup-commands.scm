;;;; define-markup-commands.scm -- markup commands
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c)  2000--2004  Han-Wen Nienhuys <hanwen@cs.uu.nl>
;;;;                  Jan Nieuwenhuizen <janneke@gnu.org>

;;; markup commands
;;; TODO:
;;;  * each markup function should have a doc string with
;;     syntax, description and example. 


(def-markup-command (word paper props str) (string?)
  "A single word."
  (interpret-markup paper props str))
  
(def-markup-command (simple paper props str) (string?)
  "A simple text-string; @code{\\markup @{ foo @}} is equivalent with
@code{\\markup @{ \\simple #\"foo\" @}}."
    (interpret-markup paper props
		      (make-word-markup str)))

; todo: use font recoding.
;		      (make-line-markup
;		       (map make-word-markup (string-tokenize str)))))

(define-public empty-markup
  (make-simple-markup ""))

;;(def-markup-command (fill-line paper props line-width markups)
;;  (number? markup-list?)
;; no parser tag -- should make number? markuk-list? thingy
(def-markup-command (fill-line paper props markups)
  (markup-list?)
  "Put @var{markups} in a horizontal line of width @var{line-width}.
   The markups are spaced/flushed to fill the entire line."

  (let* ((stencils (map (lambda (x) (interpret-markup paper props x))
			markups))
	 (text-width (apply + (map interval-length
				   (map (lambda (x)
					  (ly:stencil-extent x X))
					stencils))))
	(word-count (length markups))
	(word-space (chain-assoc-get 'word-space props))
	(line-width (chain-assoc-get 'linewidth props))
	(fill-space (if (< line-width text-width)
			word-space
			(/ (- line-width text-width)
			   (if (= word-count 1) 2 (- word-count 1)))))
	(line-stencils (if (= word-count 1)
			   (map (lambda (x) (interpret-markup paper props x))
				(list (make-word-markup "")
				      (car markups)
				      (make-word-markup "")))
				stencils)))
    (stack-stencil-line fill-space line-stencils)))
  
(define (font-markup qualifier value)
  (lambda (paper props arg)
    (interpret-markup paper
		      (prepend-alist-chain qualifier value props)
                      arg)))

(def-markup-command (line paper props args) (markup-list?)
  "Put @var{args} in a horizontal line.  The property @code{word-space}
determines the space between each markup in @var{args}."
  (stack-stencil-line
   (chain-assoc-get 'word-space props)
   (map (lambda (m) (interpret-markup paper props m)) args)))

(def-markup-command (general-align paper props axis dir arg)  (integer? number? markup?)
  "Align @var{arg} in @var{axis} direction to the @var{dir} side."
  (let* ((m (interpret-markup paper props arg)))

    (ly:stencil-align-to! m axis dir)
    m
  ))

(def-markup-command (combine paper props m1 m2) (markup? markup?)
  "Print two markups on top of each other."
  (ly:stencil-add
   (interpret-markup paper props m1)
   (interpret-markup paper props m2)))

(def-markup-command (finger paper props arg) (markup?)
  "Set the argument as small numbers."
  (interpret-markup paper
                    (cons '((font-size . -5) (font-family . number)) props)
                    arg))

(def-markup-command (fontsize paper props mag arg) (number? markup?)
  "This sets the relative font size, e.g.
@example
A \\fontsize #2 @{ B C @} D
@end example


This will enlarge the B and the C by two steps.
"
  (interpret-markup
   paper 
   (prepend-alist-chain 'font-size mag props)
   arg))

(def-markup-command (magnify paper props sz arg) (number? markup?)
  "This sets the font magnification for the its argument. In the following
example, the middle A will be 10% larger:
@example
A \\magnify #1.1 @{ A @} A
@end example

Note: magnification only works if a font-name is explicitly selected.
Use @code{\\fontsize} otherwise."

  (interpret-markup
   paper 
   (prepend-alist-chain 'font-magnification sz props)
   arg))

(def-markup-command (bold paper props arg) (markup?)
  "Switch to bold font-series"
  (interpret-markup paper (prepend-alist-chain 'font-series 'bold props) arg))

(def-markup-command (sans paper props arg) (markup?)
  "Switch to the sans-serif family"
  (interpret-markup paper (prepend-alist-chain 'font-family 'sans props) arg))

(def-markup-command (number paper props arg) (markup?)
  "Set font family to @code{number}, which yields the font used for
time signatures and fingerings.  This font only contains numbers and
some punctuation. It doesn't have any letters.  "
  (interpret-markup paper (prepend-alist-chain 'font-encoding 'number props) arg))

(def-markup-command (roman paper props arg) (markup?)
  "Set font family to @code{roman}."
  (interpret-markup paper (prepend-alist-chain 'font-family 'roman props) arg))

(def-markup-command (huge paper props arg) (markup?)
  "Set font size to +2."
  (interpret-markup paper (prepend-alist-chain 'font-size 2 props) arg))

(def-markup-command (large paper props arg) (markup?)
  "Set font size to +1."
  (interpret-markup paper (prepend-alist-chain 'font-size 1 props) arg))

(def-markup-command (normalsize paper props arg) (markup?)
  "Set font size to default."
  (interpret-markup paper (prepend-alist-chain 'font-size 0 props) arg))

(def-markup-command (small paper props arg) (markup?)
  "Set font size to -1."
  (interpret-markup paper (prepend-alist-chain 'font-size -1 props) arg))

(def-markup-command (tiny paper props arg) (markup?)
  "Set font size to -2."
  (interpret-markup paper (prepend-alist-chain 'font-size -2 props) arg))

(def-markup-command (teeny paper props arg) (markup?)
  "Set font size to -3."
  (interpret-markup paper (prepend-alist-chain 'font-size -3 props) arg))

(def-markup-command (caps paper props arg) (markup?)
  "Set font shape to @code{caps}."
  (interpret-markup paper (prepend-alist-chain 'font-shape 'caps props) arg))

(def-markup-command (latin-i paper props arg) (markup?)
  "TEST latin1 encoding."
  (interpret-markup paper (prepend-alist-chain 'font-shape 'latin1 props) arg))

(def-markup-command (dynamic paper props arg) (markup?)
  "Use the dynamic font.  This font only contains @b{s}, @b{f}, @b{m},
@b{z}, @b{p}, and @b{r}.  When producing phrases, like ``pi@`{u} @b{f}'', the
normal words (like ``pi@`{u}'') should be done in a different font.  The
recommend font for this is bold and italic"
  (interpret-markup
   paper (prepend-alist-chain 'font-encoding 'dynamic props) arg))

(def-markup-command (italic paper props arg) (markup?)
  "Use italic @code{font-shape} for @var{arg}. "
  (interpret-markup paper (prepend-alist-chain 'font-shape 'italic props) arg))

(def-markup-command (typewriter paper props arg) (markup?)
  "Use @code{font-family} typewriter for @var{arg}."
  (interpret-markup
   paper (prepend-alist-chain 'font-family 'typewriter props) arg))

(def-markup-command (upright paper props arg) (markup?)
  "Set font shape to @code{upright}."
  (interpret-markup
   paper (prepend-alist-chain 'font-shape 'upright props) arg))

(def-markup-command (doublesharp paper props) ()
  "Draw a double sharp symbol."

  (interpret-markup paper props (markup #:musicglyph "accidentals-4")))
(def-markup-command (sesquisharp paper props) ()
  "Draw a 3/2 sharp symbol."
  (interpret-markup paper props (markup #:musicglyph "accidentals-3")))

(def-markup-command (sharp paper props) ()
  "Draw a sharp symbol."
  (interpret-markup paper props (markup #:musicglyph "accidentals-2")))
(def-markup-command (semisharp paper props) ()
  "Draw a semi sharp symbol."
  (interpret-markup paper props (markup #:musicglyph "accidentals-1")))
(def-markup-command (natural paper props) ()
  "Draw a natural symbol."

  (interpret-markup paper props (markup #:musicglyph "accidentals-0")))
(def-markup-command (semiflat paper props) ()
  "Draw a semiflat."
  (interpret-markup paper props (markup #:musicglyph "accidentals--1")))
(def-markup-command (flat paper props) ()
  "Draw a flat symbol."
  
  (interpret-markup paper props (markup #:musicglyph "accidentals--2")))
(def-markup-command (sesquiflat paper props) ()
  "Draw a 3/2 flat symbol."
  
  (interpret-markup paper props (markup #:musicglyph "accidentals--3")))
(def-markup-command (doubleflat paper props) ()
  "Draw a double flat symbol."

  (interpret-markup paper props (markup #:musicglyph "accidentals--4")))


(def-markup-command (column paper props args) (markup-list?)
  "Stack the markups in @var{args} vertically."
  (stack-lines
   -1 0.0 (chain-assoc-get 'baseline-skip props)
   (map (lambda (m) (interpret-markup paper props m)) args)))

(def-markup-command (dir-column paper props args) (markup-list?)
  "Make a column of args, going up or down, depending on the setting
of the @code{#'direction} layout property."
  (let* ((dir (chain-assoc-get 'direction props)))
    (stack-lines
     (if (number? dir) dir -1)
     0.0
      (chain-assoc-get 'baseline-skip props)
     (map (lambda (x) (interpret-markup paper props x)) args))))

(def-markup-command (center-align paper props args) (markup-list?)
  "Put @code{args} in a centered column. "
  (let* ((mols (map (lambda (x) (interpret-markup paper props x)) args))
         (cmols (map (lambda (x) (ly:stencil-align-to! x X CENTER)) mols)))
    (stack-lines -1 0.0 (chain-assoc-get 'baseline-skip props) mols)))

(def-markup-command (vcenter paper props arg) (markup?)
  "Align @code{arg} to its center. "
  (let* ((mol (interpret-markup paper props arg)))
    (ly:stencil-align-to! mol Y CENTER)
    mol))

(def-markup-command (right-align paper props arg) (markup?)
  (let* ((m (interpret-markup paper props arg)))
    (ly:stencil-align-to! m X RIGHT)
    m))

(def-markup-command (left-align paper props arg) (markup?)
  "Align @var{arg} on its left edge. "
  
  (let* ((m (interpret-markup paper props arg)))
    (ly:stencil-align-to! m X LEFT)
    m))

(def-markup-command (halign paper props dir arg) (number? markup?)
  "Set horizontal alignment. If @var{dir} is -1, then it is
left-aligned, while+1 is right. Values in between interpolate alignment
accordingly."

  
  (let* ((m (interpret-markup paper props arg)))
    (ly:stencil-align-to! m X dir)
    m))

(def-markup-command (musicglyph paper props glyph-name) (string?)
  "This is converted to a musical symbol, e.g. @code{\\musicglyph
#\"accidentals-0\"} will select the natural sign from the music font.
See @usermanref{The Feta font} for  a complete listing of the possible glyphs.
"
  (ly:find-glyph-by-name
   (ly:paper-get-font paper (cons '((font-encoding . music))
                                  props))
   glyph-name))


(def-markup-command (lookup paper props glyph-name) (string?)
  "Lookup a glyph by name."
  (ly:find-glyph-by-name (ly:paper-get-font paper props)
                         glyph-name))

(def-markup-command (char paper props num) (integer?)
  "This produces a single character, e.g. @code{\\char #65} produces the 
letter 'A'."
  (ly:get-glyph (ly:paper-get-font paper props) num))

(def-markup-command (raise paper props amount arg) (number? markup?)
  "
This  raises  @var{arg}, by the distance @var{amount}.
A negative @var{amount} indicates lowering:
@c
@lilypond[verbatim,fragment,relative=1]
 c1^\\markup { C \\small \\raise #1.0 \\bold { \"9/7+\" }}
@end lilypond
The argument to @code{\\raise} is the vertical displacement amount,
measured in (global) staff spaces.  @code{\\raise} and @code{\\super}
raise objects in relation to their surrounding markups.

If the text object itself is positioned above or below the staff, then
@code{\\raise} cannot be used to move it, since the mechanism that
positions it next to the staff cancels any shift made with
@code{\\raise}. For vertical positioning, use the @code{padding}
and/or @code{extra-offset} properties. "

  
  (ly:stencil-translate-axis (interpret-markup paper props arg)
                              amount Y))

(def-markup-command (fraction paper props arg1 arg2) (markup? markup?)
  "Make a fraction of two markups."
  
  (let* ((m1 (interpret-markup paper props arg1))
         (m2 (interpret-markup paper props arg2)))
    (ly:stencil-align-to! m1 X CENTER)
    (ly:stencil-align-to! m2 X CENTER)    
    (let* ((x1 (ly:stencil-extent m1 X))
           (x2 (ly:stencil-extent m2 X))
           (line (ly:round-filled-box (interval-union x1 x2) '(-0.05 . 0.05) 0.0))
           ;; should stack mols separately, to maintain LINE on baseline
           (stack (stack-lines -1 0.2 0.6 (list m1 line m2))))
      (ly:stencil-align-to! stack Y CENTER)
      (ly:stencil-align-to! stack X LEFT)
      ;; should have EX dimension
      ;; empirical anyway
      (ly:stencil-translate-axis stack 0.75 Y))))


;; TODO: better syntax.

(def-markup-command (note-by-number paper props log dot-count dir) (number? number? number?)
  "Construct a note symbol, with stem.  By using fractional values for
@var{dir}, you can obtain longer or shorter stems."
  
  (let* ((font (ly:paper-get-font paper (cons '((font-encoding . music)) props)))
         (stemlen (max 3 (- log 1)))
         (headgl (ly:find-glyph-by-name
                  font
                  (string-append "noteheads-" (number->string (min log 2)))))
         (stemth 0.13)
         (stemy (* dir stemlen))
         (attachx (if (> dir 0)
                      (- (cdr (ly:stencil-extent headgl X)) stemth)
                      0))
         (attachy (* dir 0.28))
         (stemgl (and (> log 0)
                      (ly:round-filled-box
                       (cons attachx (+ attachx  stemth))
                       (cons (min stemy attachy)
                             (max stemy attachy))
                       (/ stemth 3))))
         (dot (ly:find-glyph-by-name font "dots-dot"))
         (dotwid (interval-length (ly:stencil-extent dot X)))
         (dots (and (> dot-count 0)
                    (apply ly:stencil-add
                           (map (lambda (x)
                                  (ly:stencil-translate-axis
                                   dot  (* (+ 1 (* 2 x)) dotwid) X) )
                                (iota dot-count 1)))))
         (flaggl (and (> log 2)
                      (ly:stencil-translate
                       (ly:find-glyph-by-name font
                                              (string-append "flags-"
                                                             (if (> dir 0) "u" "d")
                                                             (number->string log)))
                       (cons (+ attachx (/ stemth 2)) stemy)))))
    (if flaggl
        (set! stemgl (ly:stencil-add flaggl stemgl)))
    (if (ly:stencil? stemgl)
        (set! stemgl (ly:stencil-add stemgl headgl))
        (set! stemgl headgl))
    (if (ly:stencil? dots)
        (set! stemgl
              (ly:stencil-add
               (ly:stencil-translate-axis dots
                                           (+ (if (and (> dir 0) (> log 2))
                                                  (* 1.5 dotwid)
                                                  0)
                                              ;; huh ? why not necessary?
                                              ;;(cdr (ly:stencil-extent headgl X))
                                              dotwid)
                                           X)
               stemgl)))
    stemgl))

(use-modules (ice-9 regex))

(define-public log2 
  (let ((divisor (log 2)))
    (lambda (z) (inexact->exact (/ (log z) divisor)))))

(define (parse-simple-duration duration-string)
  "Parse the `duration-string', e.g. ''4..'' or ''breve.'', and return a (log dots) list."
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

(def-markup-command (note paper props duration dir) (string? number?)
  "This produces a note with a stem pointing in @var{dir} direction, with
the @var{duration} for the note head type and augmentation dots. For
example, @code{\\note #\"4.\" #-0.75} creates a dotted quarter note, with
a shortened down stem."
  
  (let ((parsed (parse-simple-duration duration)))
    (note-by-number-markup paper props (car parsed) (cadr parsed) dir)))

(def-markup-command (normal-size-super paper props arg) (markup?)
  "A superscript which does not use a smaller font."
  
  (ly:stencil-translate-axis (interpret-markup
                               paper
                               props arg)
                              (* 0.5  (chain-assoc-get 'baseline-skip props))
                              Y))

(def-markup-command (super paper props arg) (markup?)
  "
@cindex raising text
@cindex lowering text
@cindex moving text
@cindex translating text

@cindex @code{\\super}


Raising and lowering texts can be done with @code{\\super} and
@code{\\sub}:

@lilypond[verbatim,fragment,relative=1]
 c1^\\markup { E \"=\" mc \\super \"2\" }
@end lilypond

"
  
  (ly:stencil-translate-axis
   (interpret-markup
    paper
    (cons `((font-size . ,(- (chain-assoc-get 'font-size props 0) 3))) props)
    arg)
   (* 0.5 (chain-assoc-get 'baseline-skip props))
   Y))

(def-markup-command (translate paper props offset arg) (number-pair? markup?)
  "This translates an object. Its first argument is a cons of numbers
@example
A \\translate #(cons 2 -3) @{ B C @} D
@end example
This moves `B C' 2 spaces to the right, and 3 down, relative to its
surroundings. This command cannot be used to move isolated scripts
vertically, for the same reason that @code{\\raise} cannot be used for
that.

. "
  (ly:stencil-translate (interpret-markup  paper props arg)
                         offset))

(def-markup-command (sub paper props arg) (markup?)
  "Set @var{arg} in subscript."
  
  (ly:stencil-translate-axis
   (interpret-markup
    paper
    (cons `((font-size . ,(- (chain-assoc-get 'font-size props 0) 3))) props)
    arg)
   (* -0.5 (chain-assoc-get 'baseline-skip props))
   Y))

(def-markup-command (normal-size-sub paper props arg) (markup?)
  "Set @var{arg} in subscript, in a normal font size."

  (ly:stencil-translate-axis
   (interpret-markup paper props arg)
   (* -0.5 (chain-assoc-get 'baseline-skip props))
   Y))

(def-markup-command (hbracket paper props arg) (markup?)
  "Draw horizontal brackets around @var{arg}."  
  (let ((th 0.1) ;; todo: take from GROB.
        (m (interpret-markup paper props arg)))
    (bracketify-stencil m X th (* 2.5 th) th)))

(def-markup-command (bracket paper props arg) (markup?)
  "Draw vertical brackets around @var{arg}."  
  (let ((th 0.1) ;; todo: take from GROB.
        (m (interpret-markup paper props arg)))
    (bracketify-stencil m Y th (* 2.5 th) th)))

;; todo: fix negative space
(def-markup-command (hspace paper props amount) (number?)
  "This produces a invisible object taking horizontal space.
@example 
\\markup @{ A \\hspace #2.0 B @} 
@end example
will put extra space between A and B, on top of the space that is
normally inserted before elements on a line.
"
  (if (> amount 0)
      (ly:make-stencil "" (cons 0 amount) '(-1 . 1) )
      (ly:make-stencil "" (cons amount amount) '(-1 . 1))))

(def-markup-command (override paper props new-prop arg) (pair? markup?)
  "Add the first argument in to the property list.  Properties may be
any sort of property supported by @internalsref{font-interface} and
@internalsref{text-interface}, for example

@verbatim
\\override #'(font-family . married) \"bla\"
@end verbatim

"
  (interpret-markup paper (cons (list new-prop) props) arg))

(def-markup-command (smaller paper props arg) (markup?)
  "Decrease the font size relative to current setting"
  (let* ((fs (chain-assoc-get 'font-size props 0))
	 (entry (cons 'font-size (- fs 1))))
    (interpret-markup paper (cons (list entry) props) arg)))


(def-markup-command (bigger paper props arg) (markup?)
  "Increase the font size relative to current setting"
  (let* ((fs (chain-assoc-get 'font-size props 0))
         (entry (cons 'font-size (+ fs 1))))
    (interpret-markup paper (cons (list entry) props) arg)))

(def-markup-command larger (markup?)
  bigger-markup)

(def-markup-command (box paper props arg) (markup?)
  "Draw a box round @var{arg}"
  
  (let ((th 0.1)
        (pad 0.2)
        (m (interpret-markup paper props arg)))
    (box-stencil m th pad)))

(def-markup-command (strut paper props) ()
  
  "Create a box of the same height as the space in the current font.

FIXME: is this working? 
"
  
  (let ((m (Text_item::interpret_markup paper props " ")))
    (ly:stencil-set-extent! m X '(1000 . -1000))
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


(def-markup-command (markletter paper props num) (integer?)
   "Make a markup letter for @var{num}.  The letters start with A to Z
 (skipping I), and continues with double letters."
 
   (Text_item::interpret_markup paper props (number->markletter-string num)))




(def-markup-command (bracketed-y-column paper props indices args)
  (list? markup-list?)
  "Make a column of the markups in @var{args}, putting brackets around
the elements marked in @var{indices}, which is a list of numbers."

    (define (sublist l start stop)
    (take (drop l start)  (- (1+ stop) start)) )

  (define (stencil-list-extent ss axis)
    (cons
     (apply min (map (lambda (x) (car (ly:stencil-extent x axis))) ss))
     (apply max (map (lambda (x) (cdr (ly:stencil-extent x axis))) ss))))
	    
  (define (stack-stencils stencils bskip last-stencil)
    (cond
     ((null? stencils) '())
     ((not last-stencil)
      (cons (car stencils)
	    (stack-stencils (cdr stencils) bskip (car stencils))))
     (else
      (let*
	  ((orig (car stencils))
	   (dir (chain-assoc-get 'direction  props DOWN))
	   (new (ly:stencil-moved-to-edge last-stencil Y dir
					  orig
					  0.1 bskip))
	   )

	(cons new (stack-stencils (cdr stencils) bskip new))))
    ))

  (define (make-brackets stencils indices acc)
    (if (and stencils
	     (pair? indices)
	     (pair? (cdr indices)))
	(let*
	    ((encl (sublist stencils (car indices) (cadr indices)))
	     (x-ext (stencil-list-extent encl X))
	     (y-ext (stencil-list-extent encl Y))
	     (thick 0.10)
	     (pad 0.35)
	     (protusion (* 2.5 thick))
	     (lb
	      (ly:stencil-translate-axis 
	       (ly:bracket Y y-ext thick protusion)
	       (- (car x-ext) pad) X))
	     (rb (ly:stencil-translate-axis
		  (ly:bracket Y y-ext thick (- protusion))
		  (+ (cdr x-ext) pad) X))
	     )

	  (make-brackets
	   stencils (cddr indices)
	   (append
	    (list lb rb)
	     acc)))
	acc))

  (let*
      ((stencils
	(map (lambda (x)
	       (interpret-markup
		paper
		props
		x)) args))
       (leading
	 (chain-assoc-get 'baseline-skip props))
       (stacked (stack-stencils stencils 1.25 #f))
       (brackets (make-brackets stacked indices '()))
       )

    (apply ly:stencil-add
	   (append stacked brackets)
	   )))


	     

  
  

     
