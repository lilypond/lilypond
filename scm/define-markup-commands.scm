;;;; define-markup-commands.scm -- markup commands
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 2000--2009  Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;;                  Jan Nieuwenhuizen <janneke@gnu.org>


;;; markup commands
;;;  * each markup function should have a doc string with
;;     syntax, description and example. 

(use-modules (ice-9 regex))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public empty-stencil (ly:make-stencil '() '(1 . -1) '(1 . -1)))
(define-public point-stencil (ly:make-stencil "" '(0 . 0) '(0 . 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; geometric shapes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-builtin-markup-command (draw-line layout props dest)
  (number-pair?)
  graphic
  ((thickness 1))
  "
@cindex drawing lines within text

A simple line.
@lilypond[verbatim,quote]
\\markup {
  \\draw-line #'(4 . 4)
  \\override #'(thickness . 5)
  \\draw-line #'(-3 . 0)
}
@end lilypond"
  (let ((th (* (ly:output-def-lookup layout 'line-thickness)
               thickness))
        (x (car dest))
        (y (cdr dest)))
    (make-line-stencil th 0 0 x y)))

(define-builtin-markup-command (draw-circle layout props radius thickness filled)
  (number? number? boolean?)
  graphic
  ()
  "
@cindex drawing circles within text

A circle of radius @var{radius} and thickness @var{thickness},
optionally filled.

@lilypond[verbatim,quote]
\\markup {
  \\draw-circle #2 #0.5 ##f
  \\hspace #2
  \\draw-circle #2 #0 ##t
}
@end lilypond"
  (make-circle-stencil radius thickness filled))

(define-builtin-markup-command (triangle layout props filled)
  (boolean?)
  graphic
  ((thickness 0.1)
   (font-size 0)
   (baseline-skip 2))
  "
@cindex drawing triangles within text

A triangle, either filled or empty.

@lilypond[verbatim,quote]
\\markup {
  \\triangle ##t
  \\hspace #2
  \\triangle ##f
}
@end lilypond"
  (let ((ex (* (magstep font-size) 0.8 baseline-skip)))
    (ly:make-stencil
     `(polygon '(0.0 0.0
                     ,ex 0.0
                     ,(* 0.5 ex)
                     ,(* 0.86 ex))
           ,thickness
           ,filled)
     (cons 0 ex)
     (cons 0 (* .86 ex)))))

(define-builtin-markup-command (circle layout props arg)
  (markup?)
  graphic
  ((thickness 1)
   (font-size 0)
   (circle-padding 0.2))
  "
@cindex circling text

Draw a circle around @var{arg}.  Use @code{thickness},
@code{circle-padding} and @code{font-size} properties to determine line
thickness and padding around the markup.

@lilypond[verbatim,quote]
\\markup {
  \\circle {
    Hi
  }
}
@end lilypond"
  (let ((th (* (ly:output-def-lookup layout 'line-thickness)
               thickness))
         (pad (* (magstep font-size) circle-padding))
         (m (interpret-markup layout props arg)))
    (circle-stencil m th pad)))

(define-builtin-markup-command (with-url layout props url arg)
  (string? markup?)
  graphic
  ()
  "
@cindex inserting URL links into text

Add a link to URL @var{url} around @var{arg}.  This only works in
the PDF backend.

@lilypond[verbatim,quote]
\\markup {
  \\with-url #\"http://lilypond.org/web/\" {
    LilyPond ... \\italic {
      music notation for everyone
    }
  }
}
@end lilypond"
  (let* ((stil (interpret-markup layout props arg))
	 (xextent (ly:stencil-extent stil X))
	 (yextent (ly:stencil-extent stil Y))
	 (old-expr (ly:stencil-expr stil))
	 (url-expr (list 'url-link url `(quote ,xextent) `(quote ,yextent))))

    (ly:stencil-add (ly:make-stencil url-expr xextent yextent) stil)))

(define-builtin-markup-command (beam layout props width slope thickness)
  (number? number? number?)
  graphic
  ()
  "
@cindex drawing beams within text

Create a beam with the specified parameters.
@lilypond[verbatim,quote]
\\markup {
  \\beam #5 #1 #2
}
@end lilypond"
  (let* ((y (* slope width))
	 (yext (cons (min 0 y) (max 0 y)))
	 (half (/ thickness 2)))

    (ly:make-stencil
     `(polygon ',(list 
		  0 (/ thickness -2)
		    width (+ (* width slope)  (/ thickness -2))
		    width (+ (* width slope)  (/ thickness 2))
		    0 (/ thickness 2))
	       ,(ly:output-def-lookup layout 'blot-diameter)
	       #t)
     (cons 0 width)
     (cons (+ (- half) (car yext))
	   (+ half (cdr yext))))))

(define-builtin-markup-command (underline layout props arg)
  (markup?)
  font
  ((thickness 1))
  "
@cindex underlining text

Underline @var{arg}.  Looks at @code{thickness} to determine line
thickness and y-offset.

@lilypond[verbatim,quote]
\\markup {
  default
  \\hspace #2
  \\override #'(thickness . 2)
  \\underline {
    underline
  }
}
@end lilypond"
  (let* ((thick (* (ly:output-def-lookup layout 'line-thickness)
                   thickness))
         (markup (interpret-markup layout props arg))
         (x1 (car (ly:stencil-extent markup X)))
         (x2 (cdr (ly:stencil-extent markup X)))
         (y (* thick -2))
         (line (make-line-stencil thick x1 y x2 y)))
    (ly:stencil-add markup line)))

(define-builtin-markup-command (box layout props arg)
  (markup?)
  font
  ((thickness 1)
   (font-size 0)
   (box-padding 0.2))
  "
@cindex enclosing text within a box

Draw a box round @var{arg}.  Looks at @code{thickness},
@code{box-padding} and @code{font-size} properties to determine line
thickness and padding around the markup.

@lilypond[verbatim,quote]
\\markup {
  \\override #'(box-padding . 0.5)
  \\box
  \\line { V. S. }
}
@end lilypond"
  (let* ((th (* (ly:output-def-lookup layout 'line-thickness)
                thickness))
         (pad (* (magstep font-size) box-padding))
         (m (interpret-markup layout props arg)))
    (box-stencil m th pad)))

(define-builtin-markup-command (filled-box layout props xext yext blot)
  (number-pair? number-pair? number?)
  graphic
  ()
  "
@cindex drawing solid boxes within text
@cindex drawing boxes with rounded corners

Draw a box with rounded corners of dimensions @var{xext} and
@var{yext}.  For example,
@verbatim
\\filled-box #'(-.3 . 1.8) #'(-.3 . 1.8) #0
@end verbatim
creates a box extending horizontally from -0.3 to 1.8 and
vertically from -0.3 up to 1.8, with corners formed from a
circle of diameter@tie{}0 (i.e., sharp corners).

@lilypond[verbatim,quote]
\\markup {
  \\filled-box #'(0 . 4) #'(0 . 4) #0
  \\filled-box #'(0 . 2) #'(-4 . 2) #0.4
  \\filled-box #'(1 . 8) #'(0 . 7) #0.2
  \\with-color #white
  \\filled-box #'(-4.5 . -2.5) #'(3.5 . 5.5) #0.7
}
@end lilypond"
  (ly:round-filled-box
   xext yext blot))

(define-builtin-markup-command (rounded-box layout props arg)
  (markup?)
  graphic
  ((thickness 1)
   (corner-radius 1)
   (font-size 0)
   (box-padding 0.5))
  "@cindex enclosing text in a box with rounded corners
   @cindex drawing boxes with rounded corners around text
Draw a box with rounded corners around @var{arg}.  Looks at @code{thickness},
@code{box-padding} and @code{font-size} properties to determine line
thickness and padding around the markup; the @code{corner-radius} property
makes it possible to define another shape for the corners (default is 1).

@lilypond[quote,verbatim,relative=2]
c4^\\markup {
  \\rounded-box {
    Overtura
  }
}
c,8. c16 c4 r
@end lilypond" 
  (let ((th (* (ly:output-def-lookup layout 'line-thickness)
               thickness))
        (pad (* (magstep font-size) box-padding))
        (m (interpret-markup layout props arg)))
    (ly:stencil-add (rounded-box-stencil m th pad corner-radius)
                    m)))

(define-builtin-markup-command (rotate layout props ang arg)
  (number? markup?)
  align
  ()
  "
@cindex rotating text

Rotate object with @var{ang} degrees around its center.

@lilypond[verbatim,quote]
\\markup {
  default
  \\hspace #2
  \\rotate #45
  \\line {
    rotated 45°
  }
}
@end lilypond"
  (let* ((stil (interpret-markup layout props arg)))
    (ly:stencil-rotate stil ang 0 0)))

(define-builtin-markup-command (whiteout layout props arg)
  (markup?)
  other
  ()
  "
@cindex adding a white background to text

Provide a white background for @var{arg}.

@lilypond[verbatim,quote]
\\markup {
  \\combine
    \\filled-box #'(-1 . 10) #'(-3 . 4) #1
    \\whiteout whiteout
}
@end lilypond"
  (stencil-whiteout (interpret-markup layout props arg)))

(define-builtin-markup-command (pad-markup layout props amount arg)
  (number? markup?)
  align
  ()
  "
@cindex padding text
@cindex putting space around text

Add space around a markup object.

@lilypond[verbatim,quote]
\\markup {
  \\box {
    default
  }
  \\hspace #2
  \\box {
    \\pad-markup #1 {
      padded
    }
  }
}
@end lilypond"
  (let*
      ((stil (interpret-markup layout props arg))
       (xext (ly:stencil-extent stil X))
       (yext (ly:stencil-extent stil Y)))

    (ly:make-stencil
     (ly:stencil-expr stil)
     (interval-widen xext amount)
     (interval-widen yext amount))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; space
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-builtin-markup-command (strut layout props)
  ()
  other
  ()
  "
@cindex creating vertical spaces in text

Create a box of the same height as the space in the current font."
  (let ((m (ly:text-interface::interpret-markup layout props " ")))
    (ly:make-stencil (ly:stencil-expr m)
		     '(0 . 0)
		     (ly:stencil-extent m X)
		     )))

;; todo: fix negative space
(define-builtin-markup-command (hspace layout props amount)
  (number?)
  align
  ()
  "
@cindex creating horizontal spaces in text

Create an invisible object taking up horizontal space @var{amount}.

@lilypond[verbatim,quote]
\\markup {
  one
  \\hspace #2
  two
  \\hspace #8
  three
}
@end lilypond"
  (if (> amount 0)
      (ly:make-stencil "" (cons 0 amount) '(-1 . 1))
      (ly:make-stencil "" (cons amount amount) '(-1 . 1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; importing graphics.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-builtin-markup-command (stencil layout props stil)
  (ly:stencil?)
  other
  ()
  "
@cindex importing stencils into text

Use a stencil as markup.

@lilypond[verbatim,quote]
\\markup {
  \\stencil #(make-circle-stencil 2 0 #t)
}
@end lilypond"
  stil)

(define bbox-regexp
  (make-regexp "%%BoundingBox:[ \t]+([0-9-]+)[ \t]+([0-9-]+)[ \t]+([0-9-]+)[ \t]+([0-9-]+)"))

(define (get-postscript-bbox string)
  "Extract the bbox from STRING, or return #f if not present."
  (let*
      ((match (regexp-exec bbox-regexp string)))
    
    (if match
	(map (lambda (x)
	       (string->number (match:substring match x)))
	     (cdr (iota 5)))
	     
	#f)))

(define-builtin-markup-command (epsfile layout props axis size file-name)
  (number? number? string?)
  graphic
  ()
  "
@cindex inlining an Encapsulated PostScript image

Inline an EPS image.  The image is scaled along @var{axis} to
@var{size}.

@lilypond[verbatim,quote]
\\markup {
  \\general-align #Y #DOWN {
    \\epsfile #X #20 #\"context-example.eps\"
    \\epsfile #Y #20 #\"context-example.eps\"
  }
}
@end lilypond"
  (if (ly:get-option 'safe)
      (interpret-markup layout props "not allowed in safe")
      (eps-file->stencil axis size file-name)
      ))

(define-builtin-markup-command (postscript layout props str)
  (string?)
  graphic
  ()
  "
@cindex inserting PostScript directly into text
This inserts @var{str} directly into the output as a PostScript
command string.

@lilypond[verbatim,quote]
ringsps = #\"
  0.15 setlinewidth
  0.9 0.6 moveto
  0.4 0.6 0.5 0 361 arc
  stroke
  1.0 0.6 0.5 0 361 arc
  stroke
  \"

rings = \\markup {
  \\with-dimensions #'(-0.2 . 1.6) #'(0 . 1.2)
  \\postscript #ringsps
}

\\relative c'' {
  c2^\\rings
  a2_\\rings
}
@end lilypond"
  ;; FIXME
  (ly:make-stencil
   (list 'embedded-ps
	 (format "
gsave currentpoint translate
0.1 setlinewidth
 ~a
grestore
"
		 str))
   '(0 . 0) '(0 . 0)))

(define-builtin-markup-command (score layout props score)
  (ly:score?)
  music
  ()
  "
@cindex inserting music into text

Inline an image of music.

@lilypond[verbatim,quote]
\\markup {
  \\score {
    \\new PianoStaff <<
      \\new Staff \\relative c' {
        \\key f \\major
        \\time 3/4
        \\mark \\markup { Allegro }
        f2\\p( a4)
        c2( a4)
        bes2( g'4)
        f8( e) e4 r
      }
      \\new Staff \\relative c {
        \\clef bass
        \\key f \\major
        \\time 3/4
        f8( a c a c a
        f c' es c es c)
        f,( bes d bes d bes)
        f( g bes g bes g)
      }
    >>
    \\layout {
      indent = 0.0\\cm
      \\context {
        \\Score
        \\override RehearsalMark #'break-align-symbols =
          #'(time-signature key-signature)
        \\override RehearsalMark #'self-alignment-X = #LEFT
      }
      \\context {
        \\Staff
        \\override TimeSignature #'break-align-anchor-alignment = #LEFT
      }
    }
  }
}
@end lilypond"
  (let* ((output (ly:score-embedded-format score layout)))

    (if (ly:music-output? output)
	(paper-system-stencil
	 (vector-ref (ly:paper-score-paper-systems output) 0))
	(begin
	  (ly:warning (_"no systems found in \\score markup, does it have a \\layout block?"))
	  empty-stencil))))

(define-builtin-markup-command (null layout props)
  ()
  other
  ()
  "
@cindex creating empty text objects

An empty markup with extents of a single point.

@lilypond[verbatim,quote]
\\markup {
  \\null
}
@end lilypond"
  point-stencil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic formatting.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-builtin-markup-command (simple layout props str)
  (string?)
  font
  ()
  "
@cindex simple text strings

A simple text string; @code{\\markup @{ foo @}} is equivalent with
@code{\\markup @{ \\simple #\"foo\" @}}.

Note: for creating standard text markup or defining new markup commands,
the use of @code{\\simple} is unnecessary.

@lilypond[verbatim,quote]
\\markup {
  \\simple #\"simple\"
  \\simple #\"text\"
  \\simple #\"strings\"
}
@end lilypond"
  (interpret-markup layout props str))

(define-builtin-markup-command (tied-lyric layout props str)
  (string?)
  music
  ()
  "
@cindex simple text strings with tie characters

Like simple-markup, but use tie characters for @q{~} tilde symbols.

@lilypond[verbatim,quote]
\\markup {
  \\tied-lyric #\"Lasciate~i monti\"
}
@end lilypond"
  (if (string-contains str "~")
      (let*
	  ((parts (string-split str #\~))
	   (tie-str (ly:wide-char->utf-8 #x203f))
	   (joined  (list-join parts tie-str))
	   (join-stencil (interpret-markup layout props tie-str))
	   )

	(interpret-markup layout 
			  (prepend-alist-chain
			   'word-space
			   (/ (interval-length (ly:stencil-extent join-stencil X)) -3.5)
			   props)
			  (make-line-markup joined)))
			   ;(map (lambda (s) (interpret-markup layout props s)) parts))
      (interpret-markup layout props str)))

(define-public empty-markup
  (make-simple-markup ""))

;; helper for justifying lines.
(define (get-fill-space word-count line-width text-widths)
  "Calculate the necessary paddings between each two adjacent texts.
	The lengths of all texts are stored in @var{text-widths}.
	The normal formula for the padding between texts a and b is:
	padding = line-width/(word-count - 1) - (length(a) + length(b))/2
	The first and last padding have to be calculated specially using the
	whole length of the first or last text.
	Return a list of paddings."
  (cond
   ((null? text-widths) '())
   
   ;; special case first padding
   ((= (length text-widths) word-count)
    (cons 
     (- (- (/ line-width (1- word-count)) (car text-widths))
	(/ (car (cdr text-widths)) 2))
     (get-fill-space word-count line-width (cdr text-widths))))
   ;; special case last padding
   ((= (length text-widths) 2)
    (list (- (/ line-width (1- word-count))
	     (+ (/ (car text-widths) 2) (car (cdr text-widths)))) 0))
   (else
    (cons 
     (- (/ line-width (1- word-count))
	(/ (+ (car text-widths) (car (cdr text-widths))) 2))
     (get-fill-space word-count line-width (cdr text-widths))))))

(define-builtin-markup-command (fill-line layout props args)
  (markup-list?)
  align
  ((text-direction RIGHT)
   (word-space 1)
   (line-width #f))
  "Put @var{markups} in a horizontal line of width @var{line-width}.
The markups are spaced or flushed to fill the entire line.
If there are no arguments, return an empty stencil.

@lilypond[verbatim,quote]
\\markup {
  \\column {
    \\fill-line {
      Words evenly spaced across the page
    }
    \\null
    \\fill-line {
      \\line { Text markups }
      \\line {
        \\italic { evenly spaced }
      }
      \\line { across the page }
    }
  }
}
@end lilypond"
  (let* ((orig-stencils (interpret-markup-list layout props args))
	 (stencils
	  (map (lambda (stc)
		 (if (ly:stencil-empty? stc)
		     point-stencil
		     stc)) orig-stencils))
	 (text-widths
	  (map (lambda (stc)
		 (if (ly:stencil-empty? stc)
		     0.0
		     (interval-length (ly:stencil-extent stc X))))
	       stencils))
	 (text-width (apply + text-widths))
	 (word-count (length stencils))
	 (prop-line-width (chain-assoc-get 'line-width props #f))
	 (line-width (or line-width (ly:output-def-lookup layout 'line-width)))
	 (fill-space
	 	(cond
			((= word-count 1) 
				(list
					(/ (- line-width text-width) 2)
					(/ (- line-width text-width) 2)))
			((= word-count 2)
				(list
					(- line-width text-width)))
			(else 
				(get-fill-space word-count line-width text-widths))))
	 (fill-space-normal
	  (map (lambda (x)
		 (if (< x word-space)
		     word-space
		     x))
	       fill-space))
					
	 (line-stencils (if (= word-count 1)
			    (list
			     point-stencil
			     (car stencils)
			     point-stencil)
			    stencils)))

    (if (= text-direction LEFT)
	(set! line-stencils (reverse line-stencils)))

    (if (null? (remove ly:stencil-empty? orig-stencils))
	empty-stencil
	(stack-stencils-padding-list X
				     RIGHT fill-space-normal line-stencils))))
	
(define-builtin-markup-command (line layout props args)
  (markup-list?)
  align
  ((word-space)
   (text-direction RIGHT))
  "Put @var{args} in a horizontal line.  The property @code{word-space}
determines the space between markups in @var{args}.

@lilypond[verbatim,quote]
\\markup {
  \\line {
    one two three
  }
}
@end lilypond"
  (let ((stencils (interpret-markup-list layout props args)))
    (if (= text-direction LEFT)
        (set! stencils (reverse stencils)))
    (stack-stencil-line
     word-space
     (remove ly:stencil-empty? stencils))))

(define-builtin-markup-command (concat layout props args)
  (markup-list?)
  align
  ()
  "
@cindex concatenating text
@cindex ligatures in text

Concatenate @var{args} in a horizontal line, without spaces in between.
Strings and simple markups are concatenated on the input level, allowing
ligatures.  For example, @code{\\concat @{ \"f\" \\simple #\"i\" @}} is
equivalent to @code{\"fi\"}.

@lilypond[verbatim,quote]
\\markup {
  \\concat {
    one
    two
    three
  }
}
@end lilypond"
  (define (concat-string-args arg-list)
    (fold-right (lambda (arg result-list)
                  (let ((result (if (pair? result-list)
                                    (car result-list)
                                  '())))
                    (if (and (pair? arg) (eqv? (car arg) simple-markup))
                      (set! arg (cadr arg)))
                    (if (and (string? result) (string? arg))
                        (cons (string-append arg result) (cdr result-list))
                      (cons arg result-list))))
                '()
                arg-list))

  (interpret-markup layout
                    (prepend-alist-chain 'word-space 0 props)
                    (make-line-markup (if (markup-command-list? args)
					  args
					  (concat-string-args args)))))

(define (wordwrap-stencils stencils
			   justify base-space line-width text-dir)
  "Perform simple wordwrap, return stencil of each line."  
  (define space (if justify
                    ;; justify only stretches lines.
		    (* 0.7 base-space)
		    base-space))
  (define (take-list width space stencils
		     accumulator accumulated-width)
    "Return (head-list . tail) pair, with head-list fitting into width"
    (if (null? stencils)
	(cons accumulator stencils)
	(let* ((first (car stencils))
               (first-wid (cdr (ly:stencil-extent (car stencils) X)))
               (newwid (+ space first-wid accumulated-width)))
	  (if (or (null? accumulator)
                  (< newwid width))
              (take-list width space
                         (cdr stencils)
                         (cons first accumulator)
                         newwid)
              (cons accumulator stencils)))))
  (let loop ((lines '())
             (todo stencils))
    (let* ((line-break (take-list line-width space todo
                                  '() 0.0))
	   (line-stencils (car line-break))
	   (space-left (- line-width
                          (apply + (map (lambda (x) (cdr (ly:stencil-extent x X)))
                                        line-stencils))))
	   (line-word-space (cond ((not justify) space)
                                  ;; don't stretch last line of paragraph.
                                  ;; hmmm . bug - will overstretch the last line in some case. 
                                  ((null? (cdr line-break))
                                   base-space)
                                  ((null? line-stencils) 0.0)
                                  ((null? (cdr line-stencils)) 0.0)
                                  (else (/ space-left (1- (length line-stencils))))))
	   (line (stack-stencil-line line-word-space
                                     (if (= text-dir RIGHT)
                                         (reverse line-stencils)
                                         line-stencils))))
      (if (pair? (cdr line-break))
          (loop (cons line lines)
                (cdr line-break))
          (begin
            (if (= text-dir LEFT)
                (set! line
                      (ly:stencil-translate-axis
                       line
                       (- line-width (interval-end (ly:stencil-extent line X)))
                       X)))
            (reverse (cons line lines)))))))

(define-builtin-markup-list-command (wordwrap-internal layout props justify args)
  (boolean? markup-list?)
  ((line-width #f)
   (word-space)
   (text-direction RIGHT))
  "Internal markup list command used to define @code{\\justify} and @code{\\wordwrap}."
  (wordwrap-stencils (remove ly:stencil-empty?
                             (interpret-markup-list layout props args))
                     justify
                     word-space
                     (or line-width
                         (ly:output-def-lookup layout 'line-width))
                     text-direction))

(define-builtin-markup-command (justify layout props args)
  (markup-list?)
  align
  ((baseline-skip)
   wordwrap-internal-markup-list)
  "
@cindex justifying text

Like @code{\\wordwrap}, but with lines stretched to justify the margins.
Use @code{\\override #'(line-width . @var{X})} to set the line width;
@var{X}@tie{}is the number of staff spaces.

@lilypond[verbatim,quote]
\\markup {
  \\justify {
    Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed
    do eiusmod tempor incididunt ut labore et dolore magna aliqua.
    Ut enim ad minim veniam, quis nostrud exercitation ullamco
    laboris nisi ut aliquip ex ea commodo consequat.
  }
}
@end lilypond"
  (stack-lines DOWN 0.0 baseline-skip
               (wordwrap-internal-markup-list layout props #t args)))

(define-builtin-markup-command (wordwrap layout props args)
  (markup-list?)
  align
  ((baseline-skip)
   wordwrap-internal-markup-list)
  "Simple wordwrap.  Use @code{\\override #'(line-width . @var{X})} to set
the line width, where @var{X} is the number of staff spaces.

@lilypond[verbatim,quote]
\\markup {
  \\wordwrap {
    Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed
    do eiusmod tempor incididunt ut labore et dolore magna aliqua.
    Ut enim ad minim veniam, quis nostrud exercitation ullamco
    laboris nisi ut aliquip ex ea commodo consequat.
  }
}
@end lilypond"
  (stack-lines DOWN 0.0 baseline-skip
	       (wordwrap-internal-markup-list layout props #f args)))

(define-builtin-markup-list-command (wordwrap-string-internal layout props justify arg)
  (boolean? string?)
  ((line-width)
   (word-space)
   (text-direction RIGHT))
  "Internal markup list command used to define @code{\\justify-string} and
@code{\\wordwrap-string}."
  (let* ((para-strings (regexp-split
                        (string-regexp-substitute
                         "\r" "\n"
                         (string-regexp-substitute "\r\n" "\n" arg))
                        "\n[ \t\n]*\n[ \t\n]*"))
         (list-para-words (map (lambda (str)
                                 (regexp-split str "[ \t\n]+"))
                               para-strings))
         (para-lines (map (lambda (words)
                            (let* ((stencils
                                    (remove ly:stencil-empty?
                                            (map (lambda (x)
                                                   (interpret-markup layout props x))
                                                 words))))
                              (wordwrap-stencils stencils
                                                 justify word-space
                                                 line-width text-direction)))
                          list-para-words)))
    (apply append para-lines)))

(define-builtin-markup-command (wordwrap-string layout props arg)
  (string?)
  align
  ((baseline-skip)
   wordwrap-string-internal-markup-list)
  "Wordwrap a string.  Paragraphs may be separated with double newlines.
  
@lilypond[verbatim,quote]
\\markup {
  \\override #'(line-width . 40)
  \\wordwrap-string #\"Lorem ipsum dolor sit amet, consectetur
      adipisicing elit, sed do eiusmod tempor incididunt ut labore
      et dolore magna aliqua.


      Ut enim ad minim veniam, quis nostrud exercitation ullamco
      laboris nisi ut aliquip ex ea commodo consequat.


      Excepteur sint occaecat cupidatat non proident, sunt in culpa
      qui officia deserunt mollit anim id est laborum\"
}
@end lilypond"
  (stack-lines DOWN 0.0 baseline-skip
               (wordwrap-string-internal-markup-list layout props #f arg)))

(define-builtin-markup-command (justify-string layout props arg)
  (string?)
  align
  ((baseline-skip)
   wordwrap-string-internal-markup-list)
  "Justify a string.  Paragraphs may be separated with double newlines
  
@lilypond[verbatim,quote]
\\markup {
  \\override #'(line-width . 40)
  \\justify-string #\"Lorem ipsum dolor sit amet, consectetur
      adipisicing elit, sed do eiusmod tempor incididunt ut labore
      et dolore magna aliqua.


      Ut enim ad minim veniam, quis nostrud exercitation ullamco
      laboris nisi ut aliquip ex ea commodo consequat.


      Excepteur sint occaecat cupidatat non proident, sunt in culpa
      qui officia deserunt mollit anim id est laborum\"
}
@end lilypond"
  (stack-lines DOWN 0.0 baseline-skip
               (wordwrap-string-internal-markup-list layout props #t arg)))

(define-builtin-markup-command (wordwrap-field layout props symbol)
  (symbol?)
  align
  ()
  "Wordwrap the data which has been assigned to @var{symbol}.
  
@lilypond[verbatim,quote]
\\header {
  title = \"My title\"
  myText = \"Lorem ipsum dolor sit amet, consectetur adipisicing
    elit, sed do eiusmod tempor incididunt ut labore et dolore magna
    aliqua.  Ut enim ad minim veniam, quis nostrud exercitation ullamco
    laboris nisi ut aliquip ex ea commodo consequat.\"
}

\\paper {
  bookTitleMarkup = \\markup {
    \\column {
      \\fill-line { \\fromproperty #'header:title }
      \\null
      \\wordwrap-field #'header:myText
    }
  }
}

\\markup {
  \\null
}
@end lilypond"
  (let* ((m (chain-assoc-get symbol props)))
    (if (string? m)
        (wordwrap-string-markup layout props m)
        empty-stencil)))

(define-builtin-markup-command (justify-field layout props symbol)
  (symbol?)
  align
  ()
  "Justify the data which has been assigned to @var{symbol}.
  
@lilypond[verbatim,quote]
\\header {
  title = \"My title\"
  myText = \"Lorem ipsum dolor sit amet, consectetur adipisicing
    elit, sed do eiusmod tempor incididunt ut labore et dolore magna
    aliqua.  Ut enim ad minim veniam, quis nostrud exercitation ullamco
    laboris nisi ut aliquip ex ea commodo consequat.\"
}

\\paper {
  bookTitleMarkup = \\markup {
    \\column {
      \\fill-line { \\fromproperty #'header:title }
      \\null
      \\justify-field #'header:myText
    }
  }
}

\\markup {
  \\null
}
@end lilypond"
  (let* ((m (chain-assoc-get symbol props)))
    (if (string? m)
        (justify-string-markup layout props m)
        empty-stencil)))

(define-builtin-markup-command (combine layout props arg1 arg2)
  (markup? markup?)
  align
  ()
  "
@cindex merging text

Print two markups on top of each other.

Note: @code{\\combine} cannot take a list of markups enclosed in
curly braces as an argument; the follow example will not compile:

@example
\\combine @{ a list @}
@end example

@lilypond[verbatim,quote]
\\markup {
  \\fontsize #5
  \\override #'(thickness . 2)
  \\combine
    \\draw-line #'(0 . 4)
    \\arrow-head #Y #DOWN ##f
}
@end lilypond"
  (let* ((s1 (interpret-markup layout props arg1))
	 (s2 (interpret-markup layout props arg2)))
    (ly:stencil-add s1 s2)))

;;
;; TODO: should extract baseline-skip from each argument somehow..
;; 
(define-builtin-markup-command (column layout props args)
  (markup-list?)
  align
  ((baseline-skip))
  "
@cindex stacking text in a column

Stack the markups in @var{args} vertically.  The property
@code{baseline-skip} determines the space between markups
in @var{args}.

@lilypond[verbatim,quote]
\\markup {
  \\column {
    one
    two
    three
  }
}
@end lilypond"
  (let ((arg-stencils (interpret-markup-list layout props args)))
    (stack-lines -1 0.0 baseline-skip
                 (remove ly:stencil-empty? arg-stencils))))

(define-builtin-markup-command (dir-column layout props args)
  (markup-list?)
  align
  ((direction)
   (baseline-skip))
  "
@cindex changing direction of text columns

Make a column of @var{args}, going up or down, depending on the
setting of the @code{direction} layout property.

@lilypond[verbatim,quote]
\\markup {
  \\override #`(direction . ,UP) {
    \\dir-column {
      going up
    }
  }
  \\hspace #1
  \\dir-column {
    going down
  }
  \\hspace #1
  \\override #'(direction . 1) {
    \\dir-column {
      going up
    }
  }
}
@end lilypond"
  (stack-lines (if (number? direction) direction -1)
               0.0
               baseline-skip
               (interpret-markup-list layout props args)))

(define (general-column align-dir baseline mols)
  "Stack @var{mols} vertically, aligned to  @var{align-dir} horizontally."
  
  (let* ((aligned-mols (map (lambda (x) (ly:stencil-aligned-to x X align-dir)) mols)))
    (stack-lines -1 0.0 baseline aligned-mols)))

(define-builtin-markup-command (center-column layout props args)
  (markup-list?)
  align
  ((baseline-skip))
  "
@cindex centering a column of text

Put @code{args} in a centered column.

@lilypond[verbatim,quote]
\\markup {
  \\center-column {
    one
    two
    three
  }
}
@end lilypond"
  (general-column CENTER baseline-skip (interpret-markup-list layout props args)))

(define-builtin-markup-command (left-column layout props args)
  (markup-list?)
  align
  ((baseline-skip))
 "
@cindex text columns, left-aligned 

Put @code{args} in a left-aligned column.

@lilypond[verbatim,quote]
\\markup {
  \\left-column {
    one
    two
    three
  }
}
@end lilypond"
  (general-column LEFT baseline-skip (interpret-markup-list layout props args)))

(define-builtin-markup-command (right-column layout props args)
  (markup-list?)
  align
  ((baseline-skip))
 "
@cindex text columns, right-aligned

Put @code{args} in a right-aligned column.

@lilypond[verbatim,quote]
\\markup {
  \\right-column {
    one
    two
    three
  }
}
@end lilypond"
  (general-column RIGHT baseline-skip (interpret-markup-list layout props args)))

(define-builtin-markup-command (vcenter layout props arg)
  (markup?)
  align
  ()
  "
@cindex vertically centering text

Align @code{arg} to its Y@tie{}center.

@lilypond[verbatim,quote]
\\markup {
  one
  \\vcenter
  two
  three
}
@end lilypond"
  (let* ((mol (interpret-markup layout props arg)))
    (ly:stencil-aligned-to mol Y CENTER)))

(define-builtin-markup-command (center-align layout props arg)
  (markup?)
  align
  ()
  "
@cindex horizontally centering text

Align @code{arg} to its X@tie{}center.

@lilypond[verbatim,quote]
\\markup {
  \\column {
    one
    \\center-align
    two
    three
  }
}
@end lilypond"
  (let* ((mol (interpret-markup layout props arg)))
    (ly:stencil-aligned-to mol X CENTER)))

(define-builtin-markup-command (right-align layout props arg)
  (markup?)
  align
  ()
  "
@cindex right aligning text

Align @var{arg} on its right edge.

@lilypond[verbatim,quote]
\\markup {
  \\column {
    one
    \\right-align
    two
    three
  }
}
@end lilypond"
  (let* ((m (interpret-markup layout props arg)))
    (ly:stencil-aligned-to m X RIGHT)))

(define-builtin-markup-command (left-align layout props arg)
  (markup?)
  align
  ()
  "
@cindex left aligning text

Align @var{arg} on its left edge.

@lilypond[verbatim,quote]
\\markup {
  \\column {
    one
    \\left-align
    two
    three
  }
}
@end lilypond"
  (let* ((m (interpret-markup layout props arg)))
    (ly:stencil-aligned-to m X LEFT)))

(define-builtin-markup-command (general-align layout props axis dir arg)
  (integer? number? markup?)
  align
  ()
  "
@cindex controlling general text alignment

Align @var{arg} in @var{axis} direction to the @var{dir} side.

@lilypond[verbatim,quote]
\\markup {
  \\column {
    one
    \\general-align #X #LEFT
    two
    three
    \\null
    one
    \\general-align #X #CENTER
    two
    three
    \\null
    \\line {
      one
      \\general-align #Y #UP
      two
      three
    }
    \\null
    \\line {
      one
      \\general-align #Y #3.2
      two
      three
    }
  }
}
@end lilypond"
  (let* ((m (interpret-markup layout props arg)))
    (ly:stencil-aligned-to m axis dir)))

(define-builtin-markup-command (halign layout props dir arg)
  (number? markup?)
  align
  ()
  "
@cindex setting horizontal text alignment

Set horizontal alignment.  If @var{dir} is @code{-1}, then it is
left-aligned, while @code{+1} is right.  Values in between interpolate
alignment accordingly.

@lilypond[verbatim,quote]
\\markup {
  \\column {
    one
    \\halign #LEFT
    two
    three
    \\null
    one
    \\halign #CENTER
    two
    three
    \\null
    one
    \\halign #RIGHT
    two
    three
    \\null
    one
    \\halign #-5
    two
    three
  }
}
@end lilypond"
  (let* ((m (interpret-markup layout props arg)))
    (ly:stencil-aligned-to m X dir)))

(define-builtin-markup-command (with-dimensions layout props x y arg)
  (number-pair? number-pair? markup?)
  other
  ()
  "
@cindex setting extent of text objects

Set the dimensions of @var{arg} to @var{x} and@tie{}@var{y}."  
  (let* ((m (interpret-markup layout props arg)))
    (ly:make-stencil (ly:stencil-expr m) x y)))

(define-builtin-markup-command (pad-around layout props amount arg)
  (number? markup?)
  align
  ()
  "Add padding @var{amount} all around @var{arg}.
  
@lilypond[verbatim,quote]
\\markup {
  \\box {
    default
  }
  \\hspace #2
  \\box {
    \\pad-around #0.5 {
      padded
    }
  }
}
@end lilypond"
  (let* ((m (interpret-markup layout props arg))
         (x (ly:stencil-extent m X))
         (y (ly:stencil-extent m Y)))
    (ly:make-stencil (ly:stencil-expr m)
                     (interval-widen x amount)
                     (interval-widen y amount))))

(define-builtin-markup-command (pad-x layout props amount arg)
  (number? markup?)
  align
  ()
  "
@cindex padding text horizontally

Add padding @var{amount} around @var{arg} in the X@tie{}direction.

@lilypond[verbatim,quote]
\\markup {
  \\box {
    default
  }
  \\hspace #4
  \\box {
    \\pad-x #2 {
      padded
    }
  }
}
@end lilypond"
  (let* ((m (interpret-markup layout props arg))
         (x (ly:stencil-extent m X))
         (y (ly:stencil-extent m Y)))
    (ly:make-stencil (ly:stencil-expr m)
                     (interval-widen x amount)
                     y)))

(define-builtin-markup-command (put-adjacent layout props axis dir arg1 arg2)
  (integer? ly:dir? markup? markup?)
  align
  ()
  "Put @var{arg2} next to @var{arg1}, without moving @var{arg1}."
  (let ((m1 (interpret-markup layout props arg1))
        (m2 (interpret-markup layout props arg2)))
    (ly:stencil-combine-at-edge m1 axis dir m2 0.0)))

(define-builtin-markup-command (transparent layout props arg)
  (markup?)
  other
  ()
  "Make @var{arg} transparent.
  
@lilypond[verbatim,quote]
\\markup {
  \\transparent {
    invisible text
  }
}
@end lilypond"
  (let* ((m (interpret-markup layout props arg))
         (x (ly:stencil-extent m X))
         (y (ly:stencil-extent m Y)))
    (ly:make-stencil "" x y)))

(define-builtin-markup-command (pad-to-box layout props x-ext y-ext arg)
  (number-pair? number-pair? markup?)
  align
  ()
  "Make @var{arg} take at least @var{x-ext}, @var{y-ext} space.

@lilypond[verbatim,quote]
\\markup {
  \\box {
    default
  }
  \\hspace #4
  \\box {
    \\pad-to-box #'(0 . 10) #'(0 . 3) {
      padded
    }
  }
}
@end lilypond"
  (let* ((m (interpret-markup layout props arg))
         (x (ly:stencil-extent m X))
         (y (ly:stencil-extent m Y)))
    (ly:make-stencil (ly:stencil-expr m)
                     (interval-union x-ext x)
                     (interval-union y-ext y))))

(define-builtin-markup-command (hcenter-in layout props length arg)
  (number? markup?)
  align
  ()
  "Center @var{arg} horizontally within a box of extending
@var{length}/2 to the left and right.

@lilypond[quote,verbatim]
\\new StaffGroup <<
  \\new Staff {
    \\set Staff.instrumentName = \\markup {
      \\hcenter-in #12
      Oboe
    }
    c''1
  }
  \\new Staff {
    \\set Staff.instrumentName = \\markup {
      \\hcenter-in #12
      Bassoon
    }
    \\clef tenor
    c'1
  }
>>
@end lilypond"
  (interpret-markup layout props
                    (make-pad-to-box-markup
                     (cons (/ length -2) (/ length 2))
                     '(0 . 0)
                     (make-center-align-markup arg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; property
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-builtin-markup-command (fromproperty layout props symbol)
  (symbol?)
  other
  ()
  "Read the @var{symbol} from property settings, and produce a stencil
from the markup contained within.  If @var{symbol} is not defined, it
returns an empty markup.

@lilypond[verbatim,quote]
\\header {
  myTitle = \"myTitle\"
  title = \\markup {
    from
    \\italic
    \\fromproperty #'header:myTitle
  }
}
\\markup {
  \\null
}
@end lilypond"
  (let ((m (chain-assoc-get symbol props)))
    (if (markup? m)
        (interpret-markup layout props m)
        empty-stencil)))

(define-builtin-markup-command (on-the-fly layout props procedure arg)
  (symbol? markup?)
  other
  ()
  "Apply the @var{procedure} markup command to @var{arg}.
@var{procedure} should take a single argument."
  (let ((anonymous-with-signature (lambda (layout props arg) (procedure layout props arg))))
    (set-object-property! anonymous-with-signature
			  'markup-signature
			  (list markup?))
    (interpret-markup layout props (list anonymous-with-signature arg))))

(define-builtin-markup-command (override layout props new-prop arg)
  (pair? markup?)
  other
  ()
  "
@cindex overriding properties within text markup

Add the argument @var{new-prop} to the property list.  Properties
may be any property supported by @rinternals{font-interface},
@rinternals{text-interface} and
@rinternals{instrument-specific-markup-interface}.

@lilypond[verbatim,quote]
\\markup {
  \\line {
    \\column {
      default
      baseline-skip
    }
    \\hspace #2
    \\override #'(baseline-skip . 4) {
      \\column {
        increased
        baseline-skip
      }
    }
  }
}
@end lilypond"
  (interpret-markup layout (cons (list new-prop) props) arg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-builtin-markup-command (verbatim-file layout props name)
  (string?)
  other
  ()
  "Read the contents of file @var{name}, and include it verbatim.

@lilypond[verbatim,quote]
\\markup {
  \\verbatim-file #\"simple.ly\"
}
@end lilypond"
  (interpret-markup layout props
                    (if  (ly:get-option 'safe)
                         "verbatim-file disabled in safe mode"
                         (let* ((str (ly:gulp-file name))
                                (lines (string-split str #\nl)))
                           (make-typewriter-markup
                            (make-column-markup lines))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fonts.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-builtin-markup-command (smaller layout props arg)
  (markup?)
  font
  ()
  "Decrease the font size relative to the current setting.
  
@lilypond[verbatim,quote]
\\markup {
  \\fontsize #3.5 {
    some large text
    \\hspace #2
    \\smaller {
      a bit smaller
    }
    \\hspace #2
    more large text
  }
}
@end lilypond"
  (interpret-markup layout props
   `(,fontsize-markup -1 ,arg)))

(define-builtin-markup-command (larger layout props arg)
  (markup?)
  font
  ()
  "Increase the font size relative to the current setting.

@lilypond[verbatim,quote]
\\markup {
  default
  \\hspace #2
  \\larger
  larger
}
@end lilypond"
  (interpret-markup layout props
   `(,fontsize-markup 1 ,arg)))

(define-builtin-markup-command (finger layout props arg)
  (markup?)
  font
  ()
  "Set @var{arg} as small numbers.

@lilypond[verbatim,quote]
\\markup {
  \\finger {
    1 2 3 4 5
  }
}
@end lilypond"
  (interpret-markup layout
                    (cons '((font-size . -5) (font-encoding . fetaNumber)) props)
                    arg))

(define-builtin-markup-command (abs-fontsize layout props size arg)
  (number? markup?)
  font
  ()
  "Use @var{size} as the absolute font size to display @var{arg}.
Adjusts @code{baseline-skip} and @code{word-space} accordingly.

@lilypond[verbatim,quote]
\\markup {
  default text font size
  \\hspace #2
  \\abs-fontsize #16 { text font size 16 }
  \\hspace #2
  \\abs-fontsize #12 { text font size 12 }
}
@end lilypond"
  (let* ((ref-size (ly:output-def-lookup layout 'text-font-size 12))
	 (text-props (list (ly:output-def-lookup layout 'text-font-defaults)))
	 (ref-word-space (chain-assoc-get 'word-space text-props 0.6))
	 (ref-baseline (chain-assoc-get 'baseline-skip text-props 3))
	 (magnification (/ size ref-size)))
    (interpret-markup layout
		      (cons `((baseline-skip . ,(* magnification ref-baseline))
			      (word-space . ,(* magnification ref-word-space))
			      (font-size . ,(magnification->font-size magnification)))
			    props)
		      arg)))

(define-builtin-markup-command (fontsize layout props increment arg)
  (number? markup?)
  font
  ((font-size 0)
   (word-space 1)
   (baseline-skip 2))
  "Add @var{increment} to the font-size.  Adjusts @code{baseline-skip}
accordingly.

@lilypond[verbatim,quote]
\\markup {
  default
  \\hspace #2
  \\fontsize #-1.5
  smaller
}
@end lilypond"
  (let ((entries (list
                  (cons 'baseline-skip (* baseline-skip (magstep increment)))
                  (cons 'word-space (* word-space (magstep increment)))
                  (cons 'font-size (+ font-size increment)))))
    (interpret-markup layout (cons entries props) arg)))

(define-builtin-markup-command (magnify layout props sz arg)
  (number? markup?)
  font
  ()
  "
@cindex magnifying text

Set the font magnification for its argument.  In the following
example, the middle@tie{}A is 10% larger:

@example
A \\magnify #1.1 @{ A @} A
@end example

Note: Magnification only works if a font name is explicitly selected.
Use @code{\\fontsize} otherwise.

@lilypond[verbatim,quote]
\\markup {
  default
  \\hspace #2
  \\magnify #1.5 {
    50% larger
  }
}
@end lilypond"
  (interpret-markup
   layout 
   (prepend-alist-chain 'font-size (magnification->font-size sz) props)
   arg))

(define-builtin-markup-command (bold layout props arg)
  (markup?)
  font
  ()
  "Switch to bold font-series.
  
@lilypond[verbatim,quote]
\\markup {
  default
  \\hspace #2
  \\bold
  bold
}
@end lilypond"
  (interpret-markup layout (prepend-alist-chain 'font-series 'bold props) arg))

(define-builtin-markup-command (sans layout props arg)
  (markup?)
  font
  ()
  "Switch to the sans serif font family.
  
@lilypond[verbatim,quote]
\\markup {
  default
  \\hspace #2
  \\sans {
    sans serif
  }
}
@end lilypond"
  (interpret-markup layout (prepend-alist-chain 'font-family 'sans props) arg))

(define-builtin-markup-command (number layout props arg)
  (markup?)
  font
  ()
  "Set font family to @code{number}, which yields the font used for
time signatures and fingerings.  This font contains numbers and
some punctuation; it has no letters.

@lilypond[verbatim,quote]
\\markup {
  \\number {
    0 1 2 3 4 5 6 7 8 9 . ,
  }
}
@end lilypond"
  (interpret-markup layout (prepend-alist-chain 'font-encoding 'fetaNumber props) arg))

(define-builtin-markup-command (roman layout props arg)
  (markup?)
  font
  ()
  "Set font family to @code{roman}.
  
@lilypond[verbatim,quote]
\\markup {
  \\sans \\bold {
    sans serif, bold
    \\hspace #2
    \\roman {
      text in roman font family
    }
    \\hspace #2
    return to sans
  }
}
@end lilypond"
  (interpret-markup layout (prepend-alist-chain 'font-family 'roman props) arg))

(define-builtin-markup-command (huge layout props arg)
  (markup?)
  font
  ()
  "Set font size to +2.

@lilypond[verbatim,quote]
\\markup {
  default
  \\hspace #2
  \\huge
  huge
}
@end lilypond"
  (interpret-markup layout (prepend-alist-chain 'font-size 2 props) arg))

(define-builtin-markup-command (large layout props arg)
  (markup?)
  font
  ()
  "Set font size to +1.

@lilypond[verbatim,quote]
\\markup {
  default
  \\hspace #2
  \\large
  large
}
@end lilypond"
  (interpret-markup layout (prepend-alist-chain 'font-size 1 props) arg))

(define-builtin-markup-command (normalsize layout props arg)
  (markup?)
  font
  ()
  "Set font size to default.
  
@lilypond[verbatim,quote]
\\markup {
  \\teeny {
    this is very small
    \\hspace #2
    \\normalsize {
      normal size
    }
    \\hspace #2
    teeny again
  }
}
@end lilypond"
  (interpret-markup layout (prepend-alist-chain 'font-size 0 props) arg))

(define-builtin-markup-command (small layout props arg)
  (markup?)
  font
  ()
  "Set font size to -1.
  
@lilypond[verbatim,quote]
\\markup {
  default
  \\hspace #2
  \\small
  small
}
@end lilypond"
  (interpret-markup layout (prepend-alist-chain 'font-size -1 props) arg))

(define-builtin-markup-command (tiny layout props arg)
  (markup?)
  font
  ()
  "Set font size to -2.
  
@lilypond[verbatim,quote]
\\markup {
  default
  \\hspace #2
  \\tiny
  tiny
}
@end lilypond"
  (interpret-markup layout (prepend-alist-chain 'font-size -2 props) arg))

(define-builtin-markup-command (teeny layout props arg)
  (markup?)
  font
  ()
  "Set font size to -3.
  
@lilypond[verbatim,quote]
\\markup {
  default
  \\hspace #2
  \\teeny
  teeny
}
@end lilypond"
  (interpret-markup layout (prepend-alist-chain 'font-size -3 props) arg))

(define-builtin-markup-command (fontCaps layout props arg)
  (markup?)
  font
  ()
  "Set @code{font-shape} to @code{caps}
  
Note: @code{\\fontCaps} requires the installation and selection of
fonts which support the @code{caps} font shape."
  (interpret-markup layout (prepend-alist-chain 'font-shape 'caps props) arg))

;; Poor man's caps
(define-builtin-markup-command (smallCaps layout props arg)
  (markup?)
  font
  ()
  "Emit @var{arg} as small caps.

Note: @code{\\smallCaps} does not support accented characters.

@lilypond[verbatim,quote]
\\markup {
  default
  \\hspace #2
  \\smallCaps {
    Text in small caps
  }
}
@end lilypond"
  (define (char-list->markup chars lower)
    (let ((final-string (string-upcase (reverse-list->string chars))))
      (if lower
	  (markup #:fontsize -2 final-string)
	  final-string)))
  (define (make-small-caps rest-chars currents current-is-lower prev-result)
    (if (null? rest-chars)
	(make-concat-markup
	  (reverse! (cons (char-list->markup currents current-is-lower)
			  prev-result)))
	(let* ((ch (car rest-chars))
	       (is-lower (char-lower-case? ch)))
	  (if (or (and current-is-lower is-lower)
		  (and (not current-is-lower) (not is-lower)))
	      (make-small-caps (cdr rest-chars)
			       (cons ch currents)
			       is-lower
			       prev-result)
	      (make-small-caps (cdr rest-chars)
			       (list ch)
			       is-lower
			       (if (null? currents)
				   prev-result
				   (cons (char-list->markup
					    currents current-is-lower)
					 prev-result)))))))
  (interpret-markup layout props
    (if (string? arg)
	(make-small-caps (string->list arg) (list) #f (list))
	arg)))

(define-builtin-markup-command (caps layout props arg)
  (markup?)
  font
  ()
  "Copy of the @code{\\smallCaps} command.

@lilypond[verbatim,quote]
\\markup {
  default
  \\hspace #2
  \\caps {
    Text in small caps
  }
}
@end lilypond"
  (interpret-markup layout props (make-smallCaps-markup arg)))

(define-builtin-markup-command (dynamic layout props arg)
  (markup?)
  font
  ()
  "Use the dynamic font.  This font only contains @b{s}, @b{f}, @b{m},
@b{z}, @b{p}, and @b{r}.  When producing phrases, like
@q{pi@`{u}@tie{}@b{f}}, the normal words (like @q{pi@`{u}}) should be
done in a different font.  The recommended font for this is bold and italic.
@lilypond[verbatim,quote]
\\markup {
  \\dynamic {
    sfzp
  }
}
@end lilypond"
  (interpret-markup
   layout (prepend-alist-chain 'font-encoding 'fetaDynamic props) arg))

(define-builtin-markup-command (text layout props arg)
  (markup?)
  font
  ()
  "Use a text font instead of music symbol or music alphabet font.
  
@lilypond[verbatim,quote]
\\markup {
  \\number {
    1, 2,
    \\text {
      three, four,
    }
    5
  }
}
@end lilypond"

  ;; ugh - latin1
  (interpret-markup layout (prepend-alist-chain 'font-encoding 'latin1 props)
		    arg))

(define-builtin-markup-command (italic layout props arg)
  (markup?)
  font
  ()
  "Use italic @code{font-shape} for @var{arg}.

@lilypond[verbatim,quote]
\\markup {
  default
  \\hspace #2
  \\italic
  italic
}
@end lilypond"
  (interpret-markup layout (prepend-alist-chain 'font-shape 'italic props) arg))

(define-builtin-markup-command (typewriter layout props arg)
  (markup?)
  font
  ()
  "Use @code{font-family} typewriter for @var{arg}.
  
@lilypond[verbatim,quote]
\\markup {
  default
  \\hspace #2
  \\typewriter
  typewriter
}
@end lilypond"
  (interpret-markup
   layout (prepend-alist-chain 'font-family 'typewriter props) arg))

(define-builtin-markup-command (upright layout props arg)
  (markup?)
  font
  ()
  "Set @code{font-shape} to @code{upright}.  This is the opposite
of @code{italic}.

@lilypond[verbatim,quote]
\\markup {
  \\italic {
    italic text
    \\hspace #2
    \\upright {
      upright text
    }
    \\hspace #2
    italic again
  }
}
@end lilypond"
  (interpret-markup
   layout (prepend-alist-chain 'font-shape 'upright props) arg))

(define-builtin-markup-command (medium layout props arg)
  (markup?)
  font
  ()
  "Switch to medium font-series (in contrast to bold).

@lilypond[verbatim,quote]
\\markup {
  \\bold {
    some bold text
    \\hspace #2
    \\medium {
      medium font series
    }
    \\hspace #2
    bold again
  }
}
@end lilypond"
  (interpret-markup layout (prepend-alist-chain 'font-series 'medium props)
		    arg))

(define-builtin-markup-command (normal-text layout props arg)
  (markup?)
  font
  ()
  "Set all font related properties (except the size) to get the default
normal text font, no matter what font was used earlier.

@lilypond[verbatim,quote]
\\markup {
  \\huge \\bold \\sans \\caps {
    Some text with font overrides
    \\hspace #2
    \\normal-text {
      Default text, same font-size
    }
    \\hspace #2
    More text as before
  }
}
@end lilypond"
  ;; ugh - latin1
  (interpret-markup layout
                    (cons '((font-family . roman) (font-shape . upright)
			    (font-series . medium) (font-encoding . latin1))
			  props)
                    arg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; symbols.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-builtin-markup-command (doublesharp layout props)
  ()
  music
  ()
  "Draw a double sharp symbol.

@lilypond[verbatim,quote]
\\markup {
  \\doublesharp
}
@end lilypond"
  (interpret-markup layout props (markup #:musicglyph (assoc-get 1 standard-alteration-glyph-name-alist ""))))

(define-builtin-markup-command (sesquisharp layout props)
  ()
  music
  ()
  "Draw a 3/2 sharp symbol.

@lilypond[verbatim,quote]
\\markup {
  \\sesquisharp
}
@end lilypond"
  (interpret-markup layout props (markup #:musicglyph (assoc-get 3/4 standard-alteration-glyph-name-alist ""))))					 

(define-builtin-markup-command (sharp layout props)
  ()
  music
  ()
  "Draw a sharp symbol.

@lilypond[verbatim,quote]
\\markup {
  \\sharp
}
@end lilypond"
  (interpret-markup layout props (markup #:musicglyph (assoc-get 1/2 standard-alteration-glyph-name-alist ""))))

(define-builtin-markup-command (semisharp layout props)
  ()
  music
  ()
  "Draw a semisharp symbol.

@lilypond[verbatim,quote]
\\markup {
  \\semisharp
}
@end lilypond"
  (interpret-markup layout props (markup #:musicglyph (assoc-get 1/4 standard-alteration-glyph-name-alist ""))))

(define-builtin-markup-command (natural layout props)
  ()
  music
  ()
  "Draw a natural symbol.

@lilypond[verbatim,quote]
\\markup {
  \\natural
}
@end lilypond"
  (interpret-markup layout props (markup #:musicglyph (assoc-get 0 standard-alteration-glyph-name-alist ""))))

(define-builtin-markup-command (semiflat layout props)
  ()
  music
  ()
  "Draw a semiflat symbol.

@lilypond[verbatim,quote]
\\markup {
  \\semiflat
}
@end lilypond"
  (interpret-markup layout props (markup #:musicglyph (assoc-get -1/4 standard-alteration-glyph-name-alist ""))))

(define-builtin-markup-command (flat layout props)
  ()
  music
  ()
  "Draw a flat symbol.

@lilypond[verbatim,quote]
\\markup {
  \\flat
}
@end lilypond"
  (interpret-markup layout props (markup #:musicglyph (assoc-get -1/2 standard-alteration-glyph-name-alist ""))))

(define-builtin-markup-command (sesquiflat layout props)
  ()
  music
  ()
  "Draw a 3/2 flat symbol.

@lilypond[verbatim,quote]
\\markup {
  \\sesquiflat
}
@end lilypond"
  (interpret-markup layout props (markup #:musicglyph (assoc-get -3/4 standard-alteration-glyph-name-alist ""))))

(define-builtin-markup-command (doubleflat layout props)
  ()
  music
  ()
  "Draw a double flat symbol.

@lilypond[verbatim,quote]
\\markup {
  \\doubleflat
}
@end lilypond"
  (interpret-markup layout props (markup #:musicglyph (assoc-get -1 standard-alteration-glyph-name-alist ""))))

(define-builtin-markup-command (with-color layout props color arg)
  (color? markup?)
  other
  ()
  "
@cindex coloring text

Draw @var{arg} in color specified by @var{color}.

@lilypond[verbatim,quote]
\\markup {
  \\with-color #red
  red
  \\hspace #2
  \\with-color #green
  green
  \\hspace #2
  \\with-color #blue
  blue
}
@end lilypond"
  (let ((stil (interpret-markup layout props arg)))
    (ly:make-stencil (list 'color color (ly:stencil-expr stil))
		     (ly:stencil-extent stil X)
		     (ly:stencil-extent stil Y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; glyphs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-builtin-markup-command (arrow-head layout props axis dir filled)
  (integer? ly:dir? boolean?)
  graphic
  ()
  "Produce an arrow head in specified direction and axis.
Use the filled head if @var{filled} is specified.
@lilypond[verbatim,quote]
\\markup {
  \\fontsize #5 {
    \\general-align #Y #DOWN {
      \\arrow-head #Y #UP ##t
      \\arrow-head #Y #DOWN ##f
      \\hspace #2
      \\arrow-head #X #RIGHT ##f
      \\arrow-head #X #LEFT ##f
    }
  }
}
@end lilypond"
  (let*
      ((name (format "arrowheads.~a.~a~a"
		     (if filled
			 "close"
			 "open")
		     axis
		     dir)))
    (ly:font-get-glyph
     (ly:paper-get-font layout (cons '((font-encoding . fetaMusic))
				     props))
     name)))

(define-builtin-markup-command (musicglyph layout props glyph-name)
  (string?)
  music
  ()
  "@var{glyph-name} is converted to a musical symbol; for example,
@code{\\musicglyph #\"accidentals.natural\"} selects the natural sign from
the music font.  See @ruser{The Feta font} for a complete listing of
the possible glyphs.

@lilypond[verbatim,quote]
\\markup {
  \\musicglyph #\"f\"
  \\musicglyph #\"rests.2\"
  \\musicglyph #\"clefs.G_change\"
}
@end lilypond"
  (let* ((font (ly:paper-get-font layout
				  (cons '((font-encoding . fetaMusic)
					  (font-name . #f))
					
						 props)))
	 (glyph (ly:font-get-glyph font glyph-name)))
    (if (null? (ly:stencil-expr glyph))
	(ly:warning (_ "Cannot find glyph ~a") glyph-name))

    glyph))


(define-builtin-markup-command (lookup layout props glyph-name)
  (string?)
  other
  ()
  "Lookup a glyph by name.
  
@lilypond[verbatim,quote]
\\markup {
  \\override #'(font-encoding . fetaBraces) {
    \\lookup #\"brace200\"
    \\hspace #2
    \\rotate #180
    \\lookup #\"brace180\"
  }
}
@end lilypond"
  (ly:font-get-glyph (ly:paper-get-font layout props)
		     glyph-name))

(define-builtin-markup-command (char layout props num)
  (integer?)
  other
  ()
  "Produce a single character.  Characters encoded in hexadecimal
format require the prefix @code{#x}.

@lilypond[verbatim,quote]
\\markup {
  \\char #65 \\char ##x00a9
}
@end lilypond"
  (ly:text-interface::interpret-markup layout props (ly:wide-char->utf-8 num)))

(define number->mark-letter-vector (make-vector 25 #\A))

(do ((i 0 (1+ i))
     (j 0 (1+ j)))
    ((>= i 26))
  (if (= i (- (char->integer #\I) (char->integer #\A)))
      (set! i (1+ i)))
  (vector-set! number->mark-letter-vector j
               (integer->char (+ i (char->integer #\A)))))

(define number->mark-alphabet-vector (list->vector
  (map (lambda (i) (integer->char (+ i (char->integer #\A)))) (iota 26))))

(define (number->markletter-string vec n)
  "Double letters for big marks."
  (let* ((lst (vector-length vec)))
    
    (if (>= n lst)
	(string-append (number->markletter-string vec (1- (quotient n lst)))
		       (number->markletter-string vec (remainder n lst)))
	(make-string 1 (vector-ref vec n)))))

(define-builtin-markup-command (markletter layout props num)
  (integer?)
  other
  ()
  "Make a markup letter for @var{num}.  The letters start with A to@tie{}Z
(skipping letter@tie{}I), and continue with double letters.

@lilypond[verbatim,quote]
\\markup {
  \\markletter #8
  \\hspace #2
  \\markletter #26
}
@end lilypond"
  (ly:text-interface::interpret-markup layout props
    (number->markletter-string number->mark-letter-vector num)))

(define-builtin-markup-command (markalphabet layout props num)
  (integer?)
  other
  ()
   "Make a markup letter for @var{num}.  The letters start with A to@tie{}Z
and continue with double letters.

@lilypond[verbatim,quote]
\\markup {
  \\markalphabet #8
  \\hspace #2
  \\markalphabet #26
}
@end lilypond"
   (ly:text-interface::interpret-markup layout props
     (number->markletter-string number->mark-alphabet-vector num)))

(define-public (horizontal-slash-interval num forward number-interval mag)
  (if forward
    (cond ;((= num 6) (interval-widen number-interval (* mag 0.5)))
          ;((= num 5) (interval-widen number-interval (* mag 0.5)))
          (else (interval-widen number-interval (* mag 0.25))))
    (cond ((= num 6) (interval-widen number-interval (* mag 0.5)))
          ;((= num 5) (interval-widen number-interval (* mag 0.5)))
          (else (interval-widen number-interval (* mag 0.25))))
  ))

(define-public (adjust-slash-stencil num forward stencil mag)
  (if forward
    (cond ((= num 2)
              (ly:stencil-translate stencil (cons (* mag -0.00) (* mag 0.2))))
          ((= num 3)
              (ly:stencil-translate stencil (cons (* mag -0.00) (* mag 0.2))))
          ;((= num 5)
              ;(ly:stencil-translate stencil (cons (* mag -0.00) (* mag -0.07))))
          ;((= num 7)
          ;    (ly:stencil-translate stencil (cons (* mag -0.00) (* mag -0.15))))
          (else stencil))
    (cond ((= num 6)
              (ly:stencil-translate stencil (cons (* mag -0.00) (* mag 0.15))))
          ;((= num 8)
          ;    (ly:stencil-translate stencil (cons (* mag -0.00) (* mag -0.15))))
          (else stencil))
  )
)

(define (slashed-digit-internal layout props num forward font-size thickness)
  (let* ((mag (magstep font-size))
         (thickness (* mag
                       (ly:output-def-lookup layout 'line-thickness)
                       thickness))
         ; backward slashes might use slope and point in the other direction!
         (dy (* mag (if forward 0.4 -0.4)))
         (number-stencil (interpret-markup layout
                                           (prepend-alist-chain 'font-encoding 'fetaNumber props)
                                           (number->string num)))
         (num-x (horizontal-slash-interval num forward (ly:stencil-extent number-stencil X) mag))
         (center (interval-center (ly:stencil-extent number-stencil Y)))
         ; Use the real extents of the slash, not the whole number, because we
         ; might translate the slash later on!
         (num-y (interval-widen (cons center center) (abs dy)))
         (is-sane (and (interval-sane? num-x) (interval-sane? num-y)))
         (slash-stencil (if is-sane
                            (make-line-stencil thickness 
                                         (car num-x) (- (interval-center num-y) dy)
                                         (cdr num-x) (+ (interval-center num-y) dy))
                            #f)))
    (if (ly:stencil? slash-stencil)
      (begin
        ; for some numbers we need to shift the slash/backslash up or down to make
        ; the slashed digit look better
        (set! slash-stencil (adjust-slash-stencil num forward slash-stencil mag))
        (set! number-stencil
          (ly:stencil-add number-stencil slash-stencil)))
      (ly:warning "Unable to create slashed digit ~a" num))
    number-stencil))


(define-builtin-markup-command (slashed-digit layout props num)
  (integer?)
  other
  ((font-size 0)
   (thickness 1.6))
  "
@cindex slashed digits

A feta number, with slash.  This is for use in the context of
figured bass notation.
@lilypond[verbatim,quote]
\\markup {
  \\slashed-digit #5
  \\hspace #2
  \\override #'(thickness . 3)
  \\slashed-digit #7
}
@end lilypond"
  (slashed-digit-internal layout props num #t font-size thickness))

(define-builtin-markup-command (backslashed-digit layout props num)
  (integer?)
  other
  ((font-size 0)
   (thickness 1.6))
  "
@cindex backslashed digits

A feta number, with backslash.  This is for use in the context of
figured bass notation.
@lilypond[verbatim,quote]
\\markup {
  \\backslashed-digit #5
  \\hspace #2
  \\override #'(thickness . 3)
  \\backslashed-digit #7
}
@end lilypond"
  (slashed-digit-internal layout props num #f font-size thickness))

;; eyeglasses
(define eyeglassesps 
     "0.15 setlinewidth
      -0.9 0 translate
      1.1 1.1 scale
      1.2 0.7 moveto
      0.7 0.7 0.5 0 361 arc
      stroke
      2.20 0.70 0.50 0 361 arc
      stroke
      1.45 0.85 0.30 0 180 arc
      stroke
      0.20 0.70 moveto
      0.80 2.00 lineto
      0.92 2.26 1.30 2.40 1.15 1.70 curveto
      stroke
      2.70 0.70 moveto
      3.30 2.00 lineto
      3.42 2.26 3.80 2.40 3.65 1.70 curveto
      stroke")

(define-builtin-markup-command (eyeglasses layout props) () other ()
  "Prints out eyeglasses, indicating strongly to look at the conductor.
@lilypond[verbatim,quote]
\\markup { \\eyeglasses }
@end lilypond"
  (interpret-markup layout props
    (make-with-dimensions-markup '(-0.55 . 2.9) '(0.4 . 2.4)
      (make-postscript-markup eyeglassesps))))

(define-builtin-markup-command (left-brace layout props size)
  (number?)
  other
  ()
  "
A feta brace in point size @var{size}.

@lilypond[verbatim,quote]
\\markup {
  \\left-brace #35
  \\hspace #2
  \\left-brace #45
}
@end lilypond"
  (let* ((font (ly:paper-get-font layout
                                  (cons '((font-encoding . fetaBraces)
                                          (font-name . #f))
                                        props)))
	 (glyph-count (1- (ly:otf-glyph-count font)))
         (scale (ly:output-def-lookup layout 'output-scale))
         (scaled-size (/ (ly:pt size) scale))
         (glyph (lambda (n)
                  (ly:font-get-glyph font (string-append "brace"
							 (number->string n)))))
	 (get-y-from-brace (lambda (brace)
			     (interval-length
			      (ly:stencil-extent (glyph brace) Y))))
         (find-brace (binary-search 0 glyph-count get-y-from-brace scaled-size))
         (glyph-found (glyph find-brace)))

    (if (or (null? (ly:stencil-expr glyph-found))
	    (< scaled-size (interval-length (ly:stencil-extent (glyph 0) Y)))
	    (> scaled-size (interval-length
			    (ly:stencil-extent (glyph glyph-count) Y))))
        (begin
          (ly:warning (_ "no brace found for point size ~S ") size)
          (ly:warning (_ "defaulting to ~S pt")
		      (/ (* scale (interval-length
				   (ly:stencil-extent glyph-found Y)))
			 (ly:pt 1)))))
    glyph-found))

(define-builtin-markup-command (right-brace layout props size)
  (number?)
  other
  ()
  "
A feta brace in point size @var{size}, rotated 180 degrees.

@lilypond[verbatim,quote]
\\markup {
  \\right-brace #45
  \\hspace #2
  \\right-brace #35
}
@end lilypond"
  (interpret-markup layout props (markup #:rotate 180 #:left-brace size)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the note command.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: better syntax.

(define-builtin-markup-command (note-by-number layout props log dot-count dir)
  (number? number? number?)
  music
  ((font-size 0)
   (style '()))
  "
@cindex notes within text by log and dot-count

Construct a note symbol, with stem.  By using fractional values for
@var{dir}, longer or shorter stems can be obtained.

@lilypond[verbatim,quote]
\\markup {
  \\note-by-number #3 #0 #DOWN
  \\hspace #2
  \\note-by-number #1 #2 #0.8
}
@end lilypond"
  (define (get-glyph-name-candidates dir log style)
    (map (lambda (dir-name)
     (format "noteheads.~a~a~a" dir-name (min log 2)
	     (if (and (symbol? style)
		      (not (equal? 'default style)))
		 (symbol->string style)
		 "")))
	 (list (if (= dir UP) "u" "d")
	       "s")))
		   
  (define (get-glyph-name font cands)
    (if (null? cands)
     ""
     (if (ly:stencil-empty? (ly:font-get-glyph font (car cands)))
	 (get-glyph-name font (cdr cands))
	 (car cands))))
    
  (let* ((font (ly:paper-get-font layout (cons '((font-encoding . fetaMusic)) props)))
	 (size-factor (magstep font-size))
         (stem-length (*  size-factor (max 3 (- log 1))))
         (head-glyph-name (get-glyph-name font (get-glyph-name-candidates (sign dir) log style)))
         (head-glyph (ly:font-get-glyph font head-glyph-name))
	 (attach-indices (ly:note-head::stem-attachment font head-glyph-name))
         (stem-thickness (* size-factor 0.13))
         (stemy (* dir stem-length))
         (attach-off (cons (interval-index
			    (ly:stencil-extent head-glyph X)
			    (* (sign dir) (car attach-indices)))
			   (* (sign dir)	; fixme, this is inconsistent between X & Y.
			      (interval-index
			       (ly:stencil-extent head-glyph Y)
			       (cdr attach-indices)))))
         (stem-glyph (and (> log 0)
			  (ly:round-filled-box
			   (ordered-cons (car attach-off)
					 (+ (car attach-off)  (* (- (sign dir)) stem-thickness)))
			   (cons (min stemy (cdr attach-off))
				 (max stemy (cdr attach-off)))
			   (/ stem-thickness 3))))
	 
         (dot (ly:font-get-glyph font "dots.dot"))
         (dotwid (interval-length (ly:stencil-extent dot X)))
         (dots (and (> dot-count 0)
                    (apply ly:stencil-add
                           (map (lambda (x)
                                  (ly:stencil-translate-axis
                                   dot (* 2 x dotwid) X))
                                (iota dot-count)))))
         (flaggl (and (> log 2)
                      (ly:stencil-translate
                       (ly:font-get-glyph font
					  (string-append "flags."
							 (if (> dir 0) "u" "d")
							 (number->string log)))
                       (cons (+ (car attach-off) (if (< dir 0) stem-thickness 0)) stemy)))))

    ; If there is a flag on an upstem and the stem is short, move the dots to avoid the flag.
    ; 16th notes get a special case because their flags hang lower than any other flags.
    (if (and dots (> dir 0) (> log 2) (or (< dir 1.15) (and (= log 4) (< dir 1.3))))
	(set! dots (ly:stencil-translate-axis dots 0.5 X)))
    (if flaggl
        (set! stem-glyph (ly:stencil-add flaggl stem-glyph)))
    (if (ly:stencil? stem-glyph)
        (set! stem-glyph (ly:stencil-add stem-glyph head-glyph))
        (set! stem-glyph head-glyph))
    (if (ly:stencil? dots)
        (set! stem-glyph
              (ly:stencil-add
               (ly:stencil-translate-axis
		dots
		(+ (cdr (ly:stencil-extent head-glyph X)) dotwid)
		X)
               stem-glyph)))
    stem-glyph))

(define-public log2 
  (let ((divisor (log 2)))
    (lambda (z) (inexact->exact (/ (log z) divisor)))))

(define (parse-simple-duration duration-string)
  "Parse the `duration-string', e.g. ''4..'' or ''breve.'', and return a (log dots) list."
  (let ((match (regexp-exec (make-regexp "(breve|longa|maxima|[0-9]+)(\\.*)") duration-string)))
    (if (and match (string=? duration-string (match:substring match 0)))
        (let ((len  (match:substring match 1))
              (dots (match:substring match 2)))
          (list (cond ((string=? len "breve") -1)
                      ((string=? len "longa") -2)
                      ((string=? len "maxima") -3)
                      (else (log2 (string->number len))))
                (if dots (string-length dots) 0)))
        (ly:error (_ "not a valid duration string: ~a") duration-string))))

(define-builtin-markup-command (note layout props duration dir)
  (string? number?)
  music
  (note-by-number-markup)
  "
@cindex notes within text by string

This produces a note with a stem pointing in @var{dir} direction, with
the @var{duration} for the note head type and augmentation dots.  For
example, @code{\\note #\"4.\" #-0.75} creates a dotted quarter note, with
a shortened down stem.

@lilypond[verbatim,quote]
\\markup {
  \\override #'(style . cross) {
    \\note #\"4..\" #UP
  }
  \\hspace #2
  \\note #\"breve\" #0
}
@end lilypond"
  (let ((parsed (parse-simple-duration duration)))
    (note-by-number-markup layout props (car parsed) (cadr parsed) dir)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; translating.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-builtin-markup-command (lower layout props amount arg)
  (number? markup?)
  align
  ()
  "
@cindex lowering text

Lower @var{arg} by the distance @var{amount}.
A negative @var{amount} indicates raising; see also @code{\\raise}.

@lilypond[verbatim,quote]
\\markup {
  one
  \\lower #3
  two
  three
}
@end lilypond"
  (ly:stencil-translate-axis (interpret-markup layout props arg)
			     (- amount) Y))

(define-builtin-markup-command (translate-scaled layout props offset arg)
  (number-pair? markup?)
  align
  ((font-size 0))
  "
@cindex translating text
@cindex scaling text

Translate @var{arg} by @var{offset}, scaling the offset by the
@code{font-size}.

@lilypond[verbatim,quote]
\\markup {
  \\fontsize #5 {
    * \\translate #'(2 . 3) translate
    \\hspace #2
    * \\translate-scaled #'(2 . 3) translate-scaled
  }
}
@end lilypond"
  (let* ((factor (magstep font-size))
         (scaled (cons (* factor (car offset))
                       (* factor (cdr offset)))))
    (ly:stencil-translate (interpret-markup layout props arg)
                          scaled)))

(define-builtin-markup-command (raise layout props amount arg)
  (number? markup?)
  align
  ()
  "
@cindex raising text
  
Raise @var{arg} by the distance @var{amount}.
A negative @var{amount} indicates lowering, see also @code{\\lower}.

The argument to @code{\\raise} is the vertical displacement amount,
measured in (global) staff spaces.  @code{\\raise} and @code{\\super}
raise objects in relation to their surrounding markups.

If the text object itself is positioned above or below the staff, then
@code{\\raise} cannot be used to move it, since the mechanism that
positions it next to the staff cancels any shift made with
@code{\\raise}.  For vertical positioning, use the @code{padding}
and/or @code{extra-offset} properties.

@lilypond[verbatim,quote]
\\markup {
  C
  \\small
  \\bold
  \\raise #1.0
  9/7+
}
@end lilypond"
  (ly:stencil-translate-axis (interpret-markup layout props arg) amount Y))

(define-builtin-markup-command (fraction layout props arg1 arg2)
  (markup? markup?)
  other
  ((font-size 0))
  "
@cindex creating text fractions

Make a fraction of two markups.
@lilypond[verbatim,quote]
\\markup {
  π ≈
  \\fraction 355 113
}
@end lilypond"
  (let* ((m1 (interpret-markup layout props arg1))
         (m2 (interpret-markup layout props arg2))
         (factor (magstep font-size))
         (boxdimen (cons (* factor -0.05) (* factor 0.05)))
         (padding (* factor 0.2))
         (baseline (* factor 0.6))
         (offset (* factor 0.75)))
    (set! m1 (ly:stencil-aligned-to m1 X CENTER))
    (set! m2 (ly:stencil-aligned-to m2 X CENTER))
    (let* ((x1 (ly:stencil-extent m1 X))
           (x2 (ly:stencil-extent m2 X))
           (line (ly:round-filled-box (interval-union x1 x2) boxdimen 0.0))
           ;; should stack mols separately, to maintain LINE on baseline
           (stack (stack-lines DOWN padding baseline (list m1 line m2))))
      (set! stack
	    (ly:stencil-aligned-to stack Y CENTER))
      (set! stack
	    (ly:stencil-aligned-to stack X LEFT))
      ;; should have EX dimension
      ;; empirical anyway
      (ly:stencil-translate-axis stack offset Y))))

(define-builtin-markup-command (normal-size-super layout props arg)
  (markup?)
  font
  ((baseline-skip))
  "
@cindex setting superscript in standard font size

Set @var{arg} in superscript with a normal font size.

@lilypond[verbatim,quote]
\\markup {
  default
  \\normal-size-super {
    superscript in standard size
  }
}
@end lilypond"
  (ly:stencil-translate-axis
   (interpret-markup layout props arg)
   (* 0.5 baseline-skip) Y))

(define-builtin-markup-command (super layout props arg)
  (markup?)
  font
  ((font-size 0)
   (baseline-skip))
  "  
@cindex superscript text

Set @var{arg} in superscript.

@lilypond[verbatim,quote]
\\markup {
  E =
  \\concat {
    mc
    \\super
    2
  }
}
@end lilypond"
  (ly:stencil-translate-axis
   (interpret-markup
    layout
    (cons `((font-size . ,(- font-size 3))) props)
    arg)
   (* 0.5 baseline-skip)
   Y))

(define-builtin-markup-command (translate layout props offset arg)
  (number-pair? markup?)
  align
  ()
  "
@cindex translating text
  
Translate @var{arg} relative to its surroundings.  @var{offset}
is a pair of numbers representing the displacement in the X and Y axis.

@lilypond[verbatim,quote]
\\markup {
  *
  \\translate #'(2 . 3)
  \\line { translated two spaces right, three up }
}
@end lilypond"
  (ly:stencil-translate (interpret-markup layout props arg)
			offset))

(define-builtin-markup-command (sub layout props arg)
  (markup?)
  font
  ((font-size 0)
   (baseline-skip))
  "
@cindex subscript text

Set @var{arg} in subscript.

@lilypond[verbatim,quote]
\\markup {
  \\concat {
    H
    \\sub {
      2
    }
    O
  }
}
@end lilypond"
  (ly:stencil-translate-axis
   (interpret-markup
    layout
    (cons `((font-size . ,(- font-size 3))) props)
    arg)
   (* -0.5 baseline-skip)
   Y))

(define-builtin-markup-command (normal-size-sub layout props arg)
  (markup?)
  font
  ((baseline-skip))
  "
@cindex setting subscript in standard font size

Set @var{arg} in subscript with a normal font size.

@lilypond[verbatim,quote]
\\markup {
  default
  \\normal-size-sub {
    subscript in standard size
  }
}
@end lilypond"
  (ly:stencil-translate-axis
   (interpret-markup layout props arg)
   (* -0.5 baseline-skip)
   Y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; brackets.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-builtin-markup-command (hbracket layout props arg)
  (markup?)
  graphic
  ()
  "
@cindex placing horizontal brackets around text
  
Draw horizontal brackets around @var{arg}.

@lilypond[verbatim,quote]
\\markup {
  \\hbracket {
    \\line {
      one two three
    }
  }
}
@end lilypond"
  (let ((th 0.1) ;; todo: take from GROB.
        (m (interpret-markup layout props arg)))
    (bracketify-stencil m X th (* 2.5 th) th)))

(define-builtin-markup-command (bracket layout props arg)
  (markup?)
  graphic
  ()
  "
@cindex placing vertical brackets around text
  
Draw vertical brackets around @var{arg}.

@lilypond[verbatim,quote]
\\markup {
  \\bracket {
    \\note #\"2.\" #UP
  }
}
@end lilypond"
  (let ((th 0.1) ;; todo: take from GROB.
        (m (interpret-markup layout props arg)))
    (bracketify-stencil m Y th (* 2.5 th) th)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Delayed markup evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-builtin-markup-command (page-ref layout props label gauge default)
  (symbol? markup? markup?)
  other
  ()
  "
@cindex referencing page numbers in text

Reference to a page number. @var{label} is the label set on the referenced
page (using the @code{\\label} command), @var{gauge} a markup used to estimate
the maximum width of the page number, and @var{default} the value to display
when @var{label} is not found."
  (let* ((gauge-stencil (interpret-markup layout props gauge))
	 (x-ext (ly:stencil-extent gauge-stencil X))
	 (y-ext (ly:stencil-extent gauge-stencil Y)))
    (ly:make-stencil
     `(delay-stencil-evaluation
       ,(delay (ly:stencil-expr
		(let* ((table (ly:output-def-lookup layout 'label-page-table))
		       (label-page (and (list? table) (assoc label table)))
		       (page-number (and label-page (cdr label-page)))
		       (page-markup (if page-number (format "~a" page-number) default))
		       (page-stencil (interpret-markup layout props page-markup))
		       (gap (- (interval-length x-ext)
			       (interval-length (ly:stencil-extent page-stencil X)))))
		  (interpret-markup layout props
				    (markup #:concat (#:hspace gap page-markup)))))))
     x-ext
     y-ext)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markup list commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (space-lines baseline stils)
  (let space-stil ((stils stils)
		   (result (list)))
    (if (null? stils)
	(reverse! result)
	(let* ((stil (car stils))
	       (dy-top (max (- (/ baseline 1.5)
			       (interval-bound (ly:stencil-extent stil Y) UP))
			    0.0))
	       (dy-bottom (max (+ (/ baseline 3.0)
				  (interval-bound (ly:stencil-extent stil Y) DOWN))
			       0.0))
	       (new-stil (ly:make-stencil
			  (ly:stencil-expr stil)
			  (ly:stencil-extent stil X)
			  (cons (- (interval-bound (ly:stencil-extent stil Y) DOWN)
				   dy-bottom)
				(+ (interval-bound (ly:stencil-extent stil Y) UP)
				   dy-top)))))
	  (space-stil (cdr stils) (cons new-stil result))))))

(define-builtin-markup-list-command (justified-lines layout props args)
  (markup-list?)
  ((baseline-skip)
   wordwrap-internal-markup-list)
  "
@cindex justifying lines of text

Like @code{\\justify}, but return a list of lines instead of a single markup.
Use @code{\\override-lines #'(line-width . @var{X})} to set the line width;
@var{X}@tie{}is the number of staff spaces."
  (space-lines baseline-skip
               (interpret-markup-list layout props
                                      (make-wordwrap-internal-markup-list #t args))))

(define-builtin-markup-list-command (wordwrap-lines layout props args)
  (markup-list?)
  ((baseline-skip)
   wordwrap-internal-markup-list)
  "Like @code{\\wordwrap}, but return a list of lines instead of a single markup.
Use @code{\\override-lines #'(line-width . @var{X})} to set the line width,
where @var{X} is the number of staff spaces."
  (space-lines baseline-skip
               (interpret-markup-list layout props
                                      (make-wordwrap-internal-markup-list #f args))))

(define-builtin-markup-list-command (column-lines layout props args)
  (markup-list?)
  ((baseline-skip))
  "Like @code{\\column}, but return a list of lines instead of a single markup.
@code{baseline-skip} determines the space between each markup in @var{args}."
  (space-lines (chain-assoc-get 'baseline-skip props)
	       (interpret-markup-list layout props args)))

(define-builtin-markup-list-command (override-lines layout props new-prop args)
  (pair? markup-list?)
  ()
  "Like @code{\\override}, for markup lists."
  (interpret-markup-list layout (cons (list new-prop) props) args))
