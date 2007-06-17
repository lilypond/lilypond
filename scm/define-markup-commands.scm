;;;; define-markup-commands.scm -- markup commands
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 2000--2006  Han-Wen Nienhuys <hanwen@xs4all.nl>
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
  "A simple line.  Uses the @code{thickness} property."
  (let*
      ((th (chain-assoc-get 'thickness props  0.1))
       (x (car dest))
       (y (cdr dest))
       (s (ly:make-stencil
	   `(draw-line
	     ,th
	     0 0
	     ,x ,y)

	   (cons (min x 0) (max x 0))
	   (cons (min y 0) (max y 0)))))

    s))

(define-builtin-markup-command (draw-circle layout props radius thickness fill)
  (number? number? boolean?)
  "A circle of radius @var{radius}, thickness @var{thickness} and
optionally filled."
  (make-circle-stencil radius thickness fill))

(define-builtin-markup-command (triangle layout props filled) (boolean?)
  "A triangle, either filled or empty."
  (let*
      ((th (chain-assoc-get 'thickness props  0.1))
       (size (chain-assoc-get 'font-size props 0))
       (ex (* (magstep size)
	      0.8
	      (chain-assoc-get 'baseline-skip props 2))))

    (ly:make-stencil
     `(polygon '(0.0 0.0
		     ,ex 0.0
		     ,(* 0.5 ex)
		     ,(* 0.86 ex))
	   ,th
	   ,filled)

     (cons 0 ex)
     (cons 0 (* .86 ex))
     )))

(define-builtin-markup-command (circle layout props arg) (markup?)
  "Draw a circle around @var{arg}.  Use @code{thickness},
@code{circle-padding} and @code{font-size} properties to determine line
thickness and padding around the markup."
  
  (let* ((th (chain-assoc-get 'thickness props  0.1))
	 (size (chain-assoc-get 'font-size props 0))
	 (pad
	  (* (magstep size)
	     (chain-assoc-get 'circle-padding props 0.2)))
	 (m (interpret-markup layout props arg)))
    (circle-stencil m th pad)))

(define-builtin-markup-command (with-url layout props url arg) (string? markup?)
  "Add a link to URL @var{url} around @var{arg}.  This only works in
the PDF backend."
  (let* ((stil (interpret-markup layout props arg))
	 (xextent (ly:stencil-extent stil X))
	 (yextent (ly:stencil-extent stil Y))
	 (old-expr (ly:stencil-expr stil))
	 (url-expr (list 'url-link url `(quote ,xextent) `(quote ,yextent))))

    (ly:stencil-add (ly:make-stencil url-expr xextent yextent) stil)))


(define-builtin-markup-command (beam layout props width slope thickness)
  (number? number? number?)
  "Create a beam with the specified parameters."
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

(define-builtin-markup-command (box layout props arg) (markup?)
  "Draw a box round @var{arg}.  Looks at @code{thickness},
@code{box-padding} and @code{font-size} properties to determine line
thickness and padding around the markup."
  
  (let* ((th (chain-assoc-get 'thickness props  0.1))
	 (size (chain-assoc-get 'font-size props 0))
	 (pad (* (magstep size)
		 (chain-assoc-get 'box-padding props 0.2)))
	 (m (interpret-markup layout props arg)))
    (box-stencil m th pad)))

(define-builtin-markup-command (filled-box layout props xext yext blot)
  (number-pair? number-pair? number?)
  "Draw a box with rounded corners of dimensions @var{xext} and
@var{yext}.  For example,
@verbatim
\\filled-box #'(-.3 . 1.8) #'(-.3 . 1.8) #0
@end verbatim
creates a box extending horizontally from -0.3 to 1.8 and
vertically from -0.3 up to 1.8, with corners formed from a
circle of diameter@tie{}0 (i.e. sharp corners)."
  (ly:round-filled-box
   xext yext blot))

(define-builtin-markup-command (rotate layout props ang arg) (number? markup?)
  "Rotate object with @var{ang} degrees around its center."
  (let* ((stil (interpret-markup layout props arg)))
    (ly:stencil-rotate stil ang 0 0)))


(define-builtin-markup-command (whiteout layout props arg) (markup?)
  "Provide a white underground for @var{arg}."
  (stencil-whiteout (interpret-markup layout props arg)))

(define-builtin-markup-command (pad-markup layout props padding arg) (number? markup?)
  "Add space around a markup object."

  (let*
      ((stil (interpret-markup layout props arg))
       (xext (ly:stencil-extent stil X))
       (yext (ly:stencil-extent stil Y)))

    (ly:make-stencil
     (ly:stencil-expr stil)
     (interval-widen xext padding)
     (interval-widen yext padding))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; space
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;FIXME: is this working? 
(define-builtin-markup-command (strut layout props) ()
  "Create a box of the same height as the space in the current font."
  (let ((m (ly:text-interface::interpret-markup layout props " ")))
    (ly:make-stencil (ly:stencil-expr m)
		     '(1000 . -1000)
		     (ly:stencil-extent m X)
		     )))


;; todo: fix negative space
(define-builtin-markup-command (hspace layout props amount) (number?)
  "This produces a invisible object taking horizontal space.  For example,

@example 
\\markup @{ A \\hspace #2.0 B @} 
@end example

@noindent
puts extra space between A and@tie{}B, on top of the space that is
normally inserted before elements on a line."
  (if (> amount 0)
      (ly:make-stencil "" (cons 0 amount) '(-1 . 1))
      (ly:make-stencil "" (cons amount amount) '(-1 . 1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; importing graphics.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-builtin-markup-command (stencil layout props stil) (ly:stencil?)
  "Use a stencil as markup."
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

(define-builtin-markup-command (epsfile layout props axis size file-name) (number? number? string?)
  "Inline an EPS image.  The image is scaled along @var{axis} to
@var{size}."

  (if (ly:get-option 'safe)
      (interpret-markup layout props "not allowed in safe")
      (eps-file->stencil axis size file-name)
      ))

(define-builtin-markup-command (postscript layout props str) (string?)
  "This inserts @var{str} directly into the output as a PostScript
command string.  Due to technicalities of the output backends,
different scales should be used for the @TeX{} and PostScript backend,
selected with @code{-f}. 

For the @TeX{} backend, the following string prints a rotated text

@cindex rotated text

@example
0 0 moveto /ecrm10 findfont 
1.75 scalefont setfont 90 rotate (hello) show
@end example

@noindent
The magical constant 1.75 scales from LilyPond units (staff spaces) to
@TeX{} dimensions.

For the postscript backend, use the following

@example
gsave /ecrm10 findfont 
 10.0 output-scale div 
 scalefont setfont 90 rotate (hello) show grestore 
@end example"

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


(define-builtin-markup-command (score layout props score) (ly:score?)
  "Inline an image of music."
  (let* ((output (ly:score-embedded-format score layout)))

    (if (ly:music-output? output)
	(paper-system-stencil
	 (vector-ref (ly:paper-score-paper-systems output) 0))
	(begin
	  (ly:warning (_"no systems found in \\score markup, does it have a \\layout block?"))
	  empty-stencil))))

(define-builtin-markup-command (null layout props) ()
  "An empty markup with extents of a single point."

  point-stencil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic formatting.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define-builtin-markup-command (simple layout props str) (string?)
  "A simple text string; @code{\\markup @{ foo @}} is equivalent with
@code{\\markup @{ \\simple #\"foo\" @}}."
  (interpret-markup layout props str))

(define-builtin-markup-command (tied-lyric layout props str) (string?)
  "Like simple-markup, but use tie characters for @q{~} tilde symbols."
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
	Return a list of paddings.
"
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

(define-builtin-markup-command (fill-line layout props markups)
  (markup-list?)
  "Put @var{markups} in a horizontal line of width @var{line-width}.
The markups are spaced or flushed to fill the entire line.
If there are no arguments, return an empty stencil."
 
  (let* ((orig-stencils (interpret-markup-list layout props markups))
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
	 (text-dir (chain-assoc-get 'text-direction props RIGHT))
	 (word-count (length stencils))
	 (word-space (chain-assoc-get 'word-space props 1))
	 (prop-line-width (chain-assoc-get 'line-width props #f))
	 (line-width (if prop-line-width prop-line-width
			 (ly:output-def-lookup layout 'line-width)))
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

    (if (= text-dir LEFT)
	(set! line-stencils (reverse line-stencils)))

    (if (null? (remove ly:stencil-empty? orig-stencils))
	empty-stencil
	(stack-stencils-padding-list X
				     RIGHT fill-space-normal line-stencils))))
	
(define-builtin-markup-command (line layout props args) (markup-list?)
  "Put @var{args} in a horizontal line.  The property @code{word-space}
determines the space between each markup in @var{args}."
  (let*
      ((stencils (interpret-markup-list layout props args))
       (space    (chain-assoc-get 'word-space props))
       (text-dir (chain-assoc-get 'text-direction props RIGHT)) 
       )

    (if (= text-dir LEFT)
	(set! stencils (reverse stencils)))
    

    (stack-stencil-line
     space
     (remove ly:stencil-empty? stencils))))

(define-builtin-markup-command (concat layout props args) (markup-list?)
  "Concatenate @var{args} in a horizontal line, without spaces inbetween.
Strings and simple markups are concatenated on the input level, allowing
ligatures.  For example, @code{\\concat @{ \"f\" \\simple #\"i\" @}} is
equivalent to @code{\"fi\"}."

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
	(let*
	    ((first (car stencils))
	     (first-wid (cdr (ly:stencil-extent (car stencils) X)))
	     (newwid (+ space first-wid accumulated-width))
	     )

	  (if
	   (or (null? accumulator)
	       (< newwid width))

	   (take-list width space
		      (cdr stencils)
		      (cons first accumulator)
		      newwid)
	     (cons accumulator stencils))
	   )))

    (let loop
	((lines '())
	 (todo stencils))

      (let*
	  ((line-break (take-list line-width space todo
				 '() 0.0))
	   (line-stencils (car line-break))
	   (space-left (- line-width (apply + (map (lambda (x) (cdr (ly:stencil-extent x X)))
					      line-stencils))))

	   (line-word-space (cond
			     ((not justify) space)

			     ;; don't stretch last line of paragraph.
			     ;; hmmm . bug - will overstretch the last line in some case. 
			     ((null? (cdr line-break))
			      base-space)
			     ((null? line-stencils) 0.0)
			     ((null? (cdr line-stencils)) 0.0)
			     (else (/ space-left (1- (length line-stencils))))))

	   (line (stack-stencil-line
		  line-word-space
		  (if (= text-dir RIGHT)
		      (reverse line-stencils)
		      line-stencils))))

	(if (pair? (cdr line-break))
	    (loop (cons line lines)
		  (cdr line-break))

	    (begin
	      (if (= text-dir LEFT)
		  (set! line
			(ly:stencil-translate-axis line
						   (- line-width (interval-end (ly:stencil-extent line X)))
						   X)))
	      (reverse (cons line lines))
	      
	    )))

      ))


(define (wordwrap-markups layout props args justify)
  (let*
      ((prop-line-width (chain-assoc-get 'line-width props #f))
       (line-width (if prop-line-width prop-line-width
		       (ly:output-def-lookup layout 'line-width)))
       (word-space (chain-assoc-get 'word-space props))
       (text-dir (chain-assoc-get 'text-direction props RIGHT)))
    (wordwrap-stencils (remove ly:stencil-empty?
                               (interpret-markup-list layout props args))
                       justify word-space line-width
                       text-dir)))

(define-builtin-markup-command (justify layout props args) (markup-list?)
  "Like wordwrap, but with lines stretched to justify the margins.
Use @code{\\override #'(line-width . @var{X})} to set the line width;
@var{X}@tie{}is the number of staff spaces."
  (stack-lines DOWN 0.0 (chain-assoc-get 'baseline-skip props)
	       (wordwrap-markups layout props args #t)))

(define-builtin-markup-command (wordwrap layout props args) (markup-list?)
  "Simple wordwrap.  Use @code{\\override #'(line-width . @var{X})} to set
the line width, where @var{X} is the number of staff spaces."
  (stack-lines DOWN 0.0 (chain-assoc-get 'baseline-skip props)
	       (wordwrap-markups layout props args #f)))

(define (wordwrap-string layout props justify arg) 
  (let*
      ((baseline-skip (chain-assoc-get 'baseline-skip props))
       (line-width (chain-assoc-get 'line-width props))
       (word-space (chain-assoc-get 'word-space props))
       
       (para-strings (regexp-split
		      (string-regexp-substitute "\r" "\n"
						(string-regexp-substitute "\r\n" "\n" arg))
		      "\n[ \t\n]*\n[ \t\n]*"))
       
       (text-dir (chain-assoc-get 'text-direction props RIGHT)) 
       (list-para-words (map (lambda (str)
			       (regexp-split str "[ \t\n]+"))
			     para-strings))
       (para-lines (map (lambda (words)
			  (let*
			      ((stencils
				(remove
				 ly:stencil-empty? (map 
				      (lambda (x)
					(interpret-markup layout props x))
				      words)))
			       (lines (wordwrap-stencils stencils
							 justify word-space
							 line-width text-dir
							 )))

			    lines))
			
			list-para-words)))

    (stack-lines DOWN 0.0 baseline-skip (apply append para-lines))))


(define-builtin-markup-command (wordwrap-string layout props arg) (string?)
  "Wordwrap a string.  Paragraphs may be separated with double newlines."
  (wordwrap-string layout props  #f arg))
  
(define-builtin-markup-command (justify-string layout props arg) (string?)
  "Justify a string.  Paragraphs may be separated with double newlines"
  (wordwrap-string layout props #t arg))


(define-builtin-markup-command (wordwrap-field layout props symbol) (symbol?)
  "Wordwrap the data which has been assigned to @var{symbol}."
  (let* ((m (chain-assoc-get symbol props)))
    (if (string? m)
     (interpret-markup layout props
      (list wordwrap-string-markup m))
     (ly:make-stencil '()  '(1 . -1) '(1 . -1)))))

(define-builtin-markup-command (justify-field layout props symbol) (symbol?)
  "Justify the data which has been assigned to @var{symbol}."
  (let* ((m (chain-assoc-get symbol props)))
    (if (string? m)
     (interpret-markup layout props
      (list justify-string-markup m))
     (ly:make-stencil '()  '(1 . -1) '(1 . -1)))))



(define-builtin-markup-command (combine layout props m1 m2) (markup? markup?)
  "Print two markups on top of each other."
  (let* ((s1 (interpret-markup layout props m1))
	 (s2 (interpret-markup layout props m2)))
    (ly:stencil-add s1 s2)))

;;
;; TODO: should extract baseline-skip from each argument somehow..
;; 
(define-builtin-markup-command (column layout props args) (markup-list?)
  "Stack the markups in @var{args} vertically.  The property
@code{baseline-skip} determines the space between each markup in @var{args}."

  (let*
      ((arg-stencils (interpret-markup-list layout props args))
       (skip (chain-assoc-get 'baseline-skip props)))

    
    (stack-lines
     -1 0.0 skip
     (remove ly:stencil-empty? arg-stencils))))


(define-builtin-markup-command (dir-column layout props args) (markup-list?)
  "Make a column of args, going up or down, depending on the setting
of the @code{#'direction} layout property."
  (let* ((dir (chain-assoc-get 'direction props)))
    (stack-lines
     (if (number? dir) dir -1)
     0.0
     (chain-assoc-get 'baseline-skip props)
     (interpret-markup-list layout props args))))

(define-builtin-markup-command (center-align layout props args) (markup-list?)
  "Put @code{args} in a centered column."
  (let* ((mols (interpret-markup-list layout props args))
         (cmols (map (lambda (x) (ly:stencil-aligned-to x X CENTER)) mols)))
    
    (stack-lines -1 0.0 (chain-assoc-get 'baseline-skip props) cmols)))

(define-builtin-markup-command (vcenter layout props arg) (markup?)
  "Align @code{arg} to its Y@tie{}center."
  (let* ((mol (interpret-markup layout props arg)))
    (ly:stencil-aligned-to mol Y CENTER)))

(define-builtin-markup-command (hcenter layout props arg) (markup?)
  "Align @code{arg} to its X@tie{}center."
  (let* ((mol (interpret-markup layout props arg)))
    (ly:stencil-aligned-to mol X CENTER)))

(define-builtin-markup-command (right-align layout props arg) (markup?)
  "Align @var{arg} on its right edge."
  (let* ((m (interpret-markup layout props arg)))
    (ly:stencil-aligned-to m X RIGHT)))

(define-builtin-markup-command (left-align layout props arg) (markup?)
  "Align @var{arg} on its left edge."
  (let* ((m (interpret-markup layout props arg)))
    (ly:stencil-aligned-to m X LEFT)))

(define-builtin-markup-command (general-align layout props axis dir arg)  (integer? number? markup?)
  "Align @var{arg} in @var{axis} direction to the @var{dir} side."
  (let* ((m (interpret-markup layout props arg)))
    (ly:stencil-aligned-to m axis dir)))

(define-builtin-markup-command (halign layout props dir arg) (number? markup?)
  "Set horizontal alignment.  If @var{dir} is @code{-1}, then it is
left-aligned, while @code{+1} is right.  Values inbetween interpolate
alignment accordingly."
  (let* ((m (interpret-markup layout props arg)))
    (ly:stencil-aligned-to m X dir)))



(define-builtin-markup-command (with-dimensions layout props x y arg) (number-pair? number-pair? markup?)
  "Set the dimensions of @var{arg} to @var{x} and@tie{}@var{y}."
  
  (let* ((m (interpret-markup layout props arg)))
    (ly:make-stencil (ly:stencil-expr m) x y)))


(define-builtin-markup-command (pad-around layout props amount arg) (number? markup?)
  "Add padding @var{amount} all around @var{arg}."
  
  (let*
      ((m (interpret-markup layout props arg))
       (x (ly:stencil-extent m X))
       (y (ly:stencil-extent m Y)))
    
       
    (ly:make-stencil (ly:stencil-expr m)
		     (interval-widen x amount)
		     (interval-widen y amount))
   ))


(define-builtin-markup-command (pad-x layout props amount arg) (number? markup?)
  "Add padding @var{amount} around @var{arg} in the X@tie{}direction."
  (let*
      ((m (interpret-markup layout props arg))
       (x (ly:stencil-extent m X))
       (y (ly:stencil-extent m Y)))
    
       
    (ly:make-stencil (ly:stencil-expr m)
		     (interval-widen x amount)
		     y)
   ))


(define-builtin-markup-command (put-adjacent layout props arg1 axis dir arg2) (markup? integer? ly:dir?  markup?)
  "Put @var{arg2} next to @var{arg1}, without moving @var{arg1}."
  (let* ((m1 (interpret-markup layout props arg1))
	 (m2 (interpret-markup layout props arg2)))

    (ly:stencil-combine-at-edge m1 axis dir m2 0.0)
  ))

(define-builtin-markup-command (transparent layout props arg) (markup?)
  "Make the argument transparent."
  (let*
      ((m (interpret-markup layout props arg))
       (x (ly:stencil-extent m X))
       (y (ly:stencil-extent m Y)))
    

    
    (ly:make-stencil ""
		     x y)))


(define-builtin-markup-command (pad-to-box layout props x-ext y-ext arg)
  (number-pair? number-pair? markup?)
  "Make @var{arg} take at least @var{x-ext}, @var{y-ext} space."

  (let*
      ((m (interpret-markup layout props arg))
       (x (ly:stencil-extent m X))
       (y (ly:stencil-extent m Y)))

    (ly:make-stencil (ly:stencil-expr m)
		     (interval-union x-ext x)
		     (interval-union y-ext y))))


(define-builtin-markup-command (hcenter-in layout props length arg)
  (number? markup?)
  "Center @var{arg} horizontally within a box of extending
@var{length}/2 to the left and right."

  (interpret-markup layout props
		    (make-pad-to-box-markup
		     (cons (/ length -2) (/ length 2))
		     '(0 . 0)
		     (make-hcenter-markup arg))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; property
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-builtin-markup-command (fromproperty layout props symbol) (symbol?)
  "Read the @var{symbol} from property settings, and produce a stencil
from the markup contained within.  If @var{symbol} is not defined, it
returns an empty markup."
  (let* ((m (chain-assoc-get symbol props)))
    (if (markup? m)
	(interpret-markup layout props m)
	(ly:make-stencil '()  '(1 . -1) '(1 . -1)))))


(define-builtin-markup-command (on-the-fly layout props procedure arg) (symbol? markup?)
  "Apply the @var{procedure} markup command to @var{arg}.
@var{procedure} should take a single argument."
  (let* ((anonymous-with-signature (lambda (layout props arg) (procedure layout props arg))))
    (set-object-property! anonymous-with-signature
			  'markup-signature
			  (list markup?))
    (interpret-markup layout props (list anonymous-with-signature arg))))



(define-builtin-markup-command (override layout props new-prop arg) (pair? markup?)
  "Add the first argument in to the property list.  Properties may be
any sort of property supported by @internalsref{font-interface} and
@internalsref{text-interface}, for example

@example
\\override #'(font-family . married) \"bla\"
@end example"
  (interpret-markup layout (cons (list new-prop) props) arg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-builtin-markup-command (verbatim-file layout props name) (string?)
  "Read the contents of a file, and include it verbatim."

  (interpret-markup
   layout props
   (if  (ly:get-option 'safe)
	"verbatim-file disabled in safe mode"
	(let*
	    ((str (ly:gulp-file name))
	     (lines (string-split str #\nl)))

	  (make-typewriter-markup
	   (make-column-markup lines)))
	)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fonts.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-builtin-markup-command (bigger layout props arg) (markup?)
  "Increase the font size relative to current setting."
  (interpret-markup layout props
   `(,fontsize-markup 1 ,arg)))

(define-builtin-markup-command (smaller layout props arg) (markup?)
  "Decrease the font size relative to current setting."
  (interpret-markup layout props
   `(,fontsize-markup -1 ,arg)))

(define-builtin-markup-command larger (markup?) bigger-markup)

(define-builtin-markup-command (finger layout props arg) (markup?)
  "Set the argument as small numbers."
  (interpret-markup layout
                    (cons '((font-size . -5) (font-encoding . fetaNumber)) props)
                    arg))


(define-builtin-markup-command (fontsize layout props increment arg) (number? markup?)
  "Add @var{increment} to the font-size.  Adjust baseline skip accordingly."

  (let* ((fs (chain-assoc-get 'font-size props 0))
	 (bs (chain-assoc-get 'baseline-skip props 2)) 
         (entries (list
		   (cons 'baseline-skip (* bs (magstep increment)))
		   (cons 'font-size (+ fs increment )))))

    (interpret-markup layout (cons entries props) arg)))

(define-builtin-markup-command (magnify layout props sz arg) (number? markup?)
  "Set the font magnification for its argument.  In the following
example, the middle@tie{}A is 10% larger:

@example
A \\magnify #1.1 @{ A @} A
@end example

Note: Magnification only works if a font name is explicitly selected.
Use @code{\\fontsize} otherwise."
  (interpret-markup
   layout 
   (prepend-alist-chain 'font-size (magnification->font-size sz) props)
   arg))

(define-builtin-markup-command (bold layout props arg) (markup?)
  "Switch to bold font-series."
  (interpret-markup layout (prepend-alist-chain 'font-series 'bold props) arg))

(define-builtin-markup-command (sans layout props arg) (markup?)
  "Switch to the sans serif family."
  (interpret-markup layout (prepend-alist-chain 'font-family 'sans props) arg))

(define-builtin-markup-command (number layout props arg) (markup?)
  "Set font family to @code{number}, which yields the font used for
time signatures and fingerings.  This font only contains numbers and
some punctuation.  It doesn't have any letters."
  (interpret-markup layout (prepend-alist-chain 'font-encoding 'fetaNumber props) arg))

(define-builtin-markup-command (roman layout props arg) (markup?)
  "Set font family to @code{roman}."
  (interpret-markup layout (prepend-alist-chain 'font-family 'roman props) arg))

(define-builtin-markup-command (huge layout props arg) (markup?)
  "Set font size to +2."
  (interpret-markup layout (prepend-alist-chain 'font-size 2 props) arg))

(define-builtin-markup-command (large layout props arg) (markup?)
  "Set font size to +1."
  (interpret-markup layout (prepend-alist-chain 'font-size 1 props) arg))

(define-builtin-markup-command (normalsize layout props arg) (markup?)
  "Set font size to default."
  (interpret-markup layout (prepend-alist-chain 'font-size 0 props) arg))

(define-builtin-markup-command (small layout props arg) (markup?)
  "Set font size to -1."
  (interpret-markup layout (prepend-alist-chain 'font-size -1 props) arg))

(define-builtin-markup-command (tiny layout props arg) (markup?)
  "Set font size to -2."
  (interpret-markup layout (prepend-alist-chain 'font-size -2 props) arg))

(define-builtin-markup-command (teeny layout props arg) (markup?)
  "Set font size to -3."
  (interpret-markup layout (prepend-alist-chain 'font-size -3 props) arg))

(define-builtin-markup-command (fontCaps layout props arg) (markup?)
  "Set @code{font-shape} to @code{caps}."
  (interpret-markup layout (prepend-alist-chain 'font-shape 'caps props) arg))

;; Poor man's caps
(define-builtin-markup-command (smallCaps layout props text) (markup?)
  "Turn @code{text}, which should be a string, to small caps.
@example
\\markup \\smallCaps \"Text between double quotes\"
@end example"
  (define (make-small-caps-markup chars)
    (cond ((null? chars)
	   (markup))
	  ((char-whitespace? (car chars))
	   (markup #:fontsize -2 #:simple (string-upcase (list->string (cdr chars)))))
	  (else
	   (markup #:hspace -1
		   #:fontsize -2 #:simple (string-upcase (list->string chars))))))
  (define (make-not-small-caps-markup chars)
    (cond ((null? chars)
	   (markup))
	  ((char-whitespace? (car chars))
	   (markup #:simple (list->string (cdr chars))))
	  (else
	   (markup #:hspace -1
		   #:simple (list->string chars)))))
  (define (small-caps-aux done-markups current-chars rest-chars small? after-space?)
    (cond ((null? rest-chars)
	   ;; the end of the string: build the markup
	   (make-line-markup (reverse! (cons ((if small?
						  make-small-caps-markup
						  make-not-small-caps-markup)
					      (reverse! current-chars))
					     done-markups))))
	  ((char-whitespace? (car rest-chars))
	   ;; a space char.
	   (small-caps-aux done-markups current-chars (cdr rest-chars) small? #t))
	  ((or (and small? (char-lower-case? (car rest-chars)))
	       (and (not small?) (not (char-lower-case? (car rest-chars)))))
	   ;; same case
	   ;; add the char to the current char list
	   (small-caps-aux done-markups
			   (cons (car rest-chars)
				 (if after-space? 
				     (cons #\space current-chars)
				     current-chars))
			   (cdr rest-chars) 
			   small?
			   #f))
	  (else
	   ;; case change
	   ;; make a markup with current chars, and start a new list with new char
	   (small-caps-aux (cons ((if small?
				      make-small-caps-markup
				      make-not-small-caps-markup)
				  (reverse! current-chars))
				 done-markups)
			   (if after-space?
			       (list (car rest-chars) #\space)
			       (list (car rest-chars)))
			   (cdr rest-chars)
			   (not small?)
			   #f))))
  (interpret-markup layout props (small-caps-aux (list) 
						 (list) 
						 (cons #\space (string->list text))
						 #f
						 #f)))

(define-builtin-markup-command (caps layout props arg) (markup?)
  "Emit @var{arg} as small caps."
  (interpret-markup layout props (make-smallCaps-markup arg)))

(define-builtin-markup-command (dynamic layout props arg) (markup?)
  "Use the dynamic font.  This font only contains @b{s}, @b{f}, @b{m},
@b{z}, @b{p}, and @b{r}.  When producing phrases, like
@q{pi@`{u}@tie{}@b{f}}, the normal words (like @q{pi@`{u}}) should be
done in a different font.  The recommended font for this is bold and italic."
  (interpret-markup
   layout (prepend-alist-chain 'font-encoding 'fetaDynamic props) arg))

(define-builtin-markup-command (text layout props arg) (markup?)
  "Use a text font instead of music symbol or music alphabet font."  

  ;; ugh - latin1
  (interpret-markup layout (prepend-alist-chain 'font-encoding 'latin1 props)
		    arg))


(define-builtin-markup-command (italic layout props arg) (markup?)
  "Use italic @code{font-shape} for @var{arg}."
  (interpret-markup layout (prepend-alist-chain 'font-shape 'italic props) arg))

(define-builtin-markup-command (typewriter layout props arg) (markup?)
  "Use @code{font-family} typewriter for @var{arg}."
  (interpret-markup
   layout (prepend-alist-chain 'font-family 'typewriter props) arg))

(define-builtin-markup-command (upright layout props arg) (markup?)
  "Set font shape to @code{upright}.  This is the opposite of @code{italic}."
  (interpret-markup
   layout (prepend-alist-chain 'font-shape 'upright props) arg))

(define-builtin-markup-command (medium layout props arg) (markup?)
  "Switch to medium font series (in contrast to bold)."
  (interpret-markup layout (prepend-alist-chain 'font-series 'medium props)
		    arg))

(define-builtin-markup-command (normal-text layout props arg) (markup?)
  "Set all font related properties (except the size) to get the default
normal text font, no matter what font was used earlier."
  ;; ugh - latin1
  (interpret-markup layout
                    (cons '((font-family . roman) (font-shape . upright)
			    (font-series . medium) (font-encoding . latin1))
			  props)
                    arg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; symbols.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-builtin-markup-command (doublesharp layout props) ()
  "Draw a double sharp symbol."

  (interpret-markup layout props (markup #:musicglyph (assoc-get 1 standard-alteration-glyph-name-alist ""))))

(define-builtin-markup-command (sesquisharp layout props) ()
  "Draw a 3/2 sharp symbol."
  (interpret-markup layout props (markup #:musicglyph (assoc-get 3/4 standard-alteration-glyph-name-alist ""))))
					 

(define-builtin-markup-command (sharp layout props) ()
  "Draw a sharp symbol."
  (interpret-markup layout props (markup #:musicglyph (assoc-get 1/2 standard-alteration-glyph-name-alist ""))))

(define-builtin-markup-command (semisharp layout props) ()
  "Draw a semi sharp symbol."
  (interpret-markup layout props (markup #:musicglyph (assoc-get 1/4 standard-alteration-glyph-name-alist ""))))

(define-builtin-markup-command (natural layout props) ()
  "Draw a natural symbol."
  (interpret-markup layout props (markup #:musicglyph (assoc-get 0 standard-alteration-glyph-name-alist ""))))

(define-builtin-markup-command (semiflat layout props) ()
  "Draw a semiflat."
  (interpret-markup layout props (markup #:musicglyph (assoc-get -1/4 standard-alteration-glyph-name-alist ""))))

(define-builtin-markup-command (flat layout props) ()
  "Draw a flat symbol."
  (interpret-markup layout props (markup #:musicglyph (assoc-get -1/2 standard-alteration-glyph-name-alist ""))))

(define-builtin-markup-command (sesquiflat layout props) ()
  "Draw a 3/2 flat symbol."
  (interpret-markup layout props (markup #:musicglyph (assoc-get -3/4 standard-alteration-glyph-name-alist ""))))

(define-builtin-markup-command (doubleflat layout props) ()
  "Draw a double flat symbol."
  (interpret-markup layout props (markup #:musicglyph (assoc-get -1 standard-alteration-glyph-name-alist ""))))

(define-builtin-markup-command (with-color layout props color arg) (color? markup?)
  "Draw @var{arg} in color specified by @var{color}."

  (let* ((stil (interpret-markup layout props arg)))

    (ly:make-stencil (list 'color color (ly:stencil-expr stil))
		     (ly:stencil-extent stil X)
		     (ly:stencil-extent stil Y))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; glyphs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-builtin-markup-command (arrow-head layout props axis direction filled)
  (integer? ly:dir? boolean?)
  "Produce an arrow head in specified direction and axis.
Use the filled head if @var{filled} is specified."
  (let*
      ((name (format "arrowheads.~a.~a~a"
		     (if filled
			 "close"
			 "open")
		     axis
		     direction)))
    (ly:font-get-glyph
     (ly:paper-get-font layout (cons '((font-encoding . fetaMusic))
				     props))
     name)))

(define-builtin-markup-command (musicglyph layout props glyph-name) (string?)
  "@var{glyph0name} is converted to a musical symbol; for example,
@code{\\musicglyph #\"accidentals.natural\"} selects the natural sign from
the music font.  See @usermanref{The Feta font} for a complete listing of
the possible glyphs."
  (ly:font-get-glyph
   (ly:paper-get-font layout (cons '((font-encoding . fetaMusic))
				   props))
   glyph-name))

(define-builtin-markup-command (lookup layout props glyph-name) (string?)
  "Lookup a glyph by name."
  (ly:font-get-glyph (ly:paper-get-font layout props)
		     glyph-name))

(define-builtin-markup-command (char layout props num) (integer?)
  "Produce a single character.  For example, @code{\\char #65} produces the 
letter @q{A}."

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

(define-builtin-markup-command (markletter layout props num) (integer?)
  "Make a markup letter for @var{num}.  The letters start with A to@tie{}Z
(skipping letter@tie{}I), and continue with double letters."
  (ly:text-interface::interpret-markup layout props
    (number->markletter-string number->mark-letter-vector num)))

(define-builtin-markup-command (markalphabet layout props num) (integer?)
   "Make a markup letter for @var{num}.  The letters start with A to@tie{}Z
and continue with double letters."
   (ly:text-interface::interpret-markup layout props
     (number->markletter-string number->mark-alphabet-vector num)))



(define-builtin-markup-command (slashed-digit layout props num) (integer?)
  "A feta number, with slash.  This is for use in the context of
figured bass notation."
  (let*
      ((mag (magstep (chain-assoc-get 'font-size props 0)))
       (thickness
	(* mag
	   (chain-assoc-get 'thickness props 0.16)))
       (dy (* mag 0.15))
       (number-stencil (interpret-markup layout
					 (prepend-alist-chain 'font-encoding 'fetaNumber props)
					 (number->string num)))
       (num-x (interval-widen (ly:stencil-extent number-stencil X)
			      (* mag 0.2)))
       (num-y (ly:stencil-extent number-stencil Y))
       (is-sane (and (interval-sane? num-x) (interval-sane? num-y)))
       
       (slash-stencil
	(if is-sane
	    (ly:make-stencil
	     `(draw-line
	       ,thickness
	       ,(car num-x) ,(- (interval-center num-y) dy)
	       ,(cdr num-x) ,(+ (interval-center num-y) dy))
	     num-x num-y)
	    #f)))

    (set! slash-stencil
	  (cond
	   ((not (ly:stencil? slash-stencil)) #f)
	   ((= num 5) (ly:stencil-translate slash-stencil
					    ;;(cons (* mag -0.05) (* mag 0.42))
					    (cons (* mag -0.00) (* mag -0.07))

					    ))
	   ((= num 7) (ly:stencil-translate slash-stencil
					    ;;(cons (* mag -0.05) (* mag 0.42))
					    (cons (* mag -0.00) (* mag -0.15))

					    ))
	   
	   (else slash-stencil)))

    (if slash-stencil
	(set! number-stencil
	      (ly:stencil-add number-stencil slash-stencil))
	
	(ly:warning "invalid number for slashed digit ~a" num))


    number-stencil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the note command.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; TODO: better syntax.

(define-builtin-markup-command (note-by-number layout props log dot-count dir) (number? number? number?)
  "Construct a note symbol, with stem.  By using fractional values for
@var{dir}, you can obtain longer or shorter stems."

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
	 (size-factor (magstep (chain-assoc-get 'font-size props 0)))
	 (style (chain-assoc-get 'style props '()))
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
                       (cons (+ (car attach-off) (/ stem-thickness 2)) stemy)))))

    (if (and dots flaggl (> dir 0))
	(set! dots (ly:stencil-translate-axis dots 0.35 X)))
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

(define-builtin-markup-command (note layout props duration dir) (string? number?)
  "This produces a note with a stem pointing in @var{dir} direction, with
the @var{duration} for the note head type and augmentation dots.  For
example, @code{\\note #\"4.\" #-0.75} creates a dotted quarter note, with
a shortened down stem."
  (let ((parsed (parse-simple-duration duration)))
    (note-by-number-markup layout props (car parsed) (cadr parsed) dir)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; translating.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-builtin-markup-command (lower layout props amount arg) (number? markup?)
  "Lower @var{arg} by the distance @var{amount}.
A negative @var{amount} indicates raising; see also @code{\\raise}."
  (ly:stencil-translate-axis (interpret-markup layout props arg)
			     (- amount) Y))


(define-builtin-markup-command (translate-scaled layout props offset arg) (number-pair? markup?)
  "Translate @var{arg} by @var{offset}, scaling the offset by the
@code{font-size}."
  (let*
      ((factor (magstep (chain-assoc-get 'font-size props 0)))
       (scaled (cons (* factor (car offset))
		     (* factor (cdr offset)))))
    
  (ly:stencil-translate (interpret-markup layout props arg)
			scaled)))

(define-builtin-markup-command (raise layout props amount arg) (number? markup?)
  "Raise @var{arg} by the distance @var{amount}.
A negative @var{amount} indicates lowering, see also @code{\\lower}.
@c
@lilypond[verbatim,fragment,relative=1]
c1^\\markup { C \\small \\raise #1.0 \\bold { \"9/7+\" } }
@end lilypond
The argument to @code{\\raise} is the vertical displacement amount,
measured in (global) staff spaces.  @code{\\raise} and @code{\\super}
raise objects in relation to their surrounding markups.

If the text object itself is positioned above or below the staff, then
@code{\\raise} cannot be used to move it, since the mechanism that
positions it next to the staff cancels any shift made with
@code{\\raise}.  For vertical positioning, use the @code{padding}
and/or @code{extra-offset} properties."
  (ly:stencil-translate-axis (interpret-markup layout props arg) amount Y))

(define-builtin-markup-command (fraction layout props arg1 arg2) (markup? markup?)
  "Make a fraction of two markups."
  (let* ((m1 (interpret-markup layout props arg1))
         (m2 (interpret-markup layout props arg2))
         (factor (magstep (chain-assoc-get 'font-size props 0)))
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

(define-builtin-markup-command (normal-size-super layout props arg) (markup?)
  "Set @var{arg} in superscript with a normal font size."
  (ly:stencil-translate-axis
   (interpret-markup layout props arg)
   (* 0.5 (chain-assoc-get 'baseline-skip props)) Y))

(define-builtin-markup-command (super layout props arg) (markup?)
  "
@cindex raising text
@cindex lowering text
@cindex moving text
@cindex translating text

@cindex @code{\\super}

Raising and lowering texts can be done with @code{\\super} and
@code{\\sub}:
@c
@lilypond[verbatim,fragment,relative=1]
c1^\\markup { E \"=\" \\concat { \"mc\" \\super \"2\" } }
@end lilypond"
  (ly:stencil-translate-axis
   (interpret-markup
    layout
    (cons `((font-size . ,(- (chain-assoc-get 'font-size props 0) 3))) props)
    arg)
   (* 0.5 (chain-assoc-get 'baseline-skip props))
   Y))

(define-builtin-markup-command (translate layout props offset arg) (number-pair? markup?)
  "This translates an object.  Its first argument is a cons of numbers.

@example
A \\translate #(cons 2 -3) @{ B C @} D
@end example

This moves @q{B C} 2@tie{}spaces to the right, and 3 down, relative to its
surroundings.  This command cannot be used to move isolated scripts
vertically, for the same reason that @code{\\raise} cannot be used for
that."
  (ly:stencil-translate (interpret-markup  layout props arg)
			offset))

(define-builtin-markup-command (sub layout props arg) (markup?)
  "Set @var{arg} in subscript."
  (ly:stencil-translate-axis
   (interpret-markup
    layout
    (cons `((font-size . ,(- (chain-assoc-get 'font-size props 0) 3))) props)
    arg)
   (* -0.5 (chain-assoc-get 'baseline-skip props))
   Y))

(define-builtin-markup-command (normal-size-sub layout props arg) (markup?)
  "Set @var{arg} in subscript, in a normal font size."
  (ly:stencil-translate-axis
   (interpret-markup layout props arg)
   (* -0.5 (chain-assoc-get 'baseline-skip props))
   Y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; brackets.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-builtin-markup-command (hbracket layout props arg) (markup?)
  "Draw horizontal brackets around @var{arg}."  
  (let ((th 0.1) ;; todo: take from GROB.
        (m (interpret-markup layout props arg)))
    (bracketify-stencil m X th (* 2.5 th) th)))

(define-builtin-markup-command (bracket layout props arg) (markup?)
  "Draw vertical brackets around @var{arg}."  
  (let ((th 0.1) ;; todo: take from GROB.
        (m (interpret-markup layout props arg)))
    (bracketify-stencil m Y th (* 2.5 th) th)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Delayed markup evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-builtin-markup-command (page-ref layout props label gauge default)
  (symbol? markup? markup?)
  "Reference to a page number. @var{label} is the label set on the referenced
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

(define-public (space-lines baseline-skip lines)
  (map (lambda (line)
	 (stack-lines DOWN 0.0 (/ baseline-skip 2.0)
		      (list (ly:make-stencil "" (cons 0 0) (cons 0 0))
			    line
			    (ly:make-stencil "" (cons 0 0) (cons 0 0)))))
       lines))

(define-builtin-markup-list-command (justified-lines layout props args) (markup-list?)
  "Like @code{\\justify}, but return a list of lines instead of a single markup.
Use @code{\\override #'(line-width . @var{X})} to set the line width;
@var{X}@tie{}is the number of staff spaces."
  (space-lines (chain-assoc-get 'baseline-skip props)
	       (wordwrap-markups layout props args #t)))

(define-builtin-markup-list-command (wordwrap-lines layout props args) (markup-list?)
  "Like @code{\\wordwrap}, but return a list of lines instead of a single markup.
Use @code{\\override #'(line-width . @var{X})} to set the line width,
where @var{X} is the number of staff spaces."
  (space-lines (chain-assoc-get 'baseline-skip props)
	       (wordwrap-markups layout props args #f)))

(define-builtin-markup-list-command (column-lines layout props args) (markup-list?)
  "Like @code{\\column}, but return a list of lines instead of a single markup.
@code{baseline-skip} determines the space between each markup in @var{args}."
  (space-lines (chain-assoc-get 'baseline-skip props)
	       (interpret-markup-list layout props args)))

(define-builtin-markup-list-command (override-lines layout props new-prop args)
  (pair? markup-list?)
  "Like @code{\\override}, for markup lists."
  (interpret-markup-list layout (cons (list new-prop) props) args))
