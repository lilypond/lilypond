;;;; fret-diagrams.scm -- 
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 2004--2007 Carl D. Sorensen <c_sorensen@byu.edu>

(define (fret-parse-marking-list marking-list fret-count)
   (let* ((fret-range (list 1 fret-count))
          (barre-list '())
          (dot-list '())
          (xo-list '())
          (output-alist '()))
      (let parse-item ((mylist marking-list))
          (if (not (null? mylist))
              (let* ((my-item (car mylist)) (my-code (car my-item)))
                 (cond
                     ((or (eq? my-code 'open)(eq? my-code 'mute))
                        (set! xo-list (cons* my-item xo-list)))
                     ((eq? my-code 'barre)
                        (set! barre-list (cons* (cdr my-item) barre-list)))
                     ((eq? my-code 'place-fret)
                        (set! dot-list (cons* (cdr my-item) dot-list))))
                 (parse-item (cdr mylist)))))
      
      ;; calculate fret-range
               (let ((maxfret 0) (minfret 99))
                    (let updatemax ((fret-list dot-list))
                        (if (null?  fret-list)
                           '()
                           (let ((fretval (second (car fret-list))))
                               (if (> fretval maxfret) (set! maxfret fretval))
                               (if (< fretval minfret) (set! minfret fretval))
                               (updatemax (cdr fret-list)))))
                    (if (> maxfret fret-count)
                        (set! fret-range (list minfret
                             (let ((upfret (- (+ minfret fret-count) 1)))
                                  (if (> maxfret upfret) maxfret upfret)))))
                    ; subtract fret from dots
                    (set! dot-list (subtract-base-fret (- (car fret-range) 1) dot-list)))
               (acons 'fret-range fret-range
               (acons 'barre-list barre-list
               (acons 'dot-list dot-list
               (acons 'xo-list xo-list '()))))))



(define (subtract-base-fret base-fret dot-list)
"Subtract @var{base-fret} from every fret in @var{dot-list}"  
  (if (null? dot-list)
      '()
      (let ((this-list (car dot-list)))
      (cons* (list (car this-list) (- (second this-list) base-fret) (if (null? (cddr this-list))
                                                                    '()
                                                                    (third this-list)))
             (subtract-base-fret base-fret (cdr dot-list))))))

(define (sans-serif-stencil layout props mag  text)
"create a stencil in sans-serif font based on @var{layout} and @var{props}
with magnification @varr{mag} of the string @var{text}."
  (let* ((my-props (prepend-alist-chain  'font-size (stepmag mag)
                   (prepend-alist-chain  'font-family 'sans props))))
        (interpret-markup layout my-props text)))


(define (draw-strings string-count fret-range th size)
"Draw the strings (vertical lines) for a fret diagram with
@var{string-count} strings and frets as indicated in @var{fret-range}.
Line thickness is given by @var{th}, fret & string spacing by
@var{size}. "
  (let* ((fret-count (+ (- (cadr fret-range) (car fret-range)) 1))
         (sl (* (+ fret-count 1) size))
         (sth (* size th))
         (half-thickness (* sth 0.5))
         (gap (- size sth))
         (string-stencil (ly:make-stencil (list 'draw-line sth 0 0 0 sl)
                         (cons (- half-thickness) half-thickness)
                         (cons (- half-thickness) (+ sl half-thickness)))))
    (if (= string-count 1)
         string-stencil
        (ly:stencil-combine-at-edge
         (draw-strings (- string-count 1) fret-range th size) X RIGHT
         string-stencil
         gap 0))))

(define (draw-fret-lines fret-count string-count th size)
 "Draw @var{fret-count} frets (horizontal lines) for a fret diagram
with @var{string-count} strings.  Line thickness is given by @var{th},
fret & string spacing by @var{size}. "
   (let* ((fret-length (* (- string-count 1) size))
          (sth (* size th))
          (half-thickness (* sth 0.5))
          (gap (- size sth))
          (fret-line (ly:make-stencil (list 'draw-line sth half-thickness size (- fret-length half-thickness) size)
                          (cons 0 fret-length)
                          (cons (- size half-thickness) (+  size half-thickness)))))
       (if (= fret-count 1)
         fret-line
         (ly:stencil-combine-at-edge fret-line Y UP
          (draw-fret-lines (- fret-count 1) string-count th size)
          gap 0))))
          
(define (draw-thick-top-fret props string-count th size)
 "Draw a thick top fret for a fret diagram whose base fret is not 1."
   (let* ((sth (* th size))
;          (top-fret-thick (* sth (chain-assoc-get 'top-fret-thickness props 3.0)))
          (top-fret-thick (* sth 3.0))
;          (top-half-thick (* top-fret-thick 0.5))
          (half-thick (* sth 0.5))
          (x1 half-thick)
          (x2 (+ half-thick (* size (- string-count 1))))
          (y1 (- half-thick))
          (y2 (+ top-fret-thick half-thick))
          (x-extent (cons (- x1) x2))
          (y-extent (cons 0 y2)))
          (ly:make-stencil (list 'round-filled-box x1 x2 y1 y2 sth)
                            x-extent y-extent)))           
 
 
(define (draw-frets layout props fret-range string-count th size)
 "Draw the frets (horizontal lines) for a fret diagram with
@var{string-count} strings and frets as indicated in @var{fret-range}.
Line thickness is given by @var{th}, fret & string spacing by
@var{size}. "
  (let* ((fret-count (+ (- (cadr fret-range) (car fret-range)) 1))
         (fret-length (* (- string-count 1) size))
         (half-thickness (* th 0.5))
         (base-fret (car fret-range)))
       (ly:stencil-combine-at-edge
          (draw-fret-lines fret-count string-count th size) Y UP
             (if (= base-fret 1)
                 (draw-thick-top-fret props string-count th size)
                 (draw-fret-lines 1 string-count th size)) 
                 (- size th) 0))) 
                 

(define (draw-dots layout props string-count fret-range size finger-code 
                    dot-position dot-radius dot-thickness dot-list)
  "Make dots for fret diagram."

  (let* ((scale-dot-radius (* size dot-radius))
         (scale-dot-thick (* size dot-thickness))
         (dot-color (chain-assoc-get 'dot-color props 'black))
;         (finger-xoffset (chain-assoc-get 'finger-xoffset props -0.25))
;         (finger-yoffset (chain-assoc-get 'finger-yoffset props (- size)))
         (finger-xoffset -0.25)
         (finger-yoffset (- (* size 0.5)))
;         (dot-label-font-mag (* scale-dot-radius (chain-assoc-get 'dot-label-font-mag props 1.0)))
         (dot-label-font-mag scale-dot-radius)
;         (string-label-font-mag (* size (chain-assoc-get 'label-font-mag props 0.7)))
         (string-label-font-mag (* size 0.6))
         (fret-count (+ (- (cadr fret-range) (car fret-range) 1)))
         (mypair (car dot-list))
         (restlist (cdr dot-list))
         (xpos (* size (- string-count (car mypair))))
;TODO -- figure out what 4 is and get rid of it
;UGH -- 4?
         (ypos (* size (+ 4 (- fret-count (cadr mypair) dot-position ))))
         (extent (cons (- scale-dot-radius) scale-dot-radius))
         (finger (caddr mypair))
         (finger (if (number? finger) (number->string finger) finger))
         (dotstencil  (if (eq? dot-color 'white)
                          (ly:stencil-add 
                              (make-circle-stencil scale-dot-radius scale-dot-thick #t)
                              (ly:stencil-in-color
                                    (make-circle-stencil 
                                        (- scale-dot-radius (* 0.5 scale-dot-thick)) 0  #t)
                                    1 1 1))
                          (make-circle-stencil scale-dot-radius scale-dot-thick #t))) 
         (positioned-dot (begin
                           ;(display dotstencil)
                           (ly:stencil-translate-axis
                             (ly:stencil-translate-axis dotstencil xpos X)
                           ypos Y)))

         (labeled-dot-stencil 
                 (if (or (eq? finger '())(eq? finger-code 'none))
                     positioned-dot
                     (if (eq? finger-code 'in-dot)
                         (let*  ((finger-label (centered-stencil 
                                           (sans-serif-stencil layout props
                                                  dot-label-font-mag finger))))
                         (ly:stencil-translate-axis 
                            (ly:stencil-translate-axis 
			      (ly:stencil-add 
                                dotstencil
                                (if (eq? dot-color 'white)
                                     finger-label
                                     (ly:stencil-in-color finger-label 1 1 1))) 
                               xpos X)
                              ypos Y))
                 (if (eq? finger-code 'below-string) 
                     (ly:stencil-add 
                         positioned-dot
                         (ly:stencil-translate-axis 
                             (ly:stencil-translate-axis 
                                 (centered-stencil (sans-serif-stencil layout props 
                                                        string-label-font-mag finger))
                              xpos  X)
                          (* size finger-yoffset) Y))
                     ;unknown finger-code
                     positioned-dot)))))
    (if (null? restlist) 
        labeled-dot-stencil
        (ly:stencil-add 
            (draw-dots layout props string-count fret-range size finger-code 
                          dot-position dot-radius dot-thickness restlist)
            labeled-dot-stencil))))

(define (draw-xo layout props string-count fret-range size xo-list) 
"Put open and mute string indications on diagram, as contained in @var{xo-list}."
    (let* ((fret-count (+ (- (cadr fret-range) (car fret-range) 1)))
;           (xo-font-mag (* size (chain-assoc-get 'xo-font-magnification props 0.5)))
           (xo-font-mag (* size 0.5))
;           (xo-horizontal-offset (* size (chain-assoc-get 'xo-horizontal-offset props -0.35)))
           (xo-horizontal-offset (* size -0.35))
           (mypair (car xo-list))
           (restlist (cdr xo-list))
           (glyph-string (if (eq? (car mypair) 'mute) "X" "O"))
           (xpos (+ (* (- string-count (cadr mypair)) size) xo-horizontal-offset ))
           (glyph-stencil (ly:stencil-translate-axis 
              (sans-serif-stencil layout props (* size xo-font-mag) glyph-string) xpos X)))
      (if (null? restlist)
          glyph-stencil
          (ly:stencil-add
            (draw-xo layout props string-count fret-range size restlist)
            glyph-stencil))))

(define (make-bezier-sandwich-list left right bottom height thickness)
" Make the argument list for a horizontal bezier sandwich from
@var{left} to @var{right} with a bottom at @var{bottom}, a height of
@var{height}, and a thickness of @var{thickness}."
   (let* ((width (+ (- right left) 1))
          (x1 (+ (* width thickness) left))
          (x2 (- right (* width thickness)))
          (bottom-control-point-height (+ bottom (- height thickness)))
          (top-control-point-height (+ bottom height)))
           ; order of points is: left cp low, right cp low, right end low, left end low
           ;                     right cp high, left cp high, left end high, right end high.
       (list (cons x1 bottom-control-point-height) (cons x2 bottom-control-point-height) (cons right bottom) (cons left bottom)
             (cons x2 top-control-point-height) (cons x1 top-control-point-height) (cons left bottom) (cons right bottom))))

(define (draw-barre layout props string-count fret-range size finger-code dot-position dot-radius barre-list)
   "Create barre indications for a fret diagram"
   (if (not (null? barre-list))
     (let* ((string1 (caar barre-list))
            (string2 (cadar barre-list))
            (fret (caddar barre-list))
            (barre-type (chain-assoc-get 'barre-type props 'curved))
            (scale-dot-radius (* size dot-radius))
            (barre-vertical-offset (chain-assoc-get 'barre-vertical-offset props 0.5))
            ;; 2 is 1 for empty fret at bottom of figure + 1 for interval (top-fret - fret + 1) -- not an arbitrary constant
            (dot-center-y (* size
			     (- (+ 2 (- (cadr fret-range) fret)) dot-position)))
            (bottom (+ dot-center-y (* barre-vertical-offset scale-dot-radius)))
            (left (* size (- string-count string1)))
            (right (* size (- string-count string2)))
;;            (bezier-thick (chain-assoc-get 'bezier-thickness props 0.1))
            (bezier-thick 0.1)
;;            (bezier-height (chain-assoc-get 'bezier-height props 0.5))
            (bezier-height 0.5)
            (bezier-list (make-bezier-sandwich-list left right bottom (* size bezier-height) (* size bezier-thick)))
            (barre-stencil (if (eq? barre-type 'straight)
                              (ly:make-stencil (list 'draw-line (* size dot-radius) left dot-center-y right dot-center-y)
                                               (cons left right)
                                               (cons (- dot-center-y scale-dot-radius) (+ dot-center-y scale-dot-radius))) 
                              (ly:make-stencil (list 'bezier-sandwich `(quote ,bezier-list) (* size bezier-thick))
                                  (cons left right)
                                  (cons bottom (+ bottom (* size bezier-height)))))))
        (if (not (null? (cdr barre-list)))
            (ly:stencil-add barre-stencil
                 (draw-barre layout props string-count fret-range size finger-code 
                      dot-position dot-radius (cdr barre-list)))
            barre-stencil ))))
  
(define (stepmag mag)
"Calculate the font step necessary to get a desired magnification"
(* 6 (/ (log mag) (log 2))))

(define (label-fret layout props string-count fret-range size)
   "Label the base fret on a fret diagram"
   (let* ((base-fret (car fret-range))
;          (label-font-mag (chain-assoc-get 'label-font-mag props 0.7))
          (label-font-mag 0.5)
;          (label-vertical-offset (chain-assoc-get 'fret-label-vertical-offset props -0.2))
          (label-vertical-offset -0.2)
	  (number-type (chain-assoc-get 'number-type props 'roman-lower))
          (fret-count (+ (- (cadr fret-range) (car fret-range)) 1))
           (label-text 
              (cond
	       ((equal?   number-type  'roman-lower) (fancy-format #f "~(~:@r~)" base-fret))
	       ((equal?  number-type 'roman-upper) (fancy-format #f "~:@r" base-fret))
	       ((equal? 'arabic number-type)  (fancy-format #f "~d" base-fret))
	       (else (fancy-format #f  "~(~:@r~)" base-fret)))))
       (ly:stencil-translate-axis 
           (sans-serif-stencil layout props (* size label-font-mag) label-text) 
                       (* size (+ fret-count label-vertical-offset)) Y)))
 
(define-markup-command (fret-diagram-verbose layout props marking-list)
  (list?)
  "Make a fret diagram containing the symbols indicated in @var{marking-list}
  
  For example,
  
@example
   \\markup \\fret-diagram-verbose #'((mute 6) (mute 5) (open 4)
        (place-fret 3 2) (place-fret 2 3) (place-fret 1 2))
@end example 
  
  will produce a standard D chord diagram without fingering indications.
  
Possible elements in @var{marking-list}:
@table @asis
@item (mute string-number)
Place a small 'x' at the top of string @var{string-number}

@item (open string-number)
Place a small 'o' at the top of string @var{string-number}

@item (barre start-string end-string fret-number)
Place a barre indicator (much like a tie) from string @var{start-string}to string @var{end-string} at fret @var{fret-number}

@item (place-fret string-number fret-number finger-value)

Place a fret playing indication on string @var{string-number} at fret
@var{fret-number} with an optional fingering label @var{finger-value}.
By default, the fret playing indicator is a solid dot.  This can be
changed by setting the value of the variable @var{dot-color}.  If the
@var{finger} part of the place-fret element is present,
@var{finger-value} will be displayed according to the setting of the
variable @var{finger-code}.  There is no limit to the number of fret
indications per string.




@end table
"
   (make-fret-diagram layout props marking-list))
   
(define (make-fret-diagram layout props marking-list)
" Make a fret diagram markup"
  (let* (
         ; note:  here we get items from props that are needed in this routine, or that are needed in more than one
         ; of the procedures called from this routine.  If they're only used in one of the sub-procedure, they're 
         ; obtained in that procedure
         
         (size (chain-assoc-get 'size props 1.0)) ; needed for everything
;TODO -- get string-count directly from length of stringTunings; requires FretDiagram engraver, which is not yet available
;TODO -- adjust padding for fret label?  it appears to be too close to dots
         (string-count (chain-assoc-get 'string-count props 6)) ; needed for everything
         (fret-count (chain-assoc-get 'fret-count props 4)) ; needed for everything
         (finger-code (chain-assoc-get 'finger-code props 'none))  ; needed for both draw-dots and draw-barre
         (default-dot-radius (if (eq? finger-code 'in-dot) 0.425 0.25))  ; bigger dots if labeled
         (default-dot-position (if (eq? finger-code 'in-dot) (- 0.95 default-dot-radius) 0.6))  ; move up to make room for bigger if labeled
         (dot-radius (chain-assoc-get 'dot-radius props default-dot-radius))  ; needed for both draw-dots and draw-barre
         (dot-position (chain-assoc-get 'dot-position props default-dot-position)) ; needed for both draw-dots and draw-barre
         (th (* (ly:output-def-lookup layout 'line-thickness)
                (chain-assoc-get 'thickness props 0.5))) ; needed for both draw-frets and draw-strings
                
         (alignment (chain-assoc-get 'align-dir props -0.4)) ; needed only here
;         (xo-padding (* th (chain-assoc-get 'padding props 2))) ; needed only here
         (label-space (* 0.25 size))
         (xo-padding (* th size 5))
         (label-dir (chain-assoc-get 'label-dir props RIGHT))
         (parameters (fret-parse-marking-list marking-list fret-count))
         (dot-list (cdr (assoc 'dot-list parameters)))
         (xo-list (cdr (assoc 'xo-list parameters)))
         (fret-range (cdr (assoc 'fret-range parameters)))
         (barre-list (cdr (assoc 'barre-list parameters)))
         (fret-diagram-stencil (ly:stencil-add
                            (draw-strings string-count fret-range th size)
                            (draw-frets layout props fret-range string-count th size))))
         (if (not (null? barre-list))
             (set! fret-diagram-stencil (ly:stencil-add
                                    (draw-barre layout props string-count fret-range size finger-code  
                                                dot-position dot-radius barre-list)
                                    fret-diagram-stencil)))
         (if (not (null? dot-list))
             (set! fret-diagram-stencil (ly:stencil-add
                                    fret-diagram-stencil
                                    (draw-dots layout props string-count fret-range size finger-code 
                                          dot-position dot-radius th dot-list))))
         (if (not (null? xo-list))
             (set! fret-diagram-stencil (ly:stencil-combine-at-edge
                                    fret-diagram-stencil Y UP
                                    (draw-xo layout props string-count fret-range size xo-list) xo-padding 0)))
         (if (> (car fret-range) 1) 
             (set! fret-diagram-stencil
                   (ly:stencil-combine-at-edge fret-diagram-stencil X label-dir
                                              (label-fret layout props string-count fret-range size) label-space 0)))
         (ly:stencil-aligned-to fret-diagram-stencil X alignment)
	 ))
         
(define-markup-command (fret-diagram layout props definition-string)
  (string?)
  "  
Example

@example
 \\markup \\fret-diagram #\"s:0.75;6-x;5-x;4-o;3-2;2-3;1-2;\"
@end example

for fret spacing 3/4 of staff space, D chord diagram

Syntax rules for @var{definition-string}:
@itemize @minus
      
@item
Diagram items are separated by semicolons.

@item
Possible items:

@itemize @bullet
@item
s:number -- set the fret spacing of the diagram (in staff spaces). Default 1

@item
t:number -- set the line thickness (in staff spaces).  Default 0.05

@item
h:number -- set the height of the diagram in frets.  Default 4

@item
w:number -- set the width of the diagram in strings.  Default 6

@item
f:number -- set fingering label type (0 = none, 1 = in circle on string, 2 = below string)  Default 0

@item
d:number -- set radius of dot, in terms of fret spacing.  Default 0.25

@item
p:number -- set the position of the dot in the fret space. 0.5 is centered; 1 is on lower fret bar,
0 is on upper fret bar.  Default 0.6 

@item
c:string1-string2-fret -- include a barre mark from string1 to string2 on fret
      
@item
string-fret -- place a dot on string at fret.  If fret is o, string is identified
as open.  If fret is x, string is identified as muted.

@item
string-fret-fingering -- place a dot on string at fret, and label with fingering as 
defined by f: code.

@end itemize

@item
Note:  There is no limit to the number of fret indications per string.
@end itemize
    
"
       (let ((definition-list (fret-parse-definition-string props definition-string)))
       (make-fret-diagram layout (car definition-list) (cdr definition-list))))

(define (fret-parse-definition-string props definition-string)
 "parse a fret diagram string and return a pair containing:
  props, modified as necessary by the definition-string
  a fret-indication list with the appropriate values"
   (let* ((fret-count 4)
          (string-count 6)
          (fret-range (list 1 fret-count))
          (barre-list '())
          (dot-list '())
          (xo-list '())
          (output-list '())
          (new-props '())
          (items (string-split definition-string #\;)))
      (let parse-item ((myitems items))
          (if (not (null?  (cdr myitems))) 
              (let ((test-string (car myitems)))
                 (case (car (string->list (substring test-string 0 1))) 
                    ((#\s) (let ((size (get-numeric-from-key test-string)))
                                (set! new-props (acons 'size size new-props))))
                    ((#\f) (let* ((finger-code (get-numeric-from-key test-string))
                                  (finger-id (case finger-code
                                     ((0) 'none)
                                     ((1) 'in-dot) 
                                     ((2) 'below-string))))
                                (set! new-props
                                   (acons 'finger-code finger-id new-props))))
                    ((#\c) (set! output-list (cons-fret (cons 'barre (numerify (string-split (substring test-string 2) #\-)))
                                            output-list)))
                    ((#\h) (let ((fret-count (get-numeric-from-key test-string)))
                                (set! new-props (acons 'fret-count fret-count new-props))))
                    ((#\w) (let ((string-count (get-numeric-from-key test-string)))
                                (set! new-props (acons 'string-count string-count new-props))))
                    ((#\d) (let ((dot-size (get-numeric-from-key test-string)))
                                (set! new-props (acons 'dot-radius dot-size new-props))))
                    ((#\p) (let ((dot-position (get-numeric-from-key test-string)))
                                (set! new-props (acons 'dot-position dot-position new-props))))
                    (else 
                       (let ((this-list (string-split test-string #\-)))
                           (if (string->number (cadr this-list))
                              (set! output-list (cons-fret (cons 'place-fret (numerify this-list)) output-list))
                              (if (equal? (cadr this-list) "x" )
                                  (set! output-list (cons-fret (list 'mute (string->number (car this-list))) output-list))
                                  (set! output-list (cons-fret (list 'open (string->number (car this-list))) output-list)))))))
                 (parse-item (cdr myitems)))))
                 (if (eq? new-props '())
                 `(,props . ,output-list)
                 `(,(cons new-props props) . ,output-list))))

(define (cons-fret new-value old-list)
"  Put together a fret-list in the format desired by parse-string "
  (if (eq? old-list '())
      (list new-value)
      (cons* new-value old-list)))
                 
(define (get-numeric-from-key keystring)
 "Get the numeric value from a key  of the form k:val"
    (string->number (substring keystring 2 (string-length keystring))))
  
(define (numerify mylist)
 "Convert string values to numeric or character"
     (if (null? mylist)
         '()
         (let ((numeric-value (string->number (car mylist))))
             (if numeric-value
                (cons* numeric-value (numerify (cdr mylist)))
                (cons* (car (string->list (car mylist))) (numerify (cdr mylist)))))))
           
(define-markup-command (fret-diagram-terse layout props definition-string)
  (string?)
  "Make a fret diagram markup using terse string-based syntax.

Example
@example
 \\markup \\fret-diagram-terse #\"x;x;o;2;3;2;\" 
@end example
for a D chord diagram.

Syntax rules for @var{definition-string}:
@itemize @bullet

@item    
Strings are terminated by semicolons; the number of semicolons 
is the number of strings in the diagram.

@item
Mute strings are indicated by \"x\".

@item
Open strings are indicated by \"o\".

@item
A number indicates a fret indication at that fret.

@item
If there are multiple fret indicators desired on a string, they
should be separated by spaces.

@item
Fingerings are given by following the fret number with a \"-\",
followed by the finger indicator, e.g. 3-2 for playing the third
fret with the second finger.

@item
Where a barre indicator is desired, follow the fret (or fingering) symbol
with \"-(\" to start a barre and \"-)\" to end the barre.
@end itemize"
;TODO -- change syntax to fret\string-finger
       (let ((definition-list (fret-parse-terse-definition-string props definition-string)))
       (make-fret-diagram layout (car definition-list) (cdr definition-list))))

(define (fret-parse-terse-definition-string props definition-string)
 "parse a fret diagram string that uses terse syntax; return a pair containing:
    props, modified to include the string-count determined by the definition-string
    a fret-indication list with the appropriate values"
;TODO -- change syntax to  fret\string-finger
   (let* ((barre-start-list '())
          (output-list '())
          (new-props '())
          (items (string-split definition-string #\;))
          (string-count (- (length items) 1)))
      (let parse-item ((myitems items))
          (if (not (null?  (cdr myitems))) 
              (let* ((test-string (car myitems))
                    (current-string (- (length myitems) 1))
                    (indicators (string-split test-string #\ )))
                    (let parse-indicators ((myindicators indicators))
                       (if (not (eq? '() myindicators))
                           (let* ((this-list (string-split (car myindicators) #\-))
                                  (max-element-index (- (length this-list) 1))
                                  (last-element (car (list-tail this-list max-element-index)))
                                  (fret (if (string->number (car this-list)) (string->number (car this-list)) (car this-list))))
                               (if (equal? last-element "(")
                                   (begin
                                     (set! barre-start-list (cons-fret (list current-string fret) barre-start-list))
                                     (set! this-list (list-head this-list max-element-index))))
                               (if (equal? last-element ")")
                                   (let* ((this-barre (get-sub-list fret barre-start-list))
                                          (insert-index (- (length this-barre) 1)))
                                     (set! output-list (cons-fret (cons* 'barre (car this-barre) current-string (cdr this-barre)) 
                                                                  output-list))
                                     (set! this-list (list-head this-list  max-element-index))))
                               (if (number? fret)
                                   (set! output-list (cons-fret (cons* 'place-fret current-string (drop-paren (numerify this-list))) output-list))
                                   (if (equal? (car this-list) "x" )
                                       (set! output-list (cons-fret (list 'mute current-string) output-list))
                                       (set! output-list (cons-fret (list 'open current-string) output-list))))
                               (parse-indicators (cdr myindicators)))))
                 (parse-item (cdr myitems)))))
                 (set! new-props (acons 'string-count string-count new-props))
                 
                 `(,(cons new-props props) . ,output-list)))

(define (drop-paren item-list)
" drop a final parentheses from a fret indication list resulting from a terse string specification of barre."
     (if (> (length item-list) 0)
         (let* ((max-index (- (length item-list) 1))
            (last-element (car (list-tail item-list max-index))))
            (if (or (equal? last-element ")") (equal? last-element "("))
              (list-head item-list max-index) 
              item-list))
          item-list))
          
(define (get-sub-list value master-list)
" Get a sub-list whose cadr is equal to @var{value} from @var{master-list}"
    (if (eq? master-list '())
      #f
      (let ((sublist (car master-list)))
           (if (equal? (cadr sublist) value)
               sublist
               (get-sub-list value (cdr master-list))))))
