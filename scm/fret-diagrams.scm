;;;; fret-diagrams.scm -- 
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 2004 Carl D. Sorensen <c_sorensen@byu.edu>

(define ly:paper-lookup ly:output-def-lookup) ; compat for 2.3, remove  when using 2.2
(define fontify-text-white fontify-text)  ;  temporary until fontify-text-white works properly (see draw-dots for usage)

;;TODO -- Change font interface from name, magnification to family, weight, size
;      Right now, using the desired interface gives an error, so we use name, magnification

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
               ; calculate fret-range
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

(define (draw-strings string-count fret-range th size)
"Draw the strings (vertical lines) for a fret diagram with @var{string-count} strings and frets as indicated
   in @var{fret-range}.  Line thickness is given by @var{th}, fret & string spacing by @var{size}. "
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
 "Draw @var{fret-count} frets (horizontal lines) for a fret diagram with @var{string-count} strings.
   Line thickness is given by @var{th}, fret & string spacing by @var{size}. "
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
          (top-fret-thick (* sth (chain-assoc-get 'top-fret-thickness props 3.0)))
          (top-half-thick (* top-fret-thick 0.5))
          (half-thick (* sth 0.5))
          (x1 half-thick)
          (x2 (+ half-thick (* size (- string-count 1))))
          (y1 0)
          (y2 top-fret-thick)
          (x-extent (cons (- x1) x2))
          (y-extent (cons 0 y2)))
          (ly:make-stencil (list 'round-filled-box x1 x2 y1 y2 th)
                            x-extent y-extent)))           
 
 
(define (draw-frets paper props fret-range string-count th size)
 "Draw the frets (horizontal lines) for a fret diagram with @var{string-count} strings and frets as indicated
   in @var{fret-range}.  Line thickness is given by @var{th}, fret & string spacing by @var{size}. "
  (let* ((top-fret-thick (* th (chain-assoc-get 'top-fret-thickness props 3.0)))
         (fret-count (+ (- (cadr fret-range) (car fret-range)) 1))
         (fret-length (* (- string-count 1) size))
         (half-thickness (* th 0.5))
         (base-fret (car fret-range)))
       (ly:stencil-combine-at-edge
          (draw-fret-lines fret-count string-count th size) Y UP
             (if (= base-fret 1)
                 (draw-thick-top-fret props string-count th size)
                 (draw-fret-lines 1 string-count th size)) 
                 (- size th) 0))) 
                 

(define (centered-text-stencil procedure font text)
"Create a centered text stencil of @var{text} in font @var{font} using stencil creation procedure @var{procedure}"
;UGH -- version check is necessary because 2.3 is not available on cygwin, so CDS development
;   needs 2.2 compatible ly:stencil-align-to!
;   Once 2.3 is built on cygwin, version check can go (fret-diagrams.scm is not part of dist for 2.2)
(let* ((text-stencil (procedure font text)))
     (if (= (cadr (ly:version)) 3)
	 (begin
	   (ly:stencil-align-to! text-stencil Y 0)
	   (ly:stencil-align-to! text-stencil X 0)
	   text-stencil)
	 (ly:stencil-align-to (ly:stencil-align-to text-stencil X 0) Y 0))))

(define (draw-dots paper props string-count fret-range size finger-code dot-circle-font-mag dot-position dot-radius dot-list)
  "Make dots for fret diagram."
;TODO -- move away from name,magnification font spec to family, size
;  Note -- family, size doesn't work with fontify-text procedure; need to fix that before we can make the switch
  (let* ((scale-dot-radius (* size dot-radius))
         (dot-color (chain-assoc-get 'dot-color props 'black))
         (finger-xoffset (chain-assoc-get 'finger-xoffset props -0.25))
         (finger-yoffset (chain-assoc-get 'finger-yoffset props (- size)))
;part of deprecated font interface 
         (label-font-name (chain-assoc-get 'label-font-name props "cmss8"))
         (white-dot-font-mag (* scale-dot-radius (chain-assoc-get 'white-dot-font-mag props 1.8))) 
         (dot-label-font-mag (* scale-dot-radius (chain-assoc-get 'dot-label-font-mag props 1.2)))
         (string-label-font-mag (* size (chain-assoc-get 'string-label-font-mag props 0.6)))
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
; desired font interface
          (string-label-font (ly:paper-get-font paper `(((font-family . sans)(font-encoding . latin1)(font-series . medium) (font-shape . upright)
                                        (font-size . ,(stepmag (* size string-label-font-mag)))))))
; deprecated font interface
;         (string-label-font (ly:paper-get-font paper `(((font-magnification . ,string-label-font-mag)
;                                                        (font-name . ,label-font-name)))))
; desired font interface
          (dot-label-font (ly:paper-get-font paper `(((font-family . sans)(font-encoding . latin1)(font-series . medium) (font-shape . upright)
                                        (font-size . ,(stepmag (* size dot-label-font-mag)))))))
; deprecated font interface
;         (dot-label-font (ly:paper-get-text-font paper `(((font-magnification . ,dot-label-font-mag)
;                                                     (font-name . ,label-font-name)))))
; desired font interface
          (dot-circle-font (ly:paper-get-font paper `(((font-family . sans)(font-encoding . latin1)(font-series . medium) (font-shape . upright)
                                        (font-size . ,(stepmag (* size dot-circle-font-mag)))))))
; deprecated font interface
;         (dot-circle-font (ly:paper-get-font paper `(((font-magnification . ,dot-circle-font-mag)
;                                                      (font-name . ,label-font-name)))))
; deprecated font interface
         (white-dot-font (ly:paper-get-font paper `(((font-magnification . ,white-dot-font-mag)
                                                      (font-name . ,label-font-name)))))
         (dotstencil  (if (eq? dot-color 'white)
                          (begin
                          (ly:make-stencil (list 'white-dot 0 0 scale-dot-radius) extent extent))
                          (ly:make-stencil (list 'dot 0 0 scale-dot-radius ) extent extent)))
         (positioned-dot (ly:stencil-translate-axis
                           (ly:stencil-translate-axis dotstencil xpos X)
                           ypos Y))
         (labeled-dot-stencil 
                 (if (or (eq? finger '())(eq? finger-code 'none))
                     positioned-dot
                 (if (eq? finger-code 'in-dot)
                    (let*  ((dot-proc (if (eq? dot-color 'white) 'white-dot 'dot))
                            (text-proc (if (eq? dot-color 'white) fontify-text fontify-text-white)))
                     (ly:stencil-add 
                        (ly:stencil-translate-axis 
                          (ly:stencil-translate-axis 
                              (centered-text-stencil text-proc dot-label-font finger) xpos X)
                              ypos Y)
                        (ly:stencil-translate-axis
                           (ly:stencil-translate-axis 
                              (ly:make-stencil (list dot-proc 0 0 scale-dot-radius) extent extent)
                               xpos X)
                           ypos Y)))
                 (if (eq? finger-code 'below-string) 
                     (ly:stencil-add 
                         positioned-dot
                         (ly:stencil-translate-axis 
                             (ly:stencil-translate-axis 
                                 (centered-text-stencil fontify-text string-label-font finger) xpos  X)
                             (* size finger-yoffset) Y))
                     ;unknown finger-code
                     positioned-dot)))))
    (if (null? restlist) 
        labeled-dot-stencil
        (ly:stencil-add 
            (draw-dots paper props string-count fret-range size finger-code dot-circle-font-mag
                          dot-position dot-radius restlist)
            labeled-dot-stencil))))

(define (draw-xo paper props string-count fret-range size xo-list) 
"Put open and mute string indications on diagram, as contained in @var{xo-list}."
;TODO -- Move away from name,mag font spec to family, size
    (let* ((fret-count (+ (- (cadr fret-range) (car fret-range) 1)))
           (xo-font-mag (* size (chain-assoc-get 'xo-font-magnification props 0.5)))
           (xo-font-name (chain-assoc-get 'xo-font-name props "cmss8"))
           (xo-horizontal-offset (* size (chain-assoc-get 'xo-horizontal-offset props -0.35)))
; desired font interface
;           (font (ly:paper-get-font paper `(((font-family . sans)(font-series . medium) (font-shape . upright)
;                                        (font-size . ,(stepmag (* size xo-font-mag)))))))
; deprecated font interface
           (font (ly:paper-get-font paper `(((font-magnification . ,xo-font-mag)
                                             (font-name . ,xo-font-name)))))
           (mypair (car xo-list))
           (restlist (cdr xo-list))
           (glyph-string (if (eq? (car mypair) 'mute) "X" "O"))
           (xpos (+ (* (- string-count (cadr mypair)) size) xo-horizontal-offset ))
           (glyph-stencil (ly:stencil-translate-axis (fontify-text font glyph-string) xpos X)))
      (if (null? restlist)
          glyph-stencil
          (ly:stencil-add
            (draw-xo paper props string-count fret-range size restlist)
            glyph-stencil))))

(define (make-bezier-sandwich-list left right bottom height thickness)
" Make the argument list for a horizontal bezier sandwich from @var{left} to @var{right} with a bottom at @var{bottom}, 
  a height of @var{height}, and a thickness of @var{thickness}."
   (let* ((width (+ (- right left) 1))
          (x1 (+ (* width thickness) left))
          (x2 (- right (* width thickness)))
          (bottom-control-point-height (+ bottom (- height thickness)))
          (top-control-point-height (+ bottom height)))
           ; order of points is: left cp low, right cp low, right end low, left end low
           ;                     right cp high, left cp high, left end high, right end high.
       (list (cons x1 bottom-control-point-height) (cons x2 bottom-control-point-height) (cons right bottom) (cons left bottom)
             (cons x2 top-control-point-height) (cons x1 top-control-point-height) (cons left bottom) (cons right bottom))))

(define (draw-barre paper props string-count fret-range size finger-code dot-circle-font-mag dot-position dot-radius barre-list)
   "Create barre indications for a fret diagram"
   (if (not (null? barre-list))
     (let* ((string1 (caar barre-list))
            (string2 (cadar barre-list))
            (fret    (caddar barre-list))
            (barre-vertical-offset (chain-assoc-get 'barre-vertical-offset props 0.5))
            ; 2 is 1 for empty fret at bottom of figure + 1 for interval (top-fret - fret + 1) -- not an arbitrary constant
            (bottom (+ (* size (- (+ 2 (- (cadr fret-range) fret))dot-position) ) (* size barre-vertical-offset dot-radius)))
            (left (* size (- string-count string1)))
            (right (* size (- string-count string2)))
            (bezier-thick (chain-assoc-get 'bezier-thickness props 0.1))
            (bezier-height (chain-assoc-get 'bezier-height props 0.5))
            (bezier-list (make-bezier-sandwich-list left right bottom (* size bezier-height) (* size bezier-thick)))
            (sandwich-stencil (ly:make-stencil (list 'bezier-sandwich `(quote ,bezier-list) (* size bezier-thick) )
                                  (cons 0 right)
                                  (cons 0 (+ bottom (* size bezier-height))))))
        (if (not (null? (cdr barre-list)))
            (ly:stencil-add sandwich-stencil
                 (draw-barre paper props string-count fret-range size finger-code dot-circle-font-mag
                      dot-position dot-radius (cdr barre-list)))
            sandwich-stencil ))))

  
(define (stepmag mag)
"Calculate the font step necessary to get a desired magnification"
(* 6 (/ (log mag) (log 2))))

(define (label-fret paper props string-count fret-range size)
   "Label the base fret on a fret diagram"
;TODO -- move away from name,magnification font spec to family, size
   (let* ((base-fret (car fret-range))
          (label-font-mag (chain-assoc-get 'fret-label-font-magnification props 0.7))
;          (label-horizontal-offset (chain-assoc-get 'fret-label-horizontal-offset props -0.5))
          (label-vertical-offset (chain-assoc-get 'fret-label-vertical-offset props -0.2))
          (fret-count (+ (- (cadr fret-range) (car fret-range)) 1))
; desired font interface
;          (font (ly:paper-get-font paper `(((font-family . sans)(font-series . medium) (font-shape . upright)
;                                        (font-size . ,(stepmag (* size label-font-mag))))))))
; deprecated font interface
          (font (ly:paper-get-font paper `(((font-magnification . ,label-font-mag)
                                                      (font-name . "cmss8"))))))          
       (ly:stencil-translate-axis (fontify-text font (format #f "~(~:@r~)" base-fret)) 
                       (* size (+ fret-count label-vertical-offset)) Y)))
 
(def-markup-command (fret-diagram-verbose paper props marking-list)
  (list?)
;TODO -- put table in doc string
  "Make a fret diagram containing the symbols indicated in @var{marking-list}
  
  Syntax: \\fret-diagram   marking-list
  
  For example,
  
@example
   \\markup \\fret-diagram #'((mute 6) (mute 5) (open 4) (place-fret 3 2) (place-fret 2 3) (place-fret 1 2))
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
Place a fret playing indication on string @var{string-number} at fret @var{fret-number} with an optional 
fingering label @var{finger-value}.  By default, the fret playing indicator is a solid dot.  This can be
changed by setting the value of the variable @var{dot-color}.  If the @var{finger} 
part of the place-fret element is present, @var{finger-value} will be displayed according to the setting of the variable
@var{finger-code}.  There is no limit to the number of fret indications per string.
@end table
"
   (make-fret-diagram paper props marking-list))
   
(define (make-fret-diagram paper props marking-list)
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
         (dot-position (chain-assoc-get 'dot-position props 0.6)) ; needed for both draw-dots and draw-barre
         (dot-radius (chain-assoc-get 'dot-radius props 0.25))  ; needed for both draw-dots and draw-barre
         (finger-code (chain-assoc-get 'finger-code props 'none))  ; needed for both draw-dots and draw-barre
         (dot-circle-font-mag (* size (chain-assoc-get 'dot-circle-font-mag props .75))) ; needed for both draw-dots and draw-barre
         (th (* (ly:paper-lookup paper 'linethickness)
                (chain-assoc-get 'thickness props 0.5))) ; needed for both draw-frets and draw-strings
                
         (alignment (chain-assoc-get 'alignment props -0.4)) ; needed only here
         (xo-padding (* th (chain-assoc-get 'xo-padding props 2))) ; needed only here

         (parameters (fret-parse-marking-list marking-list fret-count))
         (dot-list (cdr (assoc 'dot-list parameters)))
         (xo-list (cdr (assoc 'xo-list parameters)))
         (fret-range (cdr (assoc 'fret-range parameters)))
         (barre-list (cdr (assoc 'barre-list parameters)))
         (fret-diagram-stencil (ly:stencil-add
                            (draw-strings string-count fret-range th size)
                            (draw-frets paper props fret-range string-count th size))))
         (if (not (null? barre-list))
             (set! fret-diagram-stencil (ly:stencil-add
                                    (draw-barre paper props string-count fret-range size finger-code dot-circle-font-mag 
                                                dot-position dot-radius barre-list)
                                    fret-diagram-stencil)))
         (if (not (null? dot-list))
             (set! fret-diagram-stencil (ly:stencil-add
                                    (draw-dots paper props string-count fret-range size finger-code dot-circle-font-mag
                                          dot-position dot-radius dot-list)
                                    fret-diagram-stencil)))
         (if (not (null? xo-list))
             (set! fret-diagram-stencil (ly:stencil-combine-at-edge
                                    fret-diagram-stencil Y UP
                                    (draw-xo paper props string-count fret-range size xo-list) xo-padding 0)))
         (if (> (car fret-range) 1) 
             (set! fret-diagram-stencil
                   (ly:stencil-combine-at-edge fret-diagram-stencil X RIGHT
                                              (label-fret paper props string-count fret-range size) 0 0)))
         (ly:stencil-align-to! fret-diagram-stencil X alignment)
         fret-diagram-stencil))
         
(def-markup-command (fret-diagram paper props definition-string)
  (string?)
;TODO -- put table in doc string
  "Syntax: \\fret-diagram definition-string
  
eg: \\markup \\fret-diagram #\"s:0.75;6-x;5-x;4-o;3-2;2-3;1-2;\"

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
;       (define new-props (acons 'size size '()))
;       (set! props (cons new-props props))
       (let ((definition-list (fret-parse-definition-string props definition-string)))
       (make-fret-diagram paper (car definition-list) (cdr definition-list))))

(define (fret-parse-definition-string props definition-string)
 "parse a fret diagram string and return a pair containing:
  props, modified as necessary by the definition-string
  a fret-indication list with the appropriate values"
   (let* ((fret-count 4)
          (string-count 6)
   ;       (thickness 0.05)
   ;       (finger-code 0)
   ;       (dot-size 0.25)
   ;       (dot-position 0.6)
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
    (string->number (substring keystring 2  (string-length keystring) )))
  
(define (numerify mylist)
 "Convert string values to numeric or character"
     (if (null? mylist)
         '()
         (let ((numeric-value (string->number (car mylist))))
             (if numeric-value
                (cons* numeric-value (numerify (cdr mylist)))
                (cons* (car (string->list (car mylist))) (numerify (cdr mylist)))))))
           
(def-markup-command (fret-diagram-terse paper props definition-string)
  (string?)
;TODO -- put table in doc string
  "Make a fret diagram markup using terse string-based syntax.

Syntax: \\fret-diagram-terse definition-string

eg: \\markup \\fret-diagram #\"x;x;o;2;3;2;\" for a D chord diagram.

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
       (make-fret-diagram paper (car definition-list) (cdr definition-list))))

(define (fret-parse-terse-definition-string props definition-string)
 "parse a fret diagram string that uses terse syntax; return a pair containing:
    props, modified to include the string-count determined by the definition-string
    a fret-indication list with the appropriate values"
;TODO -- change syntax to  fret\string-finger
;TODO -- fix bug that doesn't allow multiple indications per string
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
                               (if (equal? last-element "(") ; here I add ) to balance parentheses for my editor
                                   (begin
                                     (set! barre-start-list (cons-fret (list current-string fret) barre-start-list))
                                     (set! this-list (list-head this-list max-element-index))))
                               (if (equal? last-element ")") ; here I add ( to balance parentheses for my editor
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
