(define nil '())
(define (fret-parse-string definition-string)
 "parse a fret diagram string and return an alist with the appropriate values"
   (let* ((fret-count 4)
          (string-count 6)
          (thickness 0.05)
          (finger-code 0)
          (dot-size 0.25)
          (position 0.6)
          (fret-range (list 1 fret-count))
          (barre-list '())
          (dot-list '())
          (xo-list '())
          (output-list '())
          (items (string-split definition-string #\;)))
      (let parse-item ((myitems items))
          (if (not (null?  (cdr myitems))) 
              (let ((test-string (car myitems)))
                 (case (car (string->list (substring test-string 0 1))) 
                    ((#\f) (set! finger-code (get-numeric-from-key test-string)))
                    ((#\t) (set! thickness (get-numeric-from-key test-string)))
                    ((#\c) (set! barre-list (cons* (numerify (string-split (substring test-string 2) #\-))
                                            barre-list)))
                    ((#\h) (set! fret-count (get-numeric-from-key test-string)))
                    ((#\w) (set! string-count (get-numeric-from-key test-string)))
                    ((#\d) (set! dot-size (get-numeric-from-key test-string)))
                    ((#\p) (set! position (get-numeric-from-key test-string)))
                    (else 
                       (let ((this-list (string-split test-string #\-)))
                           ;(display this-list)
                           (if (string->number (cadr this-list))
                              (set! dot-list (cons* (numerify this-list) dot-list))
                              (set! xo-list (cons* (numerify this-list) xo-list))))))
                 (parse-item (cdr myitems)))))
               ; calculate fret-range
               (let ((maxfret 0) (minfret 99))
                    (let updatemax ((fret-list dot-list))
                        (if (null?  fret-list)
                           '()
                           (let ((fretval (cadar fret-list)))
                               (if (> fretval maxfret) (set! maxfret fretval))
                               (if (< fretval minfret) (set! minfret fretval))
                               (updatemax (cdr fret-list)))))
                    (if (> maxfret fret-count)
                        (set! fret-range (list minfret
                             (let ((upfret (- (+ minfret fret-count) 1)))
                                  (if (> maxfret upfret) maxfret upfret)))))
                    ; subtract fret from dots
                    (set! dot-list (subtract-base-fret (- (car fret-range) 1) dot-list)))      
               (acons "string-count" string-count
               (acons "dot-size" dot-size
               (acons "position" position
               (acons "finger-code" finger-code
               (acons "fret-range" fret-range
               (acons "thickness" thickness
               (acons "barre-list" barre-list
               (acons "dot-list" dot-list
               (acons "xo-list" xo-list '())))))))))))
   
(define (subtract-base-fret base-fret dot-list)
  
  (if (null? dot-list)
      '()
      (let ((this-list (car dot-list)))
      (cons* (list (car this-list) (- (cadr this-list) base-fret) (if (null? (cddr this-list))
                                                                    nil
                                                                    (caddr this-list)))
             (subtract-base-fret base-fret (cdr dot-list))))))

(define (draw-strings string-count fret-range th size)
  (let* ((fret-count (+ (- (cadr fret-range) (car fret-range)) 1))
         (sl (* (+ fret-count 1) size))
         (half-thickness (* th 0.5))
         (string-stencil (ly:make-stencil (list 'draw-line th 0 0 0 sl)
                         (cons (- half-thickness) half-thickness)
                         (cons (- half-thickness) (+ sl half-thickness)))))
    (if (= string-count 1)
         string-stencil
        (ly:stencil-combine-at-edge
         (draw-strings (- string-count 1) fret-range th size) 0 1
         string-stencil
         (- size th) 0))))

(define (draw-fret-lines fret-count string-count th size)
    (let* ((fret-length (* (- string-count 1) size))
          (half-thickness (* th 0.5))
          (fret-line (ly:make-stencil (list 'draw-line th 0 size fret-length size)
                          (cons 0 fret-length)
                          (cons (- size half-thickness) (+  size half-thickness)))))
       (if (= fret-count 1)
         fret-line
         (ly:stencil-combine-at-edge fret-line Y UP
          (draw-fret-lines (- fret-count 1) string-count th size)
          (- size th) 0))))
 
(define (draw-frets paper fret-range string-count th size)
  (let* ((fret-count (+ (- (cadr fret-range) (car fret-range)) 1))
         (fret-length (* (- string-count 1) size))
         (half-thickness (* th 0.5))
         (base-fret (car fret-range)))
       (ly:stencil-combine-at-edge
          (draw-fret-lines fret-count string-count th size) Y UP
             (if (= base-fret 1)
                 (draw-fret-lines 1 string-count (* th 2) size)
                 (draw-fret-lines 1 string-count th size)) 
                 (- size th) 0))) 

(define (draw-dots paper string-count fret-range size dot-size position finger-code dot-list)
  "Make dots for fret diagram."
  (let* ((dot-radius (* size dot-size))
         (fret-count (+ (- (cadr fret-range) (car fret-range) 1)))
         (mypair (car dot-list))
         (restlist (cdr dot-list))
         (xpos (* (- string-count (car mypair)) size))
         (ypos (* (+ 4 (- fret-count (cadr mypair) position )) size))
         (finger (caddr mypair))
         (font (ly:paper-get-font paper `(((font-magnification . ,(* 0.8 size))(font-name . "cmss8")
                                        (font-encoding Tex-text)))))
         (font2 (ly:paper-get-font paper `(((font-magnification . ,(* (* 2 dot-size) size))(font-name . "cmss8")
                                        (font-encoding Tex-text)))))
         (font3 (ly:paper-get-font paper `(((font-magnification . ,(* (* 3 dot-size) size))(font-name . "cmss8")
                                        (font-encoding Tex-text)))))
         (extent (cons (- (*  size 0.25)) (*  size 0.25)))
         (dotstencil (if (or (eq? finger nil)(eq? finger-code 0))
                          (ly:make-stencil (list 'dot xpos ypos dot-radius ) extent extent)
                          (if (eq? finger-code 1)
  ; TODO -- Get nice circled numbers in the font, instead of this kludge
                             (ly:stencil-add 
                               (ly:stencil-translate-axis 
                                   (ly:stencil-translate-axis 
                                       (fontify-text font2 (number->string finger)) (- xpos (* size 0.3)) X)
                                   (- ypos (* 1 dot-size size)) Y)
                               (ly:stencil-translate-axis 
                                   (ly:stencil-translate-axis 
                                       (fontify-text font3 "O") (- xpos (* 2.2 dot-size size)) X)
                                   (- ypos (* 1.7 dot-size size)) Y))
                          (if (eq? finger-code 2) 
                              (ly:stencil-add 
                                   (ly:make-stencil (list 'dot xpos ypos dot-radius ) extent extent)
                                   (ly:stencil-translate-axis 
                                        (ly:stencil-translate-axis 
                                              (fontify-text font (number->string finger)) (- xpos (* size 0.3)) X)
                                        (- size) Y)))))))
    (if (null? restlist)
        dotstencil
        (ly:stencil-add (draw-dots paper string-count fret-range size dot-size position finger-code restlist)
                         dotstencil))))

(define (draw-xo paper string-count fret-range size xo-list) 
"Put x and o on chord diagram."
    (let* ((dot-radius (* size 0.25))
           (fret-count (+ (- (cadr fret-range) (car fret-range) 1)))
           (font (ly:paper-get-font paper `(((font-size . ,(* -5 (+ 1 (* 2.6 (- 1 size)))))(font-family . music)))))
           (mypair (car xo-list))
           (restlist (cdr xo-list))
;TODO -- get better glyphs in font to use for x (mute string) and o (open string)
;        Perhaps explore just using sans-serif font?
           (glyph-name (if (char=? (cadr mypair) #\x) "noteheads-2cross"
                         "scripts-open"))
           (tmpdot (if (char=? (cadr mypair) #\x) 0 (* size 0.25)))
           (xpos (if (char=? (cadr mypair) #\x)
                (- (* (- string-count (car mypair)) size) (* .35 size) )
                (* (- string-count (car mypair)) size)))
          (ypos (* (+ 3.5 fret-count) size))
          (extent (cons (- (* size 0.25)) (* size 0.25)))
          (glyph-stencil (ly:stencil-translate-axis 
                (ly:stencil-translate-axis (ly:find-glyph-by-name font glyph-name) ypos Y)
                xpos X)))
      (if (null? restlist)
          glyph-stencil
          (ly:stencil-add
            (draw-xo paper string-count fret-range size restlist)
            glyph-stencil))))

(define (make-bezier-sandwich-list left right bottom height thickness)
   (let* ((width (+ (- right left) 1))
          (x1 (+ (* width 0.1) left))
          (x2 (- right (* width 0.1)))
          (bottom-control-point-height (+ bottom (- height thickness)))
          (top-control-point-height (+ bottom height)))
; order of points is: left cp low, right cp low, right end low, left end low
;                     right cp high, left cp high, left end high, right end high.
       (list (cons x1 bottom-control-point-height) (cons x2 bottom-control-point-height) (cons right bottom) (cons left bottom)
             (cons x2 top-control-point-height) (cons x1 top-control-point-height) (cons left bottom) (cons right bottom))))

(define (draw-barre paper string-count fret-range size barre-list)
   "Create barre indications for a chord diagram"
   (if (not (null? barre-list))
     (let* ((string1 (caar barre-list))
            (string2 (cadar barre-list))
            (fret    (caddar barre-list))
            (bottom (* size (+ 1.5 (- (cadr fret-range) fret))))
            (left (* size (- string-count string1)))
            (right (* size (- string-count string2)))
            (bezier-list (make-bezier-sandwich-list left right bottom (* size 0.5) (* size 0.1)))
            (sandwich-stencil (ly:make-stencil (list 'bezier-sandwich `(quote ,bezier-list) (* size 0.1) )
                                  (cons 0 right)
                                  (cons 0 (+ bottom (* size 0.8))))))
        (if (not (null? (cdr barre-list)))
            (ly:stencil-add sandwich-stencil
                 (draw-barre paper string-count fret-range size (cdr barre-list)))
            sandwich-stencil ))))
 
(define (label-fret paper string-count fret-range size)
   "Label the base fret on a fret diagram"
   (let ((base-fret (car fret-range))
         (fret-count (+ (- (cadr fret-range) (car fret-range)) 1))
         (font (ly:paper-get-font paper `(((font-magnification . ,(* 0.8 size))(font-name . "cmss8")
                                        (font-encoding Tex-text))))))
     (ly:stencil-translate-axis 
        (ly:stencil-translate-axis (fontify-text font (if (> base-fret 1)
                                                          (format #f "~(~:@r~)" base-fret)
                                                          " ")) (* (- string-count 0.5) size) X)
        (* (- fret-count 0.2) size) Y)))
            
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
           
  
(define (make-fret-diagram paper size definition-string)
  "Make a fret diagram"
  (let* ((parameters (fret-parse-string definition-string))
         (string-count (cdr (assoc "string-count" parameters)))
         (fret-range (cdr (assoc "fret-range" parameters)))
         (finger-code (cdr (assoc "finger-code" parameters)))
         (dot-size (cdr (assoc "dot-size" parameters)))
         (position (cdr (assoc "position" parameters)))
         (dot-list (cdr (assoc "dot-list" parameters)))
         (xo-list (cdr (assoc "xo-list" parameters)))
         (line-thickness (cdr (assoc "thickness" parameters)))
         (barre-list (cdr (assoc "barre-list" parameters)))
         (fret-diagram-stencil (ly:stencil-add
                            (draw-strings string-count fret-range line-thickness size)
                            (draw-frets paper fret-range string-count line-thickness size))))
         (if (not (null? dot-list))
             (set! fret-diagram-stencil (ly:stencil-add
                                    (draw-dots paper string-count fret-range size dot-size position finger-code dot-list)
                                    fret-diagram-stencil)))
         (if (not (null? xo-list))
             (set! fret-diagram-stencil (ly:stencil-add
                                    (draw-xo paper string-count fret-range size xo-list)
                                    fret-diagram-stencil)))
         (if (not (null? barre-list))
             (set! fret-diagram-stencil (ly:stencil-add
                                    (draw-barre paper string-count fret-range size barre-list)
                                    fret-diagram-stencil)))
         (set! fret-diagram-stencil (ly:stencil-add  fret-diagram-stencil (label-fret paper string-count fret-range size)))
         (ly:stencil-align-to! fret-diagram-stencil X -.4)
         fret-diagram-stencil))

(def-markup-command (fret-diagram paper props size definition-string)
  (number? string?)
  "Syntax: \\fret-diagram size definition-string
   eg: \\markup \\fret-diagram #0.75 #\"6-x;5-x;4-o;3-2;2-3;1-2;\"
    for fret spacing 3/4 of staff space, D chord diagram
    Syntax rules for @var{definition-string}:
      Diagram items are separated by semicolons.
      Possible items:
      t:number -- set the line thickness (in staff spaces).  Default 0.05
      h:number -- set the height of the diagram in frets.  Default 4
      w:number -- set the width of the diagram in strings.  Default 6
      f:number -- set fingering label type 
                  (0 = none, 1 = in circle on string, 2 = below string)  Default 0
      d:number -- set radius of dot, in terms of fret spacing.  Default 0.25
      p:number -- set the position of the dot in the fret space. 0.5 is centered; 1 is on lower fret bar,
                  0 is on upper fret bar.  Default 0.6 
      c:string1-string2-fret -- include a barre mark from string1 to string2 on fret
      string-fret -- place a dot on string at fret.  If fret is o, string is identified
                     as open.  If fret is x, string is identified as muted.
      string-fret-fingering -- place a dot on string at fret, and label with fingering as 
                               defined by f: code.
    Note:  There is no limit to the number of fret indications per string."
       (make-fret-diagram paper size definition-string))
