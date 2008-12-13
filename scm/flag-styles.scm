;;;;  flag-styles.scm
;;;;
;;;;  source file of the GNU LilyPOnd music typesetter
;;;;

(define-public (no-flag stem-grob)
  "No flag: Simply return empty stencil"
  empty-stencil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Straight flags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-public (add-stroke-straight stencil stem-grob stem-up? log stroke-style offset length thickness stroke-thickness)
  "Add the stroke for acciaccatura to the given flag stencil."
  (let* ((udmult (if stem-up? 1 -1))
         (start (offset-add offset (cons 0  (* (/ length 2) udmult))))
         (end (offset-add (cons 0 (cdr offset)) 
                          (cons (- (/ (car offset) 2)) (* (- (+ thickness (car offset))) udmult))))
         (stroke (make-line-stencil stroke-thickness (car start) (cdr start) (car end) (cdr end))))
  (ly:stencil-add stencil stroke)))

(define (polar->rectangular radius angle-in-degrees)
  "Convert polar coordinate @code{radius} and @code{angle-in-degrees}
   to (x-length . y-length)"
  (let* ((conversion-constant (/ (atan 1 1) 45))
         (complex (make-polar
                    radius
                    (* conversion-constant angle-in-degrees))))
     (cons
       (real-part complex)
       (imag-part complex))))

(define (buildflag flag-stencil remain curr-stencil spacing)
  (if (> remain 0)
      (let* ((translated-stencil (ly:stencil-translate-axis curr-stencil spacing Y))
             (new-stencil (ly:stencil-add flag-stencil translated-stencil)))
        (buildflag new-stencil (- remain 1) translated-stencil spacing))
      flag-stencil))

(define-public (straight-flag flag-thickness flag-spacing
                       upflag-angle upflag-length
                       downflag-angle downflag-length)
    "Create a stencil for a straight flag.
     flag-thickness, -spacing are given in staff spaces,
     *flag-angle is given in degree,
     *flag-length is given in staff spaces"
  (lambda (stem-grob)
    (let* ((log (ly:grob-property stem-grob 'duration-log))
           (layout (ly:grob-layout stem-grob))
           (stem-up? (eqv? (ly:grob-property stem-grob 'direction) UP))
           ; scale with the note size (e.g. for grace notes). Default fontsize 
           ; is fs==0, each step is ~12.246% larger / smaller
           (fs (ly:grob-property stem-grob 'font-size))
           (factor (if (number? fs) (expt 1.12246 fs) 1))
           (grob-stem-thickness (ly:grob-property stem-grob 'thickness))
           (line-thickness (ly:output-def-lookup layout 'line-thickness))
           (half-stem-thickness (/ (* grob-stem-thickness line-thickness) 2))
           (up-length (+ (* upflag-length factor) half-stem-thickness))
           (up-off (polar->rectangular up-length upflag-angle))
           (down-length (+ (* downflag-length factor) half-stem-thickness))
           (down-off (polar->rectangular down-length downflag-angle))
           (thickness (* flag-thickness factor))
           (offset (cons 0 (if stem-up? (- thickness) thickness)))
           (spacing (* flag-spacing factor (if stem-up? -1 1)))
           (start (cons (- half-stem-thickness) (if stem-up? half-stem-thickness (- half-stem-thickness))))
           (points (if stem-up? (list start up-off
                                      (offset-add up-off offset)
                                      (offset-add start offset))
                                (list start
                                      (offset-add start offset)
                                      (offset-add down-off offset)
                                      down-off)))
           (stencil (ly:round-filled-polygon points half-stem-thickness))
           ; Log for 1/8 is 3, so we need to subtract 3
           (flag-stencil (buildflag stencil (- log 3) stencil spacing))
           (stroke-style (ly:grob-property stem-grob 'stroke-style)))
    (if (null? stroke-style)
      flag-stencil
      (add-stroke-straight flag-stencil stem-grob
                           stem-up? log
                           stroke-style
                           (if stem-up? up-off down-off)
                           (if stem-up? up-length down-length)
                           thickness
                           (* half-stem-thickness 2))))))

;; Modern straight flags: angles are not as large as in the old style
(define-public (modern-straight-flag stem-grob)
  ((straight-flag 0.55 1 -18 1.1 22 1.2) stem-grob))

;; Old-straight flags (Bach, etc.): quite large flag angles
(define-public (old-straight-flag stem-grob)
  ((straight-flag 0.55 1 -45 1.2 45 1.4) stem-grob))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Flags created from feta glyphs (normal and mensural flags)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; NOTE: By default, lilypond uses the C++ method Stem::calc-flag
; (ly:stem::calc-flag is the corresponding Scheme interface) to generate the
; flag stencil. The following functions are simply a reimplementation in
; Scheme, so that one has that functionality available in Scheme, if one
; wants to write a flag style, which modifies one of the standard flags
; by some stencil operations.


(define-public (add-stroke-glyph stencil stem-grob dir stroke-style flag-style)
  "Load and add a stroke (represented by a glyph in the font) to the given
   flag stencil"
  (if (not (string? stroke-style))
    stencil
    ; Otherwise: look up the stroke glyph and combine it with the flag
    (let* ((font-char (string-append "flags." flag-style dir stroke-style))
           (alt-font-char (string-append "flags." dir stroke-style))
           (font (ly:grob-default-font stem-grob))
           (tmpstencil (ly:font-get-glyph font font-char))
           (stroke-stencil (if (ly:stencil-empty? tmpstencil)
                               (ly:font-get-glyph font alt-font-char)
                               tmpstencil)))
      (if (ly:stencil-empty? stroke-stencil)
        (begin
          (ly:warning (_ "flag stroke `~a' or `~a' not found") font-char alt-font-char)
          stencil)
        (ly:stencil-add stencil stroke-stencil)))))


(define-public (retrieve-glyph-flag flag-style dir dir-modifier stem-grob)
  "Load the correct flag glyph from the font"
  (let* ((log (ly:grob-property stem-grob 'duration-log))
         (font (ly:grob-default-font stem-grob))
         (font-char (string-append "flags." flag-style dir dir-modifier (number->string log)))
         (flag (ly:font-get-glyph font font-char)))
    (if (ly:stencil-empty? flag)
      (ly:warning "flag ~a not found" font-char))
    flag))


(define-public (create-glyph-flag flag-style dir-modifier stem-grob)
  "Create a flag stencil by looking up the glyph from the font"
  (let* ((dir (if (eqv? (ly:grob-property stem-grob 'direction) UP) "u" "d"))
         (flag (retrieve-glyph-flag flag-style dir dir-modifier stem-grob))
         (stroke-style (ly:grob-property stem-grob 'stroke-style)))
    (if (null? stroke-style)
      flag
      (add-stroke-glyph flag stem-grob dir stroke-style flag-style))))



(define-public (mensural-flag stem-grob)
  "Mensural flags: Create the flag stencil by loading the glyph from the font.
   Flags are always aligned with staff lines, so we need to check the end point
   of the stem: For stems ending on staff lines, use different flags than for 
   notes between staff lines.  The idea is that flags are always vertically 
   aligned with the staff lines, regardless of whether the note head is on a 
   staff line or between two staff lines.  In other words, the inner end of 
   a flag always touches a staff line."

  (let* ((adjust #t)
         (stem-end (inexact->exact (round (ly:grob-property stem-grob 'stem-end-position))))
         ; For some reason the stem-end is a real instead of an integer...
         (dir-modifier (if (ly:position-on-line? stem-grob stem-end) "1" "0"))
         (modifier (if adjust dir-modifier "2")))
    (create-glyph-flag "mensural" modifier stem-grob)))



(define-public ((glyph-flag flag-style) stem-grob)
  "Simulates the default way of generating flags: look up glyphs
   flags.style[ud][1234] from the feta font and use it for the flag stencil."
  (create-glyph-flag flag-style "" stem-grob))



(define-public (normal-flag stem-grob)
  "Create a default flag"
  (create-glyph-flag "" "" stem-grob))



(define-public (default-flag stem-grob)
  "Create a flag stencil for the stem. Its style will be derived from the 
   @code{'flag-style} Stem property. By default, @code{lilypond} uses a
   C++ Function (which is slightly faster) to do exactly the same as this 
   function. However, if one wants to modify the default flags, this function 
   can be used to obtain the default flag stencil, which can then be modified
   at will. The correct way to do this is:
@example
\\override Stem #'flag = #default-flag
\\override Stem #'flag-style = #'mensural
@end example
"
  (let* ((flag-style-symbol (ly:grob-property stem-grob 'flag-style))
         (flag-style (if (symbol? flag-style-symbol)
                         (symbol->string flag-style-symbol)
                         "")))
    (cond
        ((equal? flag-style "") (normal-flag stem-grob))
        ((equal? flag-style "mensural") (mensural-flag stem-grob))
        ((equal? flag-style "no-flag") (no-flag stem-grob))
        (else ((glyph-flag flag-style) stem-grob)))))
