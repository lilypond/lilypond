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


;; ;; TODO
;; (define-public (add-stroke-straight stencil dir stroke-style)
;;   stencil
;; )
;;
;; ;; Create a stencil for a straight flag
;; ;; flag-thickness, -spacing are given in staff spaces
;; ;; *flag-length are given in black notehead widths
;; ;; TODO
;; (define-public (straight-flag flag-thickness flag-spacing
;;                        upflag-angle upflag-length
;;                        downflag-angle downflag-length)
;;   (lambda (stem-grob)
;;     (let* ((log (ly:grob-property stem-grob 'duration-log))
;;            (staff-space 1) ; TODO
;;            (black-notehead-width 1) ; TODO
;;            (stem-thickness 1) ; TODO: get rid of
;;            (half-stem-thickness (/ stem-thickness 2))
;;            (staff-space 1) ; TODO
;;            (up-length (+ (* upflag-length black-notehead-width) half-stem-thickness))
;;            (down-length (+ (* downflag-length black-notehead-width) half-stem-thickness))
;;            (thickness (* flag-thickness staff-space))
;;            (spacing (* flag-spacing staff-space)))
;;       empty-stencil
;;     )
;;   )
;; )
;;
;; ;; Modern straight flags: angles are not so large as with the old style
;; (define-public (modern-straight-flag stem-grob)
;;   ((straight-flag 0.55 0.9 -18 0.95 22 1.0) stem-grob))
;;
;; ;; Old-straight flags (Bach, etc.): quite large flag angles
;; (define-public (old-straight-flag stem-grob)
;;   ((straight-flag 0.55 0.9 -45 0.95 45 1.0) stem-grob))


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
          (ly:warning (_ "flag stroke `~a' or `~a'not found") font-char alt-font-char)
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
