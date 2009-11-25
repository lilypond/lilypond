;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2008--2009 Reinhold Kainhofer <reinhold@kainhofer.com>
;;;;
;;;; LilyPond is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; LilyPond is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.

;;;;  This file implements different flag styles in Scheme / GUILE, most
;;;;  notably the old-straight-flag and the modern-straight-flag styles.


(define-public (no-flag stem-grob)
  "No flag: Simply return empty stencil"
  empty-stencil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Straight flags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-public (add-stroke-straight stencil stem-grob dir log stroke-style offset length thickness stroke-thickness)
  "Add the stroke for acciaccatura to the given flag stencil.
  The stroke starts for up-flags at upper-end-of-flag+(0,length/2) and 
  ends at (0, vertical-center-of-flag-end) - (flag-x-width/2, flag-x-width + flag-thickness).
  Here length is the whole length, while flag-x-width is just the 
  x-extent and thus depends on the angle! Other combinations don't look as 
  good... For down-stems the y-coordinates are simply mirrored."
  (let* ((start (offset-add offset (cons 0  (* (/ length 2) dir))))
         (end (offset-add (cons 0 (cdr offset)) 
                          (cons (- (/ (car offset) 2)) (* (- (+ thickness (car offset))) dir))))
         (stroke (make-line-stencil stroke-thickness (car start) (cdr start) (car end) (cdr end))))
  (ly:stencil-add stencil stroke)))

(define PI-OVER-180  (/ (atan 1 1) 45))
(define (degrees->radians angle-degrees)
  "Convert the given angle from degrees to radians"
  (* angle-degrees PI-OVER-180))

(define (polar->rectangular radius angle-in-degrees)
  "Convert polar coordinate @code{radius} and @code{angle-in-degrees}
   to (x-length . y-length)"
  (let* ((complex (make-polar
                    radius
                    (degrees->radians angle-in-degrees))))
     (cons
       (real-part complex)
       (imag-part complex))))

(define (buildflag flag-stencil remain curr-stencil spacing)
  "Internal function to recursively create a stencil with @code{remain} flags
   from the single-flag stencil curr-stencil, which is already translated to
   the position of the previous flag position."
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
     *flag-angle is given in degree, *flag-length is given in staff spaces. 
     All lengths will be scaled according to the font size of the note."
  (lambda (stem-grob)
    (let* ((log (ly:grob-property stem-grob 'duration-log))
           (dir (ly:grob-property stem-grob 'direction))
           (stem-up (eqv? dir UP))
           (layout (ly:grob-layout stem-grob))
           ; scale with the note size (e.g. for grace notes)
           (factor (magstep (ly:grob-property stem-grob 'font-size 0)))
           (grob-stem-thickness (ly:grob-property stem-grob 'thickness))
           (line-thickness (ly:output-def-lookup layout 'line-thickness))
           (half-stem-thickness (/ (* grob-stem-thickness line-thickness) 2))
           (raw-length (if stem-up upflag-length downflag-length))
           (angle (if stem-up upflag-angle downflag-angle))
           (flag-length (+ (* raw-length factor) half-stem-thickness))
           (flag-end (polar->rectangular flag-length angle))
           (thickness (* flag-thickness factor))
           (thickness-offset (cons 0 (* -1 thickness dir)))
           (spacing (* -1 flag-spacing factor dir ))
           (start (cons (- half-stem-thickness) (* half-stem-thickness dir)))
           ; The points of a round-filled-polygon need to be given in clockwise
           ; order, otherwise the polygon will be enlarged by blot-size*2!
           (points (if stem-up (list start flag-end
                                     (offset-add flag-end thickness-offset)
                                     (offset-add start thickness-offset))
                               (list start
                                     (offset-add start thickness-offset)
                                     (offset-add flag-end thickness-offset)
                                     flag-end)))
           (stencil (ly:round-filled-polygon points half-stem-thickness))
           ; Log for 1/8 is 3, so we need to subtract 3
           (flag-stencil (buildflag stencil (- log 3) stencil spacing))
           (stroke-style (ly:grob-property stem-grob 'stroke-style)))
    (if (equal? stroke-style "grace")
      (add-stroke-straight flag-stencil stem-grob
                           dir log
                           stroke-style
                           flag-end flag-length
                           thickness
                           (* half-stem-thickness 2))
      flag-stencil))))

(define-public (modern-straight-flag stem-grob)
  "Modern straight flag style (for composers like Stockhausen, Boulez, etc.).
   The angles are 18 and 22 degrees and thus smaller than for the ancient style
   of Bach etc."
  ((straight-flag 0.55 1 -18 1.1 22 1.2) stem-grob))

(define-public (old-straight-flag stem-grob)
  "Old straight flag style (for composers like Bach). The angles of the flags
   are both 45 degrees."
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
