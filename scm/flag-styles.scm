;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2008--2022 Reinhold Kainhofer <reinhold@kainhofer.com>
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
;;;;  notably the old-straight-flag, the modern-straight-flag and the flat-flag
;;;;  styles.


(define-public (no-flag grob)
  "No flag: Simply return empty stencil."
  empty-stencil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Straight flags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-public (add-stroke-straight stencil grob dir log stroke-style
                                    offset length thickness stroke-thickness)
  "Add the stroke for acciaccatura to the given flag stencil.
The stroke starts for up-flags at `upper-end-of-flag + (0,length/2)'
and ends at `(0, vertical-center-of-flag-end) - (flag-x-width/2,
flag-x-width + flag-thickness)'.  Here `length' is the
whole length, while `flag-x-width' is just the x@tie{}extent and thus
depends on the angle!  Other combinations don't look as good.

For down-stems the y@tie{}coordinates are simply mirrored."
  (let* ((stem-grob (ly:grob-parent grob X))
         (start (offset-add offset (cons 0  (* (/ length 2) dir))))
         (end (offset-add (cons 0 (cdr offset))
                          (cons (- (/ (car offset) 2)) (* (- (+ thickness (car offset))) dir))))
         (stroke (make-line-stencil stroke-thickness (car start) (cdr start) (car end) (cdr end))))
    (ly:stencil-add stencil stroke)))

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
  "Create a stencil for a straight flag.  @var{flag-thickness} and
@var{flag-spacing} are given in staff spaces, @var{upflag-angle} and
@var{downflag-angle} are given in degrees, and @var{upflag-length} and
@var{downflag-length} are given in staff spaces.

All lengths are scaled according to the font size of the note."

  (lambda (grob)
    (let* ((stem-grob (ly:grob-parent grob X))
           (log (ly:grob-property stem-grob 'duration-log))
           (dir (ly:grob-property stem-grob 'direction))
           (stem-up (eqv? dir UP))
           (layout (ly:grob-layout grob))
           (staff-space (ly:output-def-lookup layout 'staff-space))
           ;; scale with font size-and staff-space (e.g. for grace notes)
           (factor
            (* staff-space
               (magstep (ly:grob-property grob 'font-size 0))))
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
           (raw-points
            (list
             '(0 . 0)
             flag-end
             (offset-add flag-end thickness-offset)
             thickness-offset))
           (points (map (lambda (coord) (offset-add coord start)) raw-points))
           (stencil (ly:round-polygon points half-stem-thickness -1.0))
           ;; Log for 1/8 is 3, so we need to subtract 3
           (flag-stencil (buildflag stencil (- log 3) stencil spacing))
           (stroke-style (ly:grob-property grob 'stroke-style)))
      (cond ((eq? (ly:grob-property grob 'style) 'no-flag)
             empty-stencil)
            ((equal? stroke-style "grace")
             (add-stroke-straight flag-stencil grob
                                  dir log
                                  stroke-style
                                  flag-end flag-length
                                  thickness
                                  (* half-stem-thickness 2)))
            (else flag-stencil)))))

(define-public (modern-straight-flag grob)
  "Modern straight flag style (for composers like Stockhausen, Boulez, etc.).
The angles are 18 and 22@tie{}degrees and thus smaller than for the ancient
style of Bach, etc."
  ((straight-flag 0.55 1 -18 1.1 22 1.2) grob))

(define-public (old-straight-flag grob)
  "Old straight flag style (for composers like Bach).  The angles of the
flags are both 45@tie{}degrees."
  ((straight-flag 0.55 1 -45 1.2 45 1.4) grob))

(define-public (flat-flag grob)
  "Flat flag style.  The angles of the flags are both 0@tie{}degrees."
  ((straight-flag 0.55 1.0 0 1.0 0 1.0) grob))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Flags created from feta glyphs (normal and mensural flags)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; NOTE: By default, lilypond uses the C++ method Flag::stencil
;; (ly:flag::stencil is the corresponding Scheme interface) to generate the
;; flag stencil. The following functions are simply a reimplementation in
;; Scheme, so that one has that functionality available in Scheme, if one
;; wants to write a flag style, which modifies one of the standard flags
;; by some stencil operations.


(define-public (add-stroke-glyph stencil grob dir stroke-style flag-style)
  "Load and add a stroke (represented by a glyph in the font) to the given
flag stencil."
  (if (not (string? stroke-style))
      stencil
      ;; Otherwise: look up the stroke glyph and combine it with the flag
      (let* ((stem-grob (ly:grob-parent grob X))
             (font-char (string-append "flags." flag-style dir stroke-style))
             (alt-font-char (string-append "flags." dir stroke-style))
             (font (ly:grob-default-font grob))
             (tmpstencil (ly:font-get-glyph font font-char))
             (stroke-stencil (if (ly:stencil-empty? tmpstencil)
                                 (ly:font-get-glyph font alt-font-char)
                                 tmpstencil)))
        (if (ly:stencil-empty? stroke-stencil)
            (begin
              (ly:warning (G_ "flag stroke `~a' or `~a' not found") font-char alt-font-char)
              stencil)
            (ly:stencil-add stencil stroke-stencil)))))


(define-public (retrieve-glyph-flag flag-style dir dir-modifier grob)
  "Load the correct flag glyph from the font."
  (let* ((stem-grob (ly:grob-parent grob X))
         (log (ly:grob-property stem-grob 'duration-log))
         (font (ly:grob-default-font grob))
         (font-char (string-append "flags." flag-style dir dir-modifier (number->string log)))
         (flag (ly:font-get-glyph font font-char)))
    (if (ly:stencil-empty? flag)
        (ly:warning (G_ "flag ~a not found") font-char))
    flag))


(define-public (create-glyph-flag flag-style dir-modifier grob)
  "Create a flag stencil by looking up the glyph from the font."
  (let* ((stem-grob (ly:grob-parent grob X))
         (dir (if (eqv? (ly:grob-property stem-grob 'direction) UP) "u" "d"))
         (flag (retrieve-glyph-flag flag-style dir dir-modifier grob))
         (stroke-style (ly:grob-property grob 'stroke-style)))
    (cond ((eq? (ly:grob-property grob 'style) 'no-flag)
           empty-stencil)
          ((null? stroke-style)
           flag)
          (else
           (add-stroke-glyph flag grob dir stroke-style flag-style)))))


(define-public (mensural-flag grob)
  "Mensural flags: Create the flag stencil by loading the glyph from the font.
Flags are always aligned with staff lines, so we need to check the end point
of the stem: For stems ending on staff lines, use different flags than for
notes between staff lines.  The idea is that flags are always vertically
aligned with the staff lines, regardless of whether the note head is on a
staff line or between two staff lines.  In other words, the inner end of
a flag always touches a staff line."

  (let* ((stem-grob (ly:grob-parent grob X))
         (adjust #t)
         (d (ly:grob-property stem-grob 'direction))
         (ss (ly:staff-symbol-staff-space stem-grob))
         (stem-end (inexact->exact (round (* (index-cell
                                              (ly:grob-extent stem-grob
                                                              stem-grob
                                                              Y)
                                              d)
                                             (/ 2 ss)))))
         ;; For some reason the stem-end is a real instead of an integer...
         (dir-modifier (if (ly:position-on-line? stem-grob stem-end) "0" "1"))
         (modifier (if adjust dir-modifier "2")))
    (create-glyph-flag "mensural" modifier grob)))


(define-public ((glyph-flag flag-style) grob)
  "Simulatesthe default way of generating flags: Look up glyphs
@code{flags.style[ud][1234]} from the feta font and use it for the flag
stencil."
  (create-glyph-flag flag-style "" grob))


(define-public (normal-flag grob)
  "Create a default flag."
  (create-glyph-flag "" "" grob))


(define-public (default-flag grob)
  "Create a flag stencil for the stem.  Its style is derived from the
@code{'style} Flag property.  By default, @code{lilypond} uses a
C++ Function (which is slightly faster) to do exactly the same as this
function.  However, if one wants to modify the default flags, this function
can be used to obtain the default flag stencil, which can then be modified
at will.  The correct way to do this is:

@example
\\override Flag #'stencil = #default-flag
\\override Flag #'style = #'mensural
@end example
"
  (let* ((stem-grob (ly:grob-parent grob X))
         (flag-style-symbol (ly:grob-property grob 'style))
         (flag-style (if (symbol? flag-style-symbol)
                         (symbol->string flag-style-symbol)
                         "")))
    (cond
     ((equal? flag-style "") (normal-flag grob))
     ((equal? flag-style "mensural") (mensural-flag grob))
     ((equal? flag-style "no-flag") empty-stencil)
     (else ((glyph-flag flag-style) grob)))))
