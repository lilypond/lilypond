;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2008--2023 Reinhold Kainhofer <reinhold@kainhofer.com>
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
  "A callback for function @code{default-flag}, indicating @q{no flag}.

This function simply returns an empty stencil."
  empty-stencil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Straight flags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-public (add-stroke-straight stencil grob dir log stroke-style
                                    offset length thickness stroke-thickness)
  "Add an acciaccatura stroke to the given flag stencil.

This is an auxiliary function for @code{straight-flag}."


  ;; The stroke starts for up-flags at `upper-end-of-flag + (0,length/2)' and
  ;; ends at `(0, vertical-center-of-flag-end) - (flag-x-width/2, flag-x-width +
  ;; flag-thickness)'.  Here `length' is the whole length, while `flag-x-width'
  ;; is just the x-extent and thus depends on the angle!  Other combinations
  ;; don't look as good.
  ;;
  ;; For down-stems the y-coordinates are simply mirrored.
  (let* ((stem-grob (ly:grob-parent grob X))
         (start (offset-add offset (cons 0  (* (/ length 2) dir))))
         (end (offset-add (cons 0 (cdr offset))
                          (cons (- (/ (car offset) 2)) (* (- (+ thickness (car offset))) dir))))
         (stroke (make-line-stencil stroke-thickness (car start) (cdr start) (car end) (cdr end))))
    (ly:stencil-add stencil stroke)))

(define (buildflag flag-stencil remain curr-stencil spacing)
  "Internal function to recursively create a stencil with @var{remain} flags
   from the single-flag stencil @var{curr-stencil}, which is already translated
to the position of the previous flag position."
  (if (> remain 0)
      (let* ((translated-stencil (ly:stencil-translate-axis curr-stencil spacing Y))
             (new-stencil (ly:stencil-add flag-stencil translated-stencil)))
        (buildflag new-stencil (- remain 1) translated-stencil spacing))
      flag-stencil))

(define-public ((straight-flag flag-thickness flag-spacing
                               upflag-angle upflag-length
                               downflag-angle downflag-length) grob)
  "Construct a straight flag stencil function.

The constructed function expects a single argument, @var{grob}.

@var{flag-thickness} and @var{flag-spacing} are given in staff spaces,
@var{upflag-angle} and @var{downflag-angle} are given in degrees, and
@var{upflag-length} and @var{downflag-length} are given in staff spaces.

All lengths are scaled according to the font size of the note.  If the
@code{stroke-style} property in @var{grob} is set to the string
@code{\"grace\"}, add a slash through the flag.

This is an auxiliary function for @code{modern-straight-flag},
@code{old-straight-flag}, and @code{flat-flag}."
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
          (else flag-stencil))))

(define-public (modern-straight-flag grob)
  "A callback function for @code{Flag.stencil} to get a modern straight flag.

This is used by composers like Stockhausen or Boulez.

The straight flag angles are 18 and 22@tie{}degrees for up-stems and down-stems,
respectively, and thus smaller than for @code{old-straight-flag}.  If the caller
sets the @code{stroke-style} property of @var{grob} to the string
@code{\"grace\"}, add a slash through the flag.

This function returns a stencil."
  ((straight-flag 0.55 1 -18 1.1 22 1.2) grob))

(define-public (old-straight-flag grob)
  "A callback function for @code{Flag.stencil} to get an old straight flag.

This is used by composers like Bach.

The up-stem and down-stem angles of the flags are both 45@tie{}degrees.  If the
caller sets the @code{stroke-style} property of @var{grob} to the string
@code{\"grace\"}, add a slash through the flag.

This function returns a stencil."
  ((straight-flag 0.55 1 -45 1.2 45 1.4) grob))

(define-public (flat-flag grob)
  "A callback function for @code{Flag.stencil} to get a flat flag.

The up-stem and down-stem angles of the flags are both 0@tie{}degrees.  If the
caller sets the @code{stroke-style} property of @var{grob} to the string
@code{\"grace\"}, add a slash through the flag.

This function returns a stencil."
  ((straight-flag 0.55 1.0 0 1.0 0 1.0) grob))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Flags created from feta glyphs (normal and mensural flags)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; NOTE: By default, LilyPond uses the C++ method Flag::print (ly:flag::print is
;; the corresponding Scheme interface) to generate the flag stencil.  The
;; following functions are identical reimplementations in Scheme, making its
;; functionality available if someone wants to write a flag style that modifies
;; one of the standard flags by some stencil operations.


(define-public (add-stroke-glyph stencil grob dir stroke-style flag-style)
  "Add a stroke glyph (from the music font) to the given flag stencil.

This is an auxiliary function for @code{create-glyph-flag}."
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
  "Load the correct flag glyph from the music font.

This is an auxiliary function for @code{create-glyph-flag}."
  (let* ((stem-grob (ly:grob-parent grob X))
         (log (ly:grob-property stem-grob 'duration-log))
         (font (ly:grob-default-font grob))
         (font-char (string-append "flags." flag-style dir dir-modifier (number->string log)))
         (flag (ly:font-get-glyph font font-char)))
    (if (ly:stencil-empty? flag)
        (ly:warning (G_ "flag ~a not found") font-char))
    flag))


(define-public (create-glyph-flag flag-style dir-modifier grob)
  "Create a flag stencil by looking up the glyph from the music font.

This is an auxiliary function for @code{mensural-flag}, @code{glyph-flag}, and
@code{normal-flag}."
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
  "A callback for function @code{default-flag} to get a mensural flag.

Mensural flags are aligned with staff lines; for stems ending on staff lines,
use different flags than for notes between staff lines.  The idea is that the
inner end of a flag always touches a staff line.

The mensural flag glyph is taken from the music font; its name is
@code{flags.mensural@var{Dir}@var{Type}@var{Log}}.  @var{Dir} is the flag
direction (either @samp{u} or @samp{d}), @var{Type} is @samp{0} if the note head
is between staff lines and @samp{1} otherwise, @var{Log} is the duration log (an
integer in the range 3 to@tie{}6) from which the number of flags attached to the
stem is derived.  Both @var{Dir} and @var{Log} are taken from @var{grob}.
Example: @code{flags.mensuralu13}.

This function returns a stencil."
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
  "A callback for function @code{default-flag} to get a flag glyph.

This function actually constructs a function returning a stencil, expecting a
single argument, @var{grob}.

It looks up glyph @code{flags.@var{Style}@var{Dir}@var{Log}} in the music font
and uses it for the flag stencil.  @var{Style} is the flag style based on
@var{flag-style} (which can be empty), @var{Dir} is the flag direction (either
@samp{u} or @samp{d}), and @var{Log} the duration log (an integer in the range 3
to 10) from which the number of flags attached to the stem is derived.  Both
@var{Dir} and @var{Log} are taken from @var{grob}.  Example: @code{flags.u3}.

If @code{grob} has the @code{stroke-style} property set, add a second glyph with
the same glyph name components but use its value instead for @var{log}.
Example: @code{flags.ugrace}.

Not to be used with mensural flags, which have a slightly different naming
scheme (see function @code{mensural-flag})."
  (create-glyph-flag flag-style "" grob))


(define-public (normal-flag grob)
  "A callback for function @code{default-flag} to get a @q{normal} flag.

See function @code{glyph-flag} for the naming scheme of flag glyphs (with
argument @var{flag-style} set to the empty string).

This function returns a stencil."
  (create-glyph-flag "" "" grob))


(define-public (default-flag grob)
  "Create a flag stencil for the stem.

The flag style is derived from the @code{style} property of @var{grob} (which
must be of type @code{Flag}).

By default, LilyPond uses a C++ function (which is slightly faster) to do
exactly the same as this function.  However, if you want to modify the default
flags this function can be used to obtain the default flag stencil, which can
then be modified at will.

The available, predefined values for @code{style} are @code{\"\"} (empty, for
normal flags), @code{\"mensural\"}, and @code{\"no-flag\"}.  Other values are
used to construct glyph names for flags; see function @code{glyph-flag} for
details.

Example:

@example
\\override Flag.stencil = #default-flag
\\override Flag.style = #'mensural
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
     ((equal? flag-style "no-flag") (no-flag grob))
     (else ((glyph-flag flag-style) grob)))))
