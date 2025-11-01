;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2000--2023  Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;;                  Jan Nieuwenhuizen <janneke@gnu.org>
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

;;;
;;; Markup commands and markup-list commands definitions.
;;;
;;; Markup commands which are part of LilyPond, are defined
;;; in the (lily) module, which is the current module in this file,
;;; using the `define-markup-command' macro.
;;;
;;; Usage:
;;;
;;; (define-markup-command (command-name layout props args...)
;;;   args-signature
;;;   [ #:category category ]
;;;   [ #:properties property-bindings ]
;;;   [ #:as-string expression ]
;;;   documentation-string
;;;   ..body..)
;;;
;;; with:
;;;   command-name
;;;     the name of the markup command
;;;
;;;   layout and props
;;;     arguments that are automatically passed to the command when it
;;;     is interpreted.
;;;     `layout' is an output def, which properties can be accessed
;;;     using `ly:output-def-lookup'.
;;;     `props' is a list of property settings which can be accessed
;;;     using `chain-assoc-get' (more on that below)
;;;
;;;   args...
;;;     the command arguments.
;;;     There is no limitation on the order of command arguments.
;;;     However, markup functions taking a markup as their last
;;;     argument are somewhat special as you can apply them to a
;;;     markup list, and the result is a markup list where the
;;;     markup function (with the specified leading arguments) has
;;;     been applied to every element of the original markup list.
;;;
;;;     Since replicating the leading arguments for applying a
;;;     markup function to a markup list is cheap mostly for
;;;     Scheme arguments, you avoid performance pitfalls by just
;;;     using Scheme arguments for the leading arguments of markup
;;;     functions that take a markup as their last argument.
;;;
;;;   args-signature
;;;     the arguments signature, i.e., a list of type predicates which
;;;     are used to type check the arguments, and also to define the general
;;;     argument types (markup, markup-list, scheme) that the command is
;;;     expecting.
;;;     For instance, if a command expects a number, then a markup, the
;;;     signature would be: (number? markup?)
;;;
;;;   category
;;;     for documentation purposes, built-in markup commands are grouped by
;;;     category.  The available categories and their order are set up in
;;;     `category-name-alist` (in file `document-markup.scm`).
;;;
;;;   property-bindings
;;;     this is used both for documentation generation, and to ease
;;;     programming the command itself.  It is a list of
;;;        (property-name default-value)
;;;     or (property-name)
;;;     or XXX-markup
;;;     or XXX-markup-list
;;;     elements.  With the two first forms, the property with name `property-name` is
;;;     looked up in the `props` argument, and the symbol naming the property is bound
;;;     to its value.  When the property is not found in `props`, then the symbol is bound
;;;     to the given default value.  When no default value is given, #f is
;;;     used instead.
;;;     Thus, using the following property bindings:
;;;       ((thickness 0.1)
;;;        (font-size 0))
;;;     is equivalent to writing:
;;;       (let ((thickness (chain-assoc-get 'thickness props 0.1))
;;;             (font-size (chain-assoc-get 'font-size props 0)))
;;;         ..body..)
;;;     When a command B internally calls an other command A, it may be
;;;     desirable to see in B's documentation all the properties and
;;;     default values used by A.  In that case, add `A-markup` (if A is a
;;;     markup command) or `A-markup-list` (if A is a markup list command)
;;;     to the property-bindings of B.  (This is used when generating
;;;     documentation, but won't create bindings.)  A must be defined before B.
;;;
;;;   as-string
;;;     If given, this is an expression evaluated by markup->string for
;;;     lossily converting markups to strings.  See define-markup-command's
;;;     docstring for details.
;;;
;;;   documentation-string
;;;     the command documentation string (used to generate manuals)
;;;
;;;   body
;;;     the command body.  The function must return a stencil.
;;;
;;; Each markup command definition shall have a documentation string
;;; with description, syntax and example.

(use-modules (ice-9 receive)
             (ice-9 control)
             (ice-9 match)
             ((lily qr-code) #:select (qr-encode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public empty-stencil (ly:make-stencil '()
                                              empty-interval empty-interval))
(define-public point-stencil (ly:make-stencil "" '(0 . 0) '(0 . 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; line has to come early since it is often used implicitly from the
;; markup macro since \markup { a b c } -> \markup \line { a b c }
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (expand load eval)
  (define-markup-command (line layout props args)
    (markup-list?)
    #:category align
    #:properties ((word-space)
                  (text-direction RIGHT))
    "Put @var{args} into a horizontal line.

The property @code{word-space} determines the space between markups in
@var{args}.  For right-to-left scripts like Hebrew, @code{text-direction} should
be set to@tie{}-1.

@lilypond[verbatim,quote]
\\markup
  \\override #'(word-space . 3)
  \\column {
    \\line { \"A B\" \"C D\" \"E F\" }
    \\override #'(text-direction . -1)
    \\line { \"A B\" \"C D\" \"E F\" }
  }
@end lilypond"
    (let ((stencils (interpret-markup-list layout props args)))
      (if (= text-direction LEFT)
          (set! stencils (reverse stencils)))
      (stack-stencil-line word-space stencils))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; geometric shapes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: clean this up a bit.  User interfaces are not consistent.
;; - filled is sometimes a parameter, sometimes a property. blot
;;   likewise (called corner-radius for \rounded-box).
;; - Not all \xxx commands that draw something around an argument
;;   have a \draw-xxx counterpart drawing the shape in a standalone
;;   fashion.

(define-markup-command (draw-line layout props dest)
  (number-pair?)
  #:category graphic
  #:properties ((thickness 1))
  "
@cindex drawing line, within text

Draw a line along vector @var{dest}.

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

(define-markup-command (draw-dashed-line layout props dest)
  (number-pair?)
  #:category graphic
  #:properties ((thickness 1)
                (on 1)
                (off 1)
                (phase 0)
                (full-length #t))
  "
@cindex drawing dashed line, within text

Draw a dashed line along vector @var{dest}.

Properties @code{on} and @code{off} give the length of a dash and the space
between two dashes, respectively; @code{phase} shortens the first dash by the
given amount.

If the @code{full-length} property is set to @code{#t} (which is the default),
the value of property @code{off} (and @code{on} under some boundary conditions)
gets adjusted so that there is neither whitespace at the end of the line nor the
last dash truncated.

@lilypond[verbatim,quote]
\\markup {
  \\override #'((on . 0.3) (off . 0.5))
  \\draw-dashed-line #'(6 . 2)

  \\draw-dashed-line #'(6 . 2)

  \\override #'(full-length . #f)
  \\draw-dashed-line #'(6 . 2)

  \\override #'(phase . 0.5)
  \\draw-dashed-line #'(6 . 2)

  \\override #'((full-length . #f) (phase . 0.5))
  \\draw-dashed-line #'(6 . 2)
}
@end lilypond"
  (let* ((line-thickness (ly:output-def-lookup layout 'line-thickness))
         ;; Calculate the thickness to be used.
         (th (* line-thickness thickness))
         (half-thick (/ th 2))
         ;; Get the extensions in x- and y-direction.
         (x (car dest))
         (y (cdr dest))
         ;; Calculate the length of the dashed line.
         (line-length (sqrt (+ (expt x 2) (expt y 2)))))

    (if (and full-length (not (= (+ on off) 0)))
        (begin
          ;; Add double-thickness to avoid overlapping.
          (set! off (+ (* 2 th) off))
          (let* (;; Make a guess how often the off/on-pair should be printed
                 ;; after the initial `on'.
                 ;; Assume a minimum of 1 to avoid division by zero.
                 (guess (max 1 (round (/ (- line-length on) (+ off on)))))
                 ;; Not sure about the value or why corr is necessary at all,
                 ;; but it seems to be necessary.
                 (corr (if (= on 0)
                           (/ line-thickness 10)
                           0))
                 ;; Calculate a new value for off to fit the
                 ;; line-length.
                 (new-off (/ (- line-length corr (* (1+ guess) on)) guess))
                 )
            (cond

             ;; Settings for (= on 0). Resulting in a dotted line.

             ;; If line-length isn't shorter than `th', change the given
             ;; value for `off' to fit the line-length.
             ((and (= on 0) (< th line-length))
              (set! off new-off))

             ;; If the line-length is shorter than `th', it makes no
             ;; sense to adjust `off'. The rounded edges of the lines
             ;; would prevent any nice output.
             ;; Do nothing.
             ;; This will result in a single dot for very short lines.
             ((and (= on 0) (>= th line-length))
              #f)

             ;; Settings for (not (= on 0)). Resulting in a dashed line.

             ;; If line-length isn't shorter than one go of on-off-on,
             ;; change the given value for `off' to fit the line-length.
             ((< (+ (* 2 on) off) line-length)
              (set! off new-off))
             ;; If the line-length is too short, but greater than
             ;; (* 4 th) set on/off to (/ line-length 3)
             ((< (* 4 th) line-length)
              (set! on (/ line-length 3))
              (set! off (/ line-length 3)))
             ;; If the line-length is shorter than (* 4 th), it makes
             ;; no sense trying to adjust on/off. The rounded edges of
             ;; the lines would prevent any nice output.
             ;; Simply set `on' to line-length.
             (else
              (set! on line-length))))))

    ;; If `on' or `off' is negative, or the sum of `on' and `off' equals zero a
    ;; Ghostscript error occurs while calling
    ;; (ly:make-stencil (list 'dashed-line th on off x y phase) x-ext y-ext)
    ;; Better be paranoid.
    (if (or (= (+ on off) 0)
            (negative? on)
            (negative? off))
        (begin
          (ly:warning (G_ "Can't print a line - setting on/off to default"))
          (set! on 1)
          (set! off 1)))

    ;; To give the lines produced by \draw-line and \draw-dashed-line the same
    ;; length, half-thick has to be added to the stencil-extensions.
    (ly:make-stencil
     (list 'dashed-line th on off x y phase)
     (interval-widen (ordered-cons 0 x) half-thick)
     (interval-widen (ordered-cons 0 y) half-thick))))

(define-markup-command (draw-dotted-line layout props dest)
  (number-pair?)
  #:category graphic
  #:properties ((thickness 1)
                (off 1)
                (phase 0))
  "
@cindex drawing dotted line, within text

Draw a dotted line along vector @var{dest}.

Property @code{off} gives the space between two dots; its value gets adjusted so
that the first and last dot exactly start and end the line, respectively.
@code{phase} shifts all dots along the vector by the given amount.

@lilypond[verbatim,quote]
\\markup {
  \\draw-dotted-line #'(5.1 . 2.3)
  \\override #'((thickness . 2) (off . 0.2))
  \\draw-dotted-line #'(5.1 . 2.3)
}
@end lilypond"

  (let ((new-props (prepend-alist-chain 'on 0
                                        (prepend-alist-chain 'full-length #t props))))

    (interpret-markup layout
                      new-props
                      (make-draw-dashed-line-markup dest))))

(define-markup-command (draw-squiggle-line layout props sq-length dest eq-end?)
  (number? number-pair? boolean?)
  #:category graphic
  #:properties ((thickness 0.5)
                (angularity 0)
                (height 0.5)
                (orientation 1))
  "
@cindex drawing squiggled line, within text

Draw a squiggled line along vector @var{dest}.

@var{sq-length} is the length of the first bow; this value gets always adjusted
so that an integer number of squiggles is printed.  If @var{eq-end?} is set to
@code{#t}, the squiggled line ends with a bow in the same direction as the
starting one.

The appearance of the squiggled line may be customized by overriding the
@code{thickness}, @code{angularity}, @code{height}, and @code{orientation}
properties.

@lilypond[verbatim,quote]
\\markup
  \\column {
    \\draw-squiggle-line #0.5 #'(6 . 0) ##t
    \\override #'(orientation . -1)
    \\draw-squiggle-line #0.5 #'(6 . 0) ##t
    \\draw-squiggle-line #0.5 #'(6 . 0) ##f
    \\override #'(height . 1)
    \\draw-squiggle-line #0.5 #'(6 . 0) ##t
    \\override #'(thickness . 5)
    \\draw-squiggle-line #0.5 #'(6 . -2) ##t
    \\override #'(angularity . 2)
    \\draw-squiggle-line #0.5 #'(6 . 2) ##t
  }
@end lilypond"
  (let* ((line-thickness (ly:output-def-lookup layout 'line-thickness))
         (thick (* thickness line-thickness))
         (x (car dest))
         (y (cdr dest))
         (length-to-print (magnitude (make-rectangular x y)))
         ;; Make a guess how many bows may be needed
         (guess (max 1 (truncate (/ length-to-print sq-length))))
         ;; If `eq-end?' is set #t, make sure squiggle-line starts and ends
         ;; with a bow in same direction
         (amount (if (and (even? guess) eq-end?) (1+ guess) guess))
         ;; The lined-up bows needs to fit `length-to-print'
         ;; Thus scale the length of first bow accordingly
         ;; Other bows are copies
         (guessed-squiggle-line-length (* amount sq-length))
         (line-length-diff (- length-to-print guessed-squiggle-line-length))
         (line-length-diff-for-each-squiggle
          (/ line-length-diff amount))
         (first-bow-length (+ sq-length line-length-diff-for-each-squiggle))
         ;; Get first bows
         ;; TODO two bows are created via `make-bow-stencil'
         ;;      cheaper to use `ly:stencil-scale'?
         (first-bow-end-coord
          (cons
           (/ (* first-bow-length x) length-to-print)
           (/ (* first-bow-length y) length-to-print)))
         (init-bow
          (lambda (o)
            (make-bow-stencil
             '(0 . 0)
             first-bow-end-coord
             thick angularity height o)))
         (init-bow-up (init-bow orientation))
         (init-bow-down (init-bow (- orientation)))
         ;; Get a list of starting-points for the bows
         (list-of-starts
          (map
           (lambda (n)
             (cons
              (* n (car first-bow-end-coord))
              (* n (cdr first-bow-end-coord))))
           (iota amount))))
    ;; The final stencil: lined-up bows
    (apply ly:stencil-add
           (map
            ly:stencil-translate
            (circular-list init-bow-up init-bow-down)
            list-of-starts))))

(define-markup-command (draw-hline layout props)
  ()
  #:category graphic
  #:properties (draw-line-markup
                (line-width)
                (span-factor 1))
  "
@cindex drawing line, across a page

Draw a horizontal line.

The property @code{span-factor} sets the length of the line as a multiple of the
@code{line-width} property.

@lilypond[verbatim,quote,line-width=14\\cm]
\\markup {
  \\column {
    \\draw-hline
    \\override #'(span-factor . 1/3)
    \\draw-hline
  }
}
@end lilypond"
  (interpret-markup layout
                    props
                    (make-draw-line-markup (cons (* line-width
                                                    span-factor)
                                                 0))))

;; FIXME: when thickness is exactly 0, the border doesn't look
;; smooth at least in Frescobaldi's PDF viewer.  Not sure on
;; which side the problem is. --Jean AS
(define-markup-command (draw-circle layout props radius thickness filled)
  (number? number? boolean?)
  #:category graphic
  "
@cindex drawing circle, within text

Draw a circle with given @var{radius} and @var{thickness}.

Fill the circle if @var{filled} is set to @code{#t}.

@lilypond[verbatim,quote]
\\markup {
  \\draw-circle #2 #0.5 ##f
  \\hspace #2
  \\draw-circle #2 #0 ##t
}
@end lilypond"
  (make-circle-stencil radius thickness filled))

(define-markup-command (polygon layout props points)
  (number-pair-list?)
  #:category graphic
  #:properties ((extroversion 0) ; Same default as ly:round-polygon.
                (filled #t)
                (thickness 1))
  "
@cindex drawing polygon

A polygon delimited by the list of @var{points}.

Property @code{extroversion} defines how the shape of the polygon is adapted to
its thickness: if it is@tie{}0, the polygon is traced as-is.  If it is@tie{}-1,
the outer side of the line is just on the given points.  If set to@tie{}1, the
line has its inner side on the points.  The @code{thickness} property controls
the thickness of the line; for filled polygons, this means the diameter of the
blot.

@lilypond[verbatim,quote]
regularPentagon =
  #'((1 . 0) (0.31 . 0.95) (-0.81 . 0.59)
     (-0.81 . -0.59) (0.31 . -0.95))

\\markup \\scale #'(2 . 2) {
  \\polygon #'((-1 . -1) (0 . -3) (2 . 2) (1 . 2))
  \\override #'(filled . #f)
    \\override #'(thickness . 2)
      \\combine
        \\with-color #(universal-color 'blue)
          \\polygon #regularPentagon
        \\with-color #(universal-color 'vermillion)
          \\override #'(extroversion . 1)
            \\polygon #regularPentagon
}
@end lilypond"
  (ly:round-polygon
   points
   (* thickness (ly:output-def-lookup layout 'line-thickness))
   extroversion
   filled))

(define-markup-command (triangle layout props filled)
  (boolean?)
  #:category graphic
  #:properties ((extroversion 0)
                (font-size 0)
                (thickness 1))
  "
@cindex drawing triangle, within text

Draw a triangle.

Fill the triangle if @var{filled} is set to @code{#t}.

The appearance can be controlled with properties @code{extroversion},
@code{font-size}, and @code{thickness}.

@lilypond[verbatim,quote]
\\markup {
  \\triangle ##t
  \\triangle ##f
  \\override #'(font-size . 5)
  \\override #'(thickness . 5) {
    \\override #'(extroversion . 1)
    \\triangle ##f
    \\override #'(extroversion . -1)
    \\triangle ##f
  }
}
@end lilypond"
  ;; The value 1.8 was found by trial and error (previously, it was 0.8 *
  ;; baseline-skip, which was only effective if the values for baseline-skip
  ;; and font-size were both close to their default values)
  (let ((ex (* (magstep font-size) 1.8)))
    (interpret-markup
     layout
     ;; TODO: make 'filled' a property rather than a parameter?
     (cons `((filled . ,filled))
           props)
     (make-polygon-markup
      (list
       (cons 0.0 0.0)
       (cons ex 0.0)
       (cons (* 0.5 ex) (* 0.86 ex)))))))

(define-markup-command (circle layout props arg)
  (markup?)
  #:category graphic
  #:properties ((thickness 1)
                (font-size 0)
                (circle-padding 0.2)
                (bbox #f))
  ;; \circle <digit> and \circle <english_letter> could be transformed
  ;; to U+2460 etc., but there are a limited number of them, so it
  ;; would create inconsistencies when things outside the set are used.
  #:as-string (format #f "(~a)"
                      (markup->string arg #:layout layout #:props props))
  "
@cindex circling text

Draw a circle around @var{arg}.

Use properties @code{thickness}, @code{circle-padding}, and @code{font-size} to
set the line thickness and padding around the markup.  If @code{bbox} is set to
@code{#t}, make the circle enclose the bounding box of @var{arg}, otherwise use
either the width or the height of @var{arg} (whatever is larger) as the
diameter.

@lilypond[verbatim,quote]
\\markup {
  \\circle {
    Hi
  }
  \\circle {
    \\center-column { \"short\" \"short\" \"very very long\" }
  }
  \\override #'(bbox . #t) \\circle {
    \\center-column { \"short\" \"short\" \"very very long\" }
  }
}
@end lilypond

Note that the circle does not horizontally displace its argument.  Use markup
commands like @code{\\left-align} or @code{\\table} to make LilyPond realign it."
  (let ((th (* (ly:output-def-lookup layout 'line-thickness)
               thickness))
        (pad (* (magstep font-size) circle-padding))
        (m (interpret-markup layout props arg)))
    (circle-stencil m th pad bbox)))

(define-markup-command (ellipse layout props arg)
  (markup?)
  #:category graphic
  #:properties ((thickness 1)
                (font-size 0)
                (x-padding 0.2)
                (y-padding 0.2))
  "
@cindex drawing ellipse, around text

Draw an ellipse around @var{arg}.

Use properties @code{thickness}, @code{x-padding}, @code{y-padding}, and
@code{font-size} to set the line thickness and padding around the markup.

This is the same as function @code{\\oval} but with different padding defaults.

@lilypond[verbatim,quote]
\\markup {
  \\ellipse {
    Hi
  }
}
@end lilypond

Note that the ellipse does not horizontally displace its argument.  Use markup
commands like @code{\\left-align} or @code{\\table} to make LilyPond realign it."
  (let ((th (* (ly:output-def-lookup layout 'line-thickness)
               thickness))
        (pad-x (* (magstep font-size) x-padding))
        (pad-y (* (magstep font-size) y-padding))
        (m (interpret-markup layout props arg)))
    (ellipse-stencil m th pad-x pad-y)))

(define-markup-command (oval layout props arg)
  (markup?)
  #:category graphic
  #:properties ((thickness 1)
                (font-size 0)
                (x-padding 0.75)
                (y-padding 0.75))
  #:as-string (format #f "(~a)"
                      (markup->string arg #:layout layout #:props props))
  "
@cindex drawing oval, around text

Draw an oval around @var{arg}.

Use properties @code{thickness}, @code{x-padding}, @code{y-padding}, and
@code{font-size} to set the line thickness and padding around the markup.

This is the same as function @code{\\ellipse} but with different padding
defaults.

@lilypond[verbatim,quote]
\\markup {
  \\oval {
    Hi
  }
}
@end lilypond

Note that the oval does not horizontally displace its argument.  Use markup
commands like @code{\\left-align} or @code{\\table} to make LilyPond realign
it."
  (let ((th (* (ly:output-def-lookup layout 'line-thickness)
               thickness))
        (pad-x (* (magstep font-size) x-padding))
        (pad-y (* (magstep font-size) y-padding))
        (m (interpret-markup layout props arg)))
    (oval-stencil m th pad-x pad-y)))

(define-markup-command (with-url layout props url arg)
  (string? markup?)
  #:category graphic
  #:as-string (format #f "~a [~a]"
                      (markup->string arg #:layout layout #:props props)
                      url)
  "
@cindex inserting URL link, into text

Add a link to URL @var{url} around @var{arg}.

This only works in the PDF backend.@footnote{Due to technical limitations the
link doesn't work here in the Notation Reference.}

@lilypond[verbatim,quote]
\\markup {
  \\with-url \"https://lilypond.org/\" {
    LilyPond ... \\italic {
      music notation for everyone
    }
  }
}
@end lilypond"
  (let* ((stil (interpret-markup layout props arg))
         (xextent (ly:stencil-extent stil X))
         (yextent (ly:stencil-extent stil Y))
         (url-expr `(url-link ,url ,xextent ,yextent)))
    (ly:stencil-add (ly:make-stencil url-expr xextent yextent) stil)))

(define-markup-command (page-link layout props page-number arg)
  (number? markup?)
  #:category other
  "
@cindex referencing page number, in text

Add a link to a score's page @var{page-number} around @var{arg}.

This only works in the PDF backend.

@lilypond[verbatim,quote]
\\markup {
  \\page-link #2 { \\italic { This links to page 2... } }
}
@end lilypond"
  (let* ((stil (interpret-markup layout props arg))
         (xextent (ly:stencil-extent stil X))
         (yextent (ly:stencil-extent stil Y))
         (link-expr `(page-link ,page-number ,xextent ,yextent)))
    (ly:stencil-add (ly:make-stencil link-expr xextent yextent) stil)))

(define-markup-command (qr-code layout props width str) (non-negative-number? string?)
  #:properties ((error-correction-level 'low)
                (quiet-zone-size 4))
  #:category other
  "
@cindex QR code
@cindex URL link, as QR code
@cindex hyperlink, as QR code

Insert a QR code for string @var{str}, usually a URL, with a given @var{width}.

@lilypond[verbatim,quote]
\\markup \\vcenter {
  \\center-column { Engraved with LilyPond }
  \\hspace #1.5
  \\qr-code #10.0 \"https://lilypond.org\"
}
@end lilypond

The @code{error-correction-level} property can be set to one of the symbols
@code{low}, @code{medium}, @code{quarter}, or @code{high}.  The higher
the level of error correction is, the more the QR code contains redundancy,
potentially helping detectors, e.g., in poor lighting conditions; however,
a higher correction level also makes the code denser.

@lilypond[verbatim,quote]
\\markup \\vcenter {
  \\center-column { Engraved with LilyPond }
  \\hspace #1.5
  \\override #'(error-correction-level . high)
    \\qr-code #10.0 \"https://lilypond.org\"
}
@end lilypond

The @code{quiet-zone-size} property specifies the width of the @qq{quiet zone},
namely the white area around the QR code.  It is expressed as a multiple of the
width of one little square inside the QR code.  Use at least 4 for best results."
  (let ((arr (catch 'qr-code-error
                    (lambda ()
                      (qr-encode str error-correction-level))
                    (lambda (_key)
                      ;; a warning has already been emitted
                      #f))))
    (if arr
        (match-let*
            (((size _size) (array-dimensions arr))
             (square-width (/ width size))
             (square (make-filled-box-stencil `(0 . ,square-width)
                                              `(0 . ,square-width)))
             (rows (array->list arr))
             (main-qr-stencil
              (apply ly:stencil-add
                     (index-map
                      (lambda (i row)
                        (apply ly:stencil-add
                               (index-map
                                (lambda (j is-black)
                                  (let* ((color (if is-black black white))
                                         (colored-square (stencil-with-color square color))
                                         (offset (cons (* j square-width)
                                                       (* (- size 1 i) square-width))))
                                    (ly:stencil-translate colored-square offset)))
                                row)))
                      rows))))
          (centered-stencil
           (stencil-pad-around
            (* square-width quiet-zone-size)
            main-qr-stencil)))
        empty-stencil)))


(define-public (book-first-page layout props)
  "Return the @code{'first-page-number} of the entire book."
  (let loop ((layout layout))
    (let ((parent (ly:output-def-parent layout #f)))
      (if parent
          (loop parent)
          (ly:output-def-lookup layout 'first-page-number)))))

(define-markup-command (with-link layout props label arg)
  (symbol? markup?)
  #:category other
  "
@cindex referencing page label, in text

Add a link to the page holding label @var{label} around @var{arg}.

This only works in the PDF backend.

@example
\\markup @{
  \\with-link #'label @{
    \\italic @{ This links to the page
               containing the label... @}
  @}
@}
@end example"
  (let* ((arg-stencil (interpret-markup layout props arg))
         (x-ext (ly:stencil-extent arg-stencil X))
         (y-ext (ly:stencil-extent arg-stencil Y)))
    (ly:stencil-add
     (ly:make-stencil
      `(delay-stencil-evaluation
        ,(delay (let* ((table (ly:output-def-lookup layout 'label-page-table))
                       (table-page-number
                        (if (list? table)
                            (assoc-get label table)
                            #f))
                       (first-page-number (book-first-page layout props))
                       (current-page-number
                        (if table-page-number
                            (1+ (- table-page-number first-page-number))
                            #f)))
                  `(page-link ,current-page-number
                              ,x-ext ,y-ext))))
      x-ext
      y-ext)
     arg-stencil)))

(define-markup-command (beam layout props width slope thickness)
  (number? number? number?)
  #:category graphic
  "
@cindex drawing beam, within text

Draw a beam with given @var{width}, @var{slope}, and @var{thickness}.

@lilypond[verbatim,quote]
\\markup {
  \\beam #5 #1 #2
}
@end lilypond"
  (let* ((y (* slope width))
         (yext (cons (min 0 y) (max 0 y)))
         (half (/ thickness 2)))

    (ly:make-stencil
     `(polygon ,(list
                 0 (/ thickness -2)
                 width (+ (* width slope)  (/ thickness -2))
                 width (+ (* width slope)  (/ thickness 2))
                 0 (/ thickness 2))
               ,(ly:output-def-lookup layout 'blot-diameter)
               #t)
     (cons 0 width)
     (cons (+ (- half) (car yext))
           (+ half (cdr yext))))))

(define-markup-command (underline layout props arg)
  (markup?)
  #:category font
  #:properties ((thickness 1) (offset 2) (underline-shift 0) (underline-skip 2))
  ;; TODO: should we add a #:as-string handler formatting as _arg_?
  "
@cindex underlining text

Underline @var{arg}.

This function looks at the property @code{thickness} to determine the line
thickness, at @code{offset} to determine the line's vertical offset from
@var{arg}, and at @code{underline-skip} to determine the distance of additional
lines from the others.

The @code{underline-shift} property is used to make subsequent calls work
correctly.  Overriding it makes little sense since it would end up adding the
provided value to the one of @code{offset}.

@lilypond[verbatim,quote,line-width=14\\cm]
\\markup \\justify-line {
  \\underline \"underlined\"
  \\override #'(offset . 5)
  \\override #'(thickness . 1)
  \\underline \"underlined\"
  \\override #'(offset . 1)
  \\override #'(thickness . 5)
  \\underline \"underlined\"
  \\override #'(offset . 5)
  \\override #'(underline-skip . 4)
  \\underline \\underline \\underline \"underlined thrice\"
}
@end lilypond"
  (let* ((thick (ly:output-def-lookup layout 'line-thickness))
         (underline-thick (* thickness thick))
         (m (interpret-markup
             layout
             ;; For multiple calls of underline-markup, this will result in
             ;; the innermost underline ending up lowest.
             (prepend-alist-chain
              'underline-shift
              (+ underline-skip underline-shift)
              props)
             arg))
         (arg-x-ext (ly:stencil-extent m X))
         (x1 (car arg-x-ext))
         (x2 (cdr arg-x-ext))
         (y (* thick (- (+ offset underline-shift))))
         (raw-line-stil (make-line-stencil underline-thick x1 y x2 y))
         (line
          (ly:make-stencil
           (ly:stencil-expr raw-line-stil)
           ;; We use x-extent of the arg-stencil instead of the line-stencil
           ;; to avoid increasing lines with multiple calls of underline.
           ;; As a consequence the line sticks out a bit into the space
           ;; between elements of continuing text, without affecting it.
           ;; For huge values of thickness this may cause undesired output,
           ;; we regard this a very rare case, though.
           ;; Alternatively we could have shortened the underline by its
           ;; thickness, i.e. raw-line-stil would have been:
           ;;    (make-line-stencil
           ;;      underline-thick
           ;;      (+ x1 (/ underline-thick 2))
           ;;      y
           ;;      (- x2 (/ underline-thick 2))
           ;;      y))
           ;; without need to reset x-extent, this causes a different ugliness
           ;; for huge thickness, though.
           arg-x-ext
           (ly:stencil-extent raw-line-stil Y))))
    (ly:stencil-add m line)))

(define-markup-command (tie layout props arg)
  (markup?)
  #:category font
  #:properties ((thickness 1)
                (offset 2)
                (direction UP)
                (height-limit 0.7)
                (shorten-pair '(0 . 0)))
  "
@cindex tie-ing text

Add a horizontal bow at the bottom or top of @var{arg}.

This function uses @code{make-tie-stencil} to create the bow; it looks at the
@code{thickness} and @code{offset} properties to determine the line thickness
and vertical offset, respectively.  The added bow fits the extent of @var{arg};
use the @code{shorten-pair} property to modify this.  The @code{direction}
property may be set explicitly using @code{override} or direction modifiers, or
implicitly by using @code{voiceOne}, etc.

@lilypond[verbatim,quote]
\\markup {
  \\override #'(direction . 1)
  \\tie \"above\"
  \\override #'(direction . -1)
  \\tie \"below\"
}
@end lilypond

See also @code{\\undertie} and @code{\\overtie}, which are shorthands for this
function."
  (let* ((line-thickness (ly:output-def-lookup layout 'line-thickness))
         (thick (* thickness line-thickness))
         (stil (interpret-markup layout props arg))
         (x1 (car (ly:stencil-extent stil X)))
         (x2 (cdr (ly:stencil-extent stil X)))
         (y-ext (ly:stencil-extent stil Y))
         (y (+ (* line-thickness offset direction)
               ;; we put out zero for positive text-direction, to make it
               ;; consistent with `underline-markup'
               ;; TODO: this will be problematic for args like "Eng"
               ;;       fix it here _and_ in `underline-markup'
               (if (negative? direction) 0 (cdr y-ext))))
         (tie
          (make-tie-stencil
           (cons (+ x1 (car shorten-pair) line-thickness) y)
           (cons (- x2 (cdr shorten-pair) line-thickness) y)
           thick
           direction
           ;; For usage in text we choose a little less `height-limit'
           ;; than the default for `Tie', i.e 0.7 (see properties above)
           ;; TODO add the other optional arguments of `make-tie-stencil'
           ;; i.e. `ratio' and `angularity' ?
           height-limit)))
    (ly:stencil-add stil tie)))

(define-markup-command (undertie layout props arg)
  (markup?)
  #:category font
  #:properties (tie-markup)
  "
@cindex undertie-ing text

Print a tie under @var{arg}.

@lilypond[verbatim,quote]
\\markup \\line {
  \\undertie \"undertied\"
  \\override #'((offset . 5) (thickness . 1))
  \\undertie \"undertied\"
  \\override #'((offset . 1) (thickness . 5))
  \\undertie \"undertied\"
}
@end lilypond"
  (interpret-markup layout (prepend-alist-chain 'direction DOWN props)
                    (make-tie-markup arg)))

(define-markup-command (overtie layout props arg)
  (markup?)
  #:category font
  #:properties (tie-markup)
  "
@cindex overtie-ing text

Overtie @var{arg}.

@lilypond[verbatim,quote]
\\markup \\line {
  \\overtie \"overtied\"
  \\override #'((offset . 5) (thickness . 1))
  \\overtie \"overtied\"
  \\override #'((offset . 1) (thickness . 5))
  \\overtie \"overtied\"
}
@end lilypond"
  (interpret-markup layout (prepend-alist-chain 'direction UP props)
                    (make-tie-markup arg)))

(define-markup-command (box layout props arg)
  (markup?)
  #:category font
  #:properties ((thickness 1)
                (font-size 0)
                (box-padding 0.2))
  #:as-string (format #f "[~a]"
                      (markup->string arg #:layout layout #:props props))
  "
@cindex enclosing text, in box

Draw a box around @var{arg}.

This function looks at the @code{thickness}, @code{box-padding}, and
@code{font-size} properties to determine the line thickness and padding around
the markup.

@lilypond[verbatim,quote]
\\markup {
  \\override #'(box-padding . 0.5)
  \\box \\line { V. S. }
}
@end lilypond

Note that the box does not horizontally displace its argument.  Use markup
commands like @code{\\left-align} or @code{\\table} to make LilyPond realign
it.

@lilypond[verbatim,quote]
\\markup {
  \\override #'(box-padding . 1.5)
  \\column {
    \"text\"
    \\box \"text\"
    \\left-align \\box \"text\"
  }
}
@end lilypond
"
  (let* ((th (* (ly:output-def-lookup layout 'line-thickness)
                thickness))
         (pad (* (magstep font-size) box-padding))
         (m (interpret-markup layout props arg)))
    (box-stencil m th pad)))

(define-markup-command (filled-box layout props xext yext blot)
  (number-pair? number-pair? number?)
  #:category graphic
  "
@cindex drawing solid box, within text
@cindex drawing box, with rounded corners

Draw a box of dimensions @var{xext} and @var{yext}, with rounded corners given
by @var{blot}.

For example,

@example
\\filled-box #'(-.3 . 1.8) #'(-.3 . 1.8) #0
@end example

@noindent
creates a box extending horizontally from -0.3 to 1.8 and
vertically from -0.3 up to 1.8, with corners formed from a
circle of diameter@tie{}0 (i.e., sharp corners).

@lilypond[verbatim,quote]
\\markup {
  \\filled-box #'(0 . 4) #'(0 . 4) #0
  \\filled-box #'(0 . 2) #'(-4 . 2) #0.4
  \\combine
    \\filled-box #'(1 . 8) #'(0 . 7) #0.2
    \\with-color #white
      \\filled-box #'(3.6 . 5.6) #'(3.5 . 5.5) #0.7
}
@end lilypond"
  (ly:round-filled-box
   xext yext blot))

(define-markup-command (rounded-box layout props arg)
  (markup?)
  #:category graphic
  #:properties ((thickness 1)
                (corner-radius 1)
                (font-size 0)
                (box-padding 0.5))
  #:as-string (format #f "[~a]"
                      (markup->string arg #:layout layout #:props props))
  "
@cindex enclosing text, in box with rounded corners
@cindex drawing box, with rounded corners, around text

Draw a box with rounded corners around @var{arg}.

This function looks at properties @code{thickness}, @code{box-padding}, and
@code{font-size} to determine the line thickness and padding around the
@var{arg}.  The @code{corner-radius} property defines the radius of the round
corners (default value is@tie{}1).

@lilypond[verbatim,quote,relative=2]
c4^\\markup {
  \\rounded-box {
    Overtura
  }
}
c,8. c16 c4 r
@end lilypond

Note that the box does not horizontally displace its argument.  Use markup
commands like @code{\\left-align} or @code{\\table} to make LilyPond realign
it.

@lilypond[verbatim,quote]
\\markup {
  \\override #'(box-padding . 1.5)
  \\column {
    \"text\"
    \\rounded-box \"text\"
    \\left-align \\rounded-box \"text\"
  }
}
@end lilypond
"
  (let ((th (* (ly:output-def-lookup layout 'line-thickness)
               thickness))
        (pad (* (magstep font-size) box-padding))
        (m (interpret-markup layout props arg)))
    (rounded-box-stencil m th pad corner-radius)))

(define-markup-command (rotate layout props ang arg)
  (number? markup?)
  #:category align
  "
@cindex rotating text

Rotate @var{arg} by @var{ang} degrees around its center.

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

(define-markup-command (whiteout layout props arg)
  (markup?)
  #:category other
  #:properties ((style 'box)
                (thickness '())
                (color "white"))
  "
@cindex adding white background, to text

Provide a white background for @var{arg}.

The shape of the white background is determined by the @code{style} property.
The default is @code{box} which produces a rectangle.  @code{rounded-box}
produces a rounded rectangle, and @code{outline} approximates the outline of the
markup.

The color of the background can be controlled with the @code{color} property,
defaulting to @code{\"white\"}.

@lilypond[verbatim,quote]
\\markup {
  \\combine
    \\filled-box #'(-1 . 62) #'(-3 . 4) #1
    \\override #'(line-width . 60)
      \\fill-line {
        \\override #'(thickness . 1.5)
          \\whiteout box
        \\override #'((style . rounded-box) (thickness . 3))
          \\whiteout rounded-box
        \\override #'((style . outline) (thickness . 3))
          \\whiteout outline
        \\override #'((color . \"red\") (style . outline))
          \\whiteout red-outline
      }
}
@end lilypond"
  (stencil-whiteout
   (interpret-markup layout props arg)
   style
   thickness
   (ly:output-def-lookup layout 'line-thickness)
   color))

(define-markup-command (pad-markup layout props amount arg)
  (number? markup?)
  #:category align
  "
@cindex padding text
@cindex putting space around text

Add padding @var{amount} all around @var{arg}.

Identical to function @code{\\pad-around}.

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
  (stencil-pad-around amount (interpret-markup layout props arg)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; space
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-markup-command (strut layout props)
  ()
  #:category other
  "
@cindex creating vertical space, in text

Create a box of the same height as the space in the current font."
  (let ((m (ly:text-interface::interpret-markup layout props " ")))
    (ly:make-stencil (ly:stencil-expr m)
                     '(0 . 0)
                     (ly:stencil-extent m X)
                     )))

(define-markup-command (hspace layout props amount)
  (number?)
  #:category align
  "
@cindex creating horizontal space, in text

Create an invisible object taking up @var{amount} horizontal space.

@lilypond[verbatim,quote]
\\markup {
  one
  \\hspace #2
  two
  \\hspace #8
  three
}
@end lilypond

@var{amount} can be also a negative value, which can be best visualized as if
the current drawing point gets moved to the left.

@lilypond[verbatim,quote]
\\markup \\concat {
  \\hspace #4
  \\column {
    \\box \\concat { AAAA \\hspace #4 }
    \\box \\concat { AAAA \\hspace #-4 }
    \\box \\concat { \\hspace #4 AAAA }
    \\box \\concat { \\hspace #-4 AAAA }
  }
}
@end lilypond

See also @code{\\abs-hspace}."
  (ly:make-stencil "" (cons 0 amount) empty-interval))

(define-markup-command (abs-hspace layout props amount)
  (number?)
  #:category align
  "
@cindex creating horizontal space, in text

Create an invisible object taking up absolute horizontal space of @var{amount}
points.

@lilypond[verbatim,quote]
\\markup {
  one
  \\abs-hspace #20
  two
  \\abs-hspace #40
  three
}
@end lilypond

See also @code{\\hspace}."
  (ly:make-stencil "" (cons 0 (to-staff-space amount)) empty-interval))

(define-markup-command (vspace layout props amount)
  (number?)
  #:category align
  "
@cindex creating vertical space, in text

Create an invisible object taking up vertical space of @var{amount} multiplied
by@tie{}3.

@lilypond[verbatim,quote]
\\markup {
    \\center-column {
    one
    \\vspace #1
    two
    \\vspace #3
    three
  }
}
@end lilypond

@var{amount} can be also a negative value, which can be best visualized as if
the current drawing point gets moved up.

@lilypond[verbatim,quote]
\\markup {
  \\vspace #1
  \\box \\column { AAAA \\vspace #0.4 }
  \\box \\column { AAAA \\vspace #-0.4 }
  \\box \\column { \\vspace #0.4 AAAA }
  \\box \\column { \\vspace #-0.4 AAAA }
}
@end lilypond

See also @code{\\abs-vspace}."
  (let ((amount (* amount 3.0)))
    (ly:make-stencil "" empty-interval (cons 0 amount))))

(define-markup-command (abs-vspace layout props amount)
  (number?)
  #:category align
  "
@cindex creating vertical space, in text

Create an invisible object taking up absolute vertical space of @var{amount}
points.

@lilypond[verbatim,quote]
\\markup {
    \\center-column {
    one
    \\abs-vspace #20
    two
    \\abs-vspace #40
    three
  }
}
@end lilypond

See also @code{\\vspace}."
  (ly:make-stencil "" empty-interval (cons 0 (to-staff-space amount))))

(define-markup-command (annotate-moving layout props arg)
  (markup?)
  #:category other
  #:properties ((color "red")
                (size 1))
  "
@cindex annotate moving by spacing, in text

Indicate @code{\\vspace} and @code{\\hspace} movement with an arrow.

The arrow changes its size and thickness depending on the printed length;
the maximum size of the arrow head can be controlled with the @code{size}
property.  If @code{size} exceeds a third of the length of the final arrow,
it falls back to that third.

Note that the arrows do not reflect the actual extents of the objects created
by @code{\\vspace} and @code{\\hspace}; you might use @code{\\box} for that.

@lilypond[verbatim,quote]
\\markup
  \\column {
    \\line { left \\annotate-moving \\hspace #4 right }
    \\line { left \\annotate-moving \\hspace #-4 right }
    \\line {
      \\column {
        top \\override #'(size . 0.6) \\annotate-moving \\vspace #4/3 bottom
      }
      \\column {
        top \\override #'(size . 2.0) \\annotate-moving \\vspace #-4/3 bottom
      }
    }
  }
@end lilypond
"
  (let* ((arg-stil (interpret-markup layout props arg))
         (x-ext (ly:stencil-extent arg-stil X))
         (y-ext (ly:stencil-extent arg-stil Y))
         (v-space? (ly:stencil-empty? arg-stil X))
         (h-space? (ly:stencil-empty? arg-stil Y))
         (moving-arrows (arrow-stencil-maker #f #t)))
    (cond
      ((and v-space? (not (zero? (- (cdr y-ext) (car y-ext)))))
        (let* ((v-arrow
                 (stencil-with-color
                   (moving-arrows (cons 0.0 (- (cdr y-ext))) size)
                   color))
               (final-v-arrow (ly:stencil-outline v-arrow arg-stil)))
          (ly:stencil-add arg-stil final-v-arrow)))
      ((and h-space? (not (zero? (- (cdr x-ext) (car x-ext)))))
        (let* ((h-arrow
                 (stencil-with-color
                   (moving-arrows (cons (cdr x-ext) 0.0) size)
                   color))
               (final-h-arrow (ly:stencil-outline h-arrow arg-stil)))
          (ly:stencil-add arg-stil final-h-arrow)))
      (else arg-stil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; importing graphics.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-markup-command (stencil layout props stil)
  (ly:stencil?)
  #:category other
  "
@cindex importing stencil, into text

Use stencil @var{stil} as markup.

@lilypond[verbatim,quote]
\\markup {
  \\stencil #(make-circle-stencil 2 0 #t)
}
@end lilypond"
  stil)

(define bbox-regexp
  (ly:make-regex "%%BoundingBox:[ \t]+([0-9-]+)[ \t]+([0-9-]+)[ \t]+([0-9-]+)[ \t]+([0-9-]+)"))

(define-public (get-postscript-bbox string)
  "Extract the bounding box from @var{string}, or return @code{#f} if not
present."
  (let*
      ((match (ly:regex-exec bbox-regexp string)))

    (if match
        (map (lambda (x)
               (string->number (ly:regex-match-substring match x)))
             (cdr (iota 5)))

        #f)))

(define-markup-command (epsfile layout props axis size file-name)
  (number? number? string?)
  #:category graphic
  #:as-string ""
  "Inline an image @var{file-name}, scaled along @var{axis} to @var{size}.

See @code{\\image} for details on this command; calling

@example
\\markup \\epsfile @var{axis} @var{size} @var{file-name}
@end example

@noindent
is the same as

@example
\\markup
  \\override #'(background-color . #f)
  \\image @var{axis} @var{size} @var{file-name}
@end example"
  (interpret-markup layout props
                    (make-override-markup
                     '(background-color . #f)
                     (make-image-markup axis size file-name))))

(define-markup-command (postscript layout props str)
  (string?)
  #:category graphic
  #:as-string ""
  "
@cindex inserting PostScript directly, into text
Insert @var{str} directly into the output as a PostScript
command string.

This command is meant as a @emph{last resort}.  Almost all needs are better
fulfilled by other markup commands (see, for example, @code{\\path} and
@code{\\draw-line}).  If you do use this command, keep the following points in
mind:

@itemize
@item @code{\\postscript} does not work in SVG output.

@item Only a subset of the PostScript language is supported during the
conversion from PostScript to PDF.

@item There are no stability guarantees on the details of how LilyPond produces
its own output (i.e., the context into which the PostScript code is inserted).
They may change substantially across versions.

@item LilyPond cannot understand the shape of the drawing, leading to suboptimal
spacing.  Usually, it is necessary to explicitly set up dimensions with a
command like @code{\\with-dimensions}.

@item Depending on how you install LilyPond, the version of the PostScript
interpreter (Ghostscript) can vary, and some of its features may be disabled.
@end itemize

@var{str} is processed with the following constraints.

@itemize
@item The string is embedded into the (intermediate) output file with the
PostScript commands

@example
gsave currentpoint translate 0.1 setlinewidth
@end example

@noindent
before and

@example
grestore
@end example

@noindent
after it.

@item After these preceding commands (i.e., @code{currentpoint translate}) the
origin of the current transformation is the reference point of
@code{\\postscript}.  Scale and rotation of the current transformation reflect
the global staff line distance and (if applied) other transformation markup
commands (e.g., @code{\\scale} and @code{\\rotate}) encapsulating the
@code{\\postscript} command.

@item The current point is set to the coordinate (0,@tie{}0).

@item If an unwanted line appears at the beginning of your PostScript code, you
are probably missing a call to @code{newpath}.
@end itemize

@lilypond[verbatim,quote]
ringsps = \"
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
         (format #f "
gsave currentpoint translate
0.1 setlinewidth
 ~a
grestore
"
                 str))
   '(0 . 0) '(0 . 0)))

(define-markup-command (path layout props thickness commands) (number? list?)
  #:category graphic
  #:properties ((line-cap-style 'round)
                (line-join-style 'round)
                (filled #f))
  "
@cindex path, drawing
@cindex drawing path
Draw a path with line @var{thickness} according to the
directions given in @var{commands}.

@var{commands} is a list of lists where the @code{car} of each sublist is a
drawing command and the @code{cdr} comprises the associated arguments for each
command.

There are seven commands available to use in
@var{commands}: @code{moveto}, @code{rmoveto}, @code{lineto},
@code{rlineto}, @code{curveto}, @code{rcurveto}, and
@code{closepath}.  Note that the commands that begin with @samp{r}
are the relative variants of the other three commands.  You may also
use the standard SVG single-letter equivalents: @code{moveto} = @code{M},
@code{lineto} = @code{L}, @code{curveto} = @code{C},
@code{closepath} = @code{Z}.  The relative commands are written
lowercase: @code{rmoveto} = @code{r}, @code{rlineto} = @code{l},
@code{rcurveto} = @code{c}.

The commands @code{moveto}, @code{rmoveto}, @code{lineto}, and
@code{rlineto} take 2@tie{}arguments, namely the X and Y@tie{}coordinates
of the destination point.

The commands @code{curveto} and @code{rcurveto} create cubic
Bézier curves, and take 6@tie{}arguments; the first two are the X and
Y@tie{}coordinates for the first control point, the second two are the X
and Y@tie{}coordinates for the second control point, and the last two
are the X and Y@tie{}coordinates for the destination point.

The @code{closepath} command takes zero arguments and closes the
current subpath in the active path.

Line-cap styles and line-join styles may be customized by
overriding the @code{line-cap-style} and @code{line-join-style}
properties, respectively.  Available line-cap styles are
@code{butt}, @code{round}, and @code{square}.  Available
line-join styles are @code{miter}, @code{round}, and
@code{bevel}.

The property @code{filled} specifies whether or not the path is
filled with color.

@lilypond[verbatim,quote]
samplePath =
  #'((lineto -1 1)
     (lineto 1 1)
     (lineto 1 -1)
     (curveto -5 -5 -5 5 -1 0)
     (closepath))

\\markup \\scale #'(2 . 2) {
  \\path #0.25 #samplePath

  \\override #'(line-join-style . miter)
  \\path #0.25 #samplePath

  \\override #'(filled . #t)
  \\path #0.25 #samplePath
}
@end lilypond"
  ;; FIXME: discrepancy between nested lists '((moveto x y) (lineto z t) ...)
  ;; in \path and flat lists '(moveto x y lineto z t ...) in make-path-stencil.
  (make-path-stencil (apply append commands)
                     thickness
                     1
                     1
                     filled
                     #:line-cap-style line-cap-style
                     #:line-join-style line-join-style))

(define-markup-command (image layout props axis size file-name)
  (number? number? string?)
  #:category graphic
  #:properties ((background-color "white"))
  #:as-string ""
  "
@cindex image
@cindex markup, image
@cindex PNG image
@cindex EPS image

Inline an image @var{file-name}, scaled along @var{axis} to @var{size}.

The image format is determined based on the extension of @var{file-name}, which
should be @file{.png} for a PNG image, or @file{.eps} for an EPS
image (@file{.PNG} and @file{.EPS} are allowed as well).

EPS files are only supported in the PostScript backend -- for all output
formats@tie{}--, and in the Cairo backend -- when creating PostScript or EPS
output.

If the image has transparency, it will appear over a colored background with
the color specified by the @code{background-color} property, which defaults to
@code{\"white\"}.

To disable the colored background, set @code{background-color} to @code{#f}.
For EPS images, this always works (where EPS images work in the first place).
On the other hand, for PNG images, it works in the Cairo backend (which can
output all supported formats), as well as in the SVG backend, but @emph{not} in
the PostScript backend, which is the default.  See @rprogram{Advanced
command-line options for LilyPond} for how to activate the Cairo backend.

Use @code{\\withRelativeDir} as a prefix to @var{file-name} if the file should
be found relative to the input file."
  ;; From the width and height of the actual image plus one of the scaled
  ;; dimensions specified by the user, determine the scaling factor from image
  ;; coordinates to page coordinates, and the two dimensions of the stencil.
  (define (calc-factor width height)
    (let ((bbox-size (if (= axis X)
                         width
                         height)))
      (if (< 0 bbox-size)
          (let ((factor (exact->inexact (/ size bbox-size))))
            ;; Stencil starts at (0, 0), even for an EPS image whose bounding
            ;; box does not start at (0, 0).
            (values factor
                    (cons 0 (* width factor))
                    (cons 0 (* height factor))))
          (values 0 0 0))))
  (let-escape-continuation
   escape-and-return
   (let* ((file-name (let ((it (ly:find-file file-name #|strict|# #t)))
                       (ly:note-extra-source-file it)
                       it))
          (format
           (cond
            ((string-endswith file-name ".png" #:case-insensitive #t)
             'png)
            ((string-endswith file-name ".eps" #:case-insensitive #t)
             'eps)
            (else
             (ly:warning
              (G_ "unrecognized image file, should have .png or .eps extension: ~a")
              file-name)
             (escape-and-return empty-stencil))))
          (background-color (and=> background-color normalize-color)))
     (case format
       ((eps)
        (let ((contents (ly:gulp-file file-name)))
          (match (get-postscript-bbox (car (string-split contents #\nul)))
            (#f
             (ly:warning
              (G_ "EPS image dimensions could not be determined; \
is '~a' a valid EPS file?")
              file-name)
             (escape-and-return empty-stencil))
            ((and bbox (x1 y1 x2 y2))
             (let ((width (- x2 x1))
                   (height (- y2 y1)))
               (receive (factor real-width real-height)
                   (calc-factor width height)
                 (let* ((expr `(eps-file ,file-name ,contents ,bbox ,factor))
                        (basic-stencil (ly:make-stencil expr real-width real-height)))
                   (if background-color
                       (ly:stencil-add
                        (stencil-with-color
                         (make-filled-box-stencil real-width real-height)
                         background-color)
                        basic-stencil)
                       basic-stencil))))))))
       ((png)
        (match (ly:png-dimensions file-name)
          (#f
           ;; No warning: assume ly:png-dimensions already complained.
           (escape-and-return empty-stencil))
          ((width . height)
           (receive (factor real-width real-height)
               (calc-factor width height)
             (let ((expr `(png-file ,file-name ,width ,height
                                    ,factor ,background-color)))
               (ly:make-stencil expr real-width real-height))))))))))

(define-markup-list-command (score-lines layout props score)
  (ly:score?)
  #:properties ((tags-to-keep '())
                (tags-to-remove '())
                (tags-with-pushes-alist '())
                (tags-with-appends-alist '()))
  "Inline an image of music as specified by @var{score}.

Like @code{\\score} but return a list of lines instead of a single markup."
  (define (apply-tag-additions tags-with-additions-alist music folder tag-applier)
    (if (pair? tags-with-additions-alist)
      (let ((tag (caar tags-with-additions-alist))
            (additions (cdar tags-with-additions-alist)))
        (apply-tag-additions
          (alist-delete tag tags-with-additions-alist eq?)
          (folder
            (lambda (addition mus)
              (tag-applier tag addition mus))
            music
            additions)
          folder
          tag-applier))
      music))

  (define music (ly:score-music score))
  (set! music
    (fold
      (lambda (keep-tags mus) #{ \keepWithTag #keep-tags #mus #})
      music
      tags-to-keep))
  (if (pair? tags-to-remove)
    (set! music #{ \removeWithTag #tags-to-remove #music #}))
  (set! music
    (apply-tag-additions
      tags-with-pushes-alist
      music
      fold-right
      (lambda (tag addition mus)
        #{ \pushToTagMarkup #tag #addition #mus #})))
  (set! music
    (apply-tag-additions
      tags-with-appends-alist
      music
      fold
      (lambda (tag addition mus)
        #{ \appendToTagMarkup #tag #addition #mus #})))

  (let ((output (ly:score-embedded-format score layout)))

    (if (ly:music-output? output)
        (map
         (lambda (paper-system)
           ;; shift such that the refpoint of the bottom staff of
           ;; the first system is the baseline of the score
           (ly:stencil-translate-axis
            (paper-system-stencil paper-system)
            (- (car (paper-system-staff-extents paper-system)))
            Y))
         (vector->list (ly:paper-score-paper-systems output)))
        (begin
          (ly:warning (G_ "no systems found in \\score markup, does it have a \\layout block?"))
          '()))))

(define-markup-command (score layout props score)
  (ly:score?)
  #:category music
  #:properties ((baseline-skip))
  "
@cindex inserting music, into text

@anchor{Scores within markup}
Inline an image of music as specified by @var{score}.

The reference point (usually the middle staff line) of the lowest staff in the
top system is placed on the baseline.

No page breaks and no MIDI output, i.e., both @code{\\pageBreak} commands and
@code{\\midi@{@}} blocks get ignored.

@lilypond[verbatim,quote,line-width=14\\cm,staffsize=16]
\\markup {
  Text before the score.
  \\score {
    \\new PianoStaff <<
      \\new Staff \\relative c' {
        \\key f \\major
        \\time 3/4
        \\mark \\markup { Allegro }
        f2\\p( a4)
      }
      \\new Staff \\relative c {
        \\clef bass
        \\key f \\major
        \\time 3/4
        f8( a c a c a
      }
    >>

    \\layout {
      indent = 0.0\\cm
    }
  }
  Text after the score.
}
@end lilypond"
  (stack-stencils Y DOWN baseline-skip
                  (score-lines-markup-list layout props score)))

(define-markup-command (rhythm layout props music)
  (ly:music?)
  #:category music
  #:properties ((font-size -2))
  "
@cindex rhythm, in text
@cindex markup, rhythm
@cindex swing
@cindex tempo, with rhythm

Draw embedded rhythmic pattern as specified by @var{music}.

@lilypond[verbatim,quote]
\\relative {
  \\tempo \\markup {
    Swing
    \\hspace #0.4
    \\rhythm { 8[ 8] } = \\rhythm { \\tuplet 3/2 { 4 8 } }
  }
  b8 g' c, d ees d16 ees d c r8
}
@end lilypond

Within @code{\\rhythm}, there is no time signature and no division in measures
(as with @code{\\cadenzaOn}, @pxref{Unmetered music}).  Beaming must
be added explicitly with the syntax explained in @ref{Manual beams}.

@lilypond[verbatim,quote]
\\markup {
  The rhythmic pattern \\rhythm { 16[ 8 16] } is
  a type of syncopation.
}
@end lilypond

@code{\\stemDown} can be used to flip the stems.

@lilypond[verbatim,quote]
\\markup \\rhythm { \\stemDown 8 16 8 }
@end lilypond

@code{\\rhythm} works by creating a @code{StandaloneRhythmVoice} context,
which is enclosed in a @code{StandaloneRhythmStaff} context, which is
enclosed in a @code{StandaloneRhythmScore} context. It is possible to
apply global tweaks to the output by using a @code{\\layout} block.

@lilypond[verbatim,quote]
\\layout {
  \\context {
    \\StandaloneRhythmVoice
    \\xNotesOn
  }
}

\\markup \\rhythm { 8 16 8 }
@end lilypond
"
  (let* ((mkup
          #{
            \markup \score {
              \layout {
                system-count = 1
                ragged-right = ##t
              }
              \new StandaloneRhythmVoice \with {
                \magnifyStaff #(magstep font-size)
              }
              { #music }
            }
          #})
         (stil (interpret-markup layout props mkup)))
    (ly:stencil-aligned-to stil X LEFT)))


(define-markup-command (null layout props)
  ()
  #:category other
  "
@cindex creating empty text object

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

(define-markup-command (simple layout props str)
  (string?)
  #:category font
  "Print string @var{str}.

@code{\\markup \\simple \"x\"} is equivalent to @code{\\markup \"x\"}.  This
command was previously used internally, but no longer is, and is being kept for
backward compatibility only."
  (interpret-markup layout props str))

(define-markup-command (first-visible layout props args)
  (markup-list?)
  #:category other
  #:as-string "" ;; TODO: should we do something here?
  "Use the first markup in @var{args} that yields a non-empty stencil
and ignore the rest.

@lilypond[verbatim,quote]
\\markup {
  \\first-visible {
    \\fromproperty #'header:composer
    \\italic Unknown
  }
}
@end lilypond"
  (define (false-if-empty stencil)
    (if (ly:stencil-empty? stencil) #f stencil))
  (or
   (any
    (lambda (m)
      (if (markup? m)
          (false-if-empty (interpret-markup layout props m))
          (any false-if-empty (interpret-markup-list layout props (list m)))))
    args)
   empty-stencil))

(define-public empty-markup "")

;; helper for justifying lines.
(define (get-fill-spaces
         word-count line-width word-space text-widths constant-space?)
  "Calculate paddings between adjacent texts for a single, justified line.

The lengths of all texts are stored in the list TEXT-WIDTHS; the number of
elements is WORD-COUNT.  When CONSTANT_SPACE? is #t, the formula for the padding
between texts is

  padding = (line-width - total-text-width) / (word-count - 1) .

Otherwise the formula for the padding between two successive texts A and B is

  padding = line-width / (word-count - 1) - (length(a) + length(b)) / 2 ,

with the first and last padding calculated specially so that the first text
element is left-aligned and and last element right-aligned.

The minimum padding value is WORD-SPACE to ensure that no texts collide.  This
function returns a list of paddings."
  (define (get-spaces word-count width text-widths)
    (cond
     ;; First padding.
     ((= (length text-widths) word-count)
      (cons (- width (car text-widths) (/ (cadr text-widths) 2))
            (get-spaces word-count width (cdr text-widths))))
     ;; Last padding.
     ((= (length text-widths) 2)
      (list (- width (/ (car text-widths) 2) (cadr text-widths))))
     ;; Padding inbetween.
     (else
      (cons (- width (/ (+ (car text-widths) (cadr text-widths)) 2))
            (get-spaces word-count width (cdr text-widths))))))

  (cond
   ((null? text-widths) '())
   (constant-space?
    (make-list
     (1- word-count)
     (max word-space
          (/ (- line-width (apply + text-widths)) (1- word-count)))))
   (else
    (let* ((width (/ line-width (1- word-count)))
           (fill-spaces (get-spaces word-count width text-widths)))
      (map (lambda (fs) (max word-space fs)) fill-spaces)))))

(define (justify-line-helper
         layout props args text-direction word-space line-width constant-space?)
  "Return a stencil which spreads @var{args} along a line of width
@var{line-width}.  If @var{constant-space?} is set to @code{#t}, the
space between words is constant.  If @code{#f}, the distance between
words varies according to their relative lengths."
  (let* ((orig-stencils (interpret-markup-list layout props args))
         (stencils
          (map (lambda (stc)
                 (if (ly:stencil-empty? stc X)
                     (ly:make-stencil (ly:stencil-expr stc)
                                      '(0 . 0) (ly:stencil-extent stc Y))
                     stc))
               orig-stencils))
         (text-widths
          (map (lambda (stc)
                 (interval-length (ly:stencil-extent stc X)))
               stencils))
         (text-width (apply + text-widths))
         (word-count (length stencils))
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
            (get-fill-spaces
             word-count line-width word-space text-widths
             constant-space?))))
         (line-contents (if (= word-count 1)
                            (list
                             point-stencil
                             (car stencils)
                             point-stencil)
                            stencils)))

    (if (null? (remove ly:stencil-empty? orig-stencils))
        empty-stencil
        (begin
          (if (= text-direction LEFT)
              (set! line-contents (reverse line-contents)))
          (set! line-contents
                (stack-stencils-padding-list
                 X RIGHT fill-space line-contents))
          (if (> word-count 1)
              ;; shift s.t. stencils align on the left edge, even if
              ;; first stencil had negative X-extent (e.g. center-column)
              ;; (if word-count = 1, X-extents are already normalized in
              ;; the definition of line-contents)
              (set! line-contents
                    (ly:stencil-translate-axis
                     line-contents
                     (- (car (ly:stencil-extent (car stencils) X)))
                     X)))
          line-contents))))

(define-markup-command (fill-line layout props args)
  (markup-list?)
  #:category align
  #:properties ((text-direction RIGHT)
                (word-space 0.6)
                (line-width #f))
  "Put markups @var{args} into a horizontal line at fixed positions.

If there is a single argument, it gets centered.  If there are two arguments,
they get aligned to the left and right, respectively.  Otherwise, if there are
@var{n}@tie{}arguments, the markups are aligned to @var{n}@tie{}equally spaced
positions, with the first markup left-aligned, the last markup right-aligned,
and the remaining markups centered at the respective positions.  If there are no
arguments, return an empty stencil.

The width of the horizontal line can be modified by overriding the
@code{line-width} property.  The space between arguments is at least as large as
the value of the @code{word-space} property, at the cost of lengthening the line
if necessary.

@lilypond[verbatim,quote,line-width=14\\cm]
\\markup {
  \\column {
    \\fill-line { Words positioned evenly across a line }
    \\fill-line { | | | | | | }
    \\null
    \\fill-line {
      \\line { Text markups }
      \\line { \\italic { positioned evenly } }
      \\line { across a line }
    }
    \\null
    \\override #'(line-width . 50)
    \\fill-line { Width explicitly specified }
  }
}
@end lilypond

See also @code{\\justify-line}."
  (justify-line-helper
   layout props args text-direction word-space line-width #f))

(define-markup-command (justify-line layout props args)
  (markup-list?)
  #:category align
  #:properties ((text-direction RIGHT)
                (word-space 0.6)
                (line-width #f))
  "Put markups @var{args} into a horizontal line, equally spaced.

If there is a single argument, it gets centered.  If there are two arguments,
they get aligned to the left and right, respectively.  Otherwise, the markups
are spread to fill the entire line, separated by equally large spaces.  If there
are no arguments, return an empty stencil.

The width of the horizontal line can be modified by overriding the
@code{line-width} property.  The space between arguments is at least as large as
the value of the @code{word-space} property, at the cost of lengthening the line
if necessary.

@lilypond[verbatim,quote,line-width=14\\cm]
\\markup {
  \\justify-line {
    Constant space between neighboring words
  }
}
@end lilypond

See also @code{\\fill-line}."
  (justify-line-helper
   layout props args text-direction word-space line-width #t))

(define-markup-command (concat layout props args)
  (markup-list?)
  #:category align
  ;; TODO: do we really want no spaces?  \overlay or \combine will
  ;; return a string with spaces.
  #:as-string (apply
               string-append
               (map (lambda (m)
                      (markup->string m #:layout layout #:props props))
                    args))
  "
@cindex concatenating text
@cindex ligature, in text

Concatenate @var{args} in a horizontal line, without spaces in between.

Strings are concatenated on the input level, allowing ligatures.
For example, @code{\\concat @{ \"f\" \"i\" @}} is
equivalent to @code{\"fi\"}.

@lilypond[verbatim,quote]
\\markup {
  \\concat {
    one two three
  }
}
@end lilypond"
  (define (concat-string-args arg-list)
    (fold-right (lambda (arg result-list)
                  (let ((result (and (pair? result-list)
                                     (car result-list))))
                    (cond ((not (pair? arg)))
                          ((eq? (car arg) simple-markup)
                           (set! arg (cadr arg)))
                          ((eq? (car arg) char-markup)
                           (set! arg (ly:wide-char->utf-8 (cadr arg)))))
                    (if (and (string? result) (string? arg))
                        (cons (string-append arg result) (cdr result-list))
                        (cons arg result-list))))
                '()
                arg-list))
  (stack-stencil-line 0
                      (interpret-markup-list layout props
                                             (if (markup-command-list? args)
                                                 args
                                                 (concat-string-args args)))))

(define (wordwrap-stencils stencils
                           justify base-space line-width text-dir)
  "Perform simple word-wrap, return stencil of each line."
  (define space (if justify
                    ;; justify only stretches lines.
                    (* 0.7 base-space)
                    base-space))
  (define (stencil-len s)
    (interval-end (ly:stencil-extent s X)))
  (define (maybe-shift line)
    (if (= text-dir LEFT)
        (ly:stencil-translate-axis
         line
         (- line-width (stencil-len line))
         X)
        line))
  (if (null? stencils)
      '()
      (let loop ((lines '())
                 (todo stencils))
        (let word-loop
            ((line (first todo))
             (todo (cdr todo))
             (word-list (list (first todo))))
          (cond
           ((pair? todo)
            (let ((new (if (= text-dir LEFT)
                           (ly:stencil-stack (car todo) X RIGHT line space)
                           (ly:stencil-stack line X RIGHT (car todo) space))))
              (cond
               ((<= (stencil-len new) line-width)
                (word-loop new (cdr todo)
                           (cons (car todo) word-list)))
               (justify
                (let* ((word-list
                        ;; This depends on stencil stacking being
                        ;; associative so that stacking
                        ;; left-to-right and right-to-left leads to
                        ;; the same result
                        (if (= text-dir LEFT)
                            word-list
                            (reverse! word-list)))
                       (len (stencil-len line))
                       (stretch (- line-width len))
                       (spaces
                        (- (stencil-len
                            (stack-stencils X RIGHT (1+ space) word-list))
                           len)))
                  (if (zero? spaces)
                      ;; Uh oh, nothing to fill.
                      (loop (cons (maybe-shift line) lines) todo)
                      (loop (cons
                             (stack-stencils X RIGHT
                                             (+ space (/ stretch spaces))
                                             word-list)
                             lines)
                            todo))))
               (else ;; not justify
                (loop (cons (maybe-shift line) lines) todo)))))
           ;; todo is null
           (justify
            ;; Now we have the last line assembled with space
            ;; which is compressed.  We want to use the
            ;; uncompressed version instead if it fits, and the
            ;; justified version if it doesn't.
            (let* ((word-list
                    ;; This depends on stencil stacking being
                    ;; associative so that stacking
                    ;; left-to-right and right-to-left leads to
                    ;; the same result
                    (if (= text-dir LEFT)
                        word-list
                        (reverse! word-list)))
                   (big-line (stack-stencils X RIGHT base-space word-list))
                   (big-len (stencil-len big-line))
                   (len (stencil-len line)))
              (reverse! lines
                        (list
                         (if (> big-len line-width)
                             (stack-stencils X RIGHT
                                             (/
                                              (+
                                               (* (- big-len line-width)
                                                  space)
                                               (* (- line-width len)
                                                  base-space))
                                              (- big-len len))
                                             word-list)
                             (maybe-shift big-line))))))
           (else ;; not justify
            (reverse! lines (list (maybe-shift line)))))))))


(define-markup-list-command (wordwrap-internal layout props justify args)
  (boolean? markup-list?)
  #:properties ((line-width #f)
                (word-space)
                (text-direction RIGHT))
  #:internal? #t
  (wordwrap-stencils (interpret-markup-list layout props args)
                     justify
                     word-space
                     (or line-width
                         (ly:output-def-lookup layout 'line-width))
                     text-direction))

(define-markup-command (justify layout props args)
  (markup-list?)
  #:category align
  #:properties ((baseline-skip)
                wordwrap-internal-markup-list)
  "
@cindex justifying text

Print @var{args} as lines aligned both at the left and the right.

Like @code{\\wordwrap}, but with lines stretched to justify the margins.
Use @code{\\override #'(line-width . @var{X})} to set the line width;
@var{X}@tie{}is the number of staff spaces.

@lilypond[verbatim,quote,line-width=14\\cm]
\\markup {
  \\justify {
    Lorem ipsum dolor sit amet, consectetur adipisicing elit,
    sed do eiusmod tempor incididunt ut labore et dolore
    magna aliqua.  Ut enim ad minim veniam, quis nostrud
    exercitation ullamco laboris nisi ut aliquip ex ea
    commodo consequat.
  }
}
@end lilypond

The baseline of the output of @code{\\justify} is the baseline of its first
line."
  (stack-lines DOWN 0.0 baseline-skip
               (wordwrap-internal-markup-list layout props #t args)))

(define-markup-command (wordwrap layout props args)
  (markup-list?)
  #:category align
  #:properties ((baseline-skip)
                wordwrap-internal-markup-list)
  "Print @var{args} as left-aligned lines.

This function provides simple word-wrap.  Use @code{\\override #'(line-width
. @var{X})} to set the line width; @var{X}@tie{}is the number of staff spaces.

@lilypond[verbatim,quote,line-width=14\\cm]
\\markup {
  \\wordwrap {
    Lorem ipsum dolor sit amet, consectetur adipisicing elit,
    sed do eiusmod tempor incididunt ut labore et dolore
    magna aliqua.  Ut enim ad minim veniam, quis nostrud
    exercitation ullamco laboris nisi ut aliquip ex ea
    commodo consequat.
  }
}
@end lilypond

The baseline of the output of @code{\\wordwrap} is the baseline of its first
line."
  (stack-lines DOWN 0.0 baseline-skip
               (wordwrap-internal-markup-list layout props #f args)))

(define cr-regex (ly:make-regex "\r"))
(define crlf-regex (ly:make-regex "\r\n"))
(define para-sep-regex (ly:make-regex "\n[ \t\n]*\n[ \t\n]*"))
(define whitespace-regex (ly:make-regex "[ \t\n]+")) ;; should use "\\s+" ?
(define-markup-list-command (wordwrap-string-internal layout props justify arg)
  (boolean? string?)
  #:properties ((line-width)
                (word-space)
                (text-direction RIGHT))
  #:internal? #t
  (let* ((para-strings (ly:regex-split
                        para-sep-regex
                        (ly:regex-replace
                         cr-regex
                         (ly:regex-replace crlf-regex arg "\n")
                         "\n")))
         (list-para-words (map (lambda (str)
                                 (ly:regex-split whitespace-regex str))
                               para-strings))
         (para-lines (map (lambda (words)
                            (let* ((stencils
                                    (map (lambda (x)
                                           (interpret-markup layout props x))
                                         words)))
                              (wordwrap-stencils stencils
                                                 justify word-space
                                                 line-width text-direction)))
                          list-para-words)))
    (concatenate para-lines)))

(define-markup-command (wordwrap-string layout props arg)
  (string?)
  #:category align
  #:properties ((baseline-skip)
                wordwrap-string-internal-markup-list)
  "Print string @var{arg} as left-aligned lines.

Paragraphs are indicated by double newlines.  Use @code{\\override #'(line-width
. @var{X})} to set the line width; @var{X}@tie{}is the number of staff spaces.

@lilypond[verbatim,quote]
\\markup {
  \\override #'(line-width . 40)
  \\wordwrap-string \"Lorem ipsum dolor sit amet,
      consectetur adipisicing elit, sed do eiusmod tempor
      incididunt ut labore et dolore magna aliqua.


      Ut enim ad minim veniam, quis nostrud exercitation
      ullamco laboris nisi ut aliquip ex ea commodo
      consequat.


      Excepteur sint occaecat cupidatat non proident,
      sunt in culpa qui officia deserunt mollit anim id
      est laborum\"
}
@end lilypond

The baseline of the output of @code{\\wordwrap-string} is the baseline of its
first line."
  (stack-lines DOWN 0.0 baseline-skip
               (wordwrap-string-internal-markup-list layout props #f arg)))

(define-markup-command (justify-string layout props arg)
  (string?)
  #:category align
  #:properties ((baseline-skip)
                wordwrap-string-internal-markup-list)
  "Print string @var{arg} as lines aligned both at the left and the right.

Paragraphs are indicated by double newlines.  Use @code{\\override #'(line-width
. @var{X})} to set the line width; @var{X}@tie{}is the number of staff spaces.

@lilypond[verbatim,quote]
\\markup {
  \\override #'(line-width . 40)
  \\justify-string \"Lorem ipsum dolor sit amet, consectetur
      adipisicing elit, sed do eiusmod tempor incididunt ut
      labore et dolore magna aliqua.


      Ut enim ad minim veniam, quis nostrud exercitation
      ullamco laboris nisi ut aliquip ex ea commodo
      consequat.


      Excepteur sint occaecat cupidatat non proident, sunt
      in culpa qui officia deserunt mollit anim id est
      laborum\"
}
@end lilypond

The baseline of the output of @code{\\justify-string} is the baseline of its
first line."
  (stack-lines DOWN 0.0 baseline-skip
               (wordwrap-string-internal-markup-list layout props #t arg)))

(define-markup-command (wordwrap-field layout props symbol)
  (symbol?)
  #:category align
  #:as-string (markup->string (chain-assoc-get symbol props)
                              #:layout layout
                              #:props props)
  "Word-wrap the data that has been assigned to @var{symbol}.

@lilypond[verbatim,quote,line-width=14\\cm]
\\header {
  title = \"My title\"
  myText = \"Lorem ipsum dolor sit amet, consectetur
    adipisicing elit, sed do eiusmod tempor incididunt ut
    labore et dolore magna aliqua.  Ut enim ad minim
    veniam, quis nostrud exercitation ullamco laboris nisi
    ut aliquip ex ea commodo consequat.\"
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

(define-markup-command (justify-field layout props symbol)
  (symbol?)
  #:category align
  #:as-string (markup->string (chain-assoc-get symbol props)
                              #:layout layout
                              #:props props)
  "Justify the data that has been assigned to @var{symbol}.

@lilypond[verbatim,quote,line-width=14\\cm]
\\header {
  title = \"My title\"
  myText = \"Lorem ipsum dolor sit amet, consectetur
    adipisicing elit, sed do eiusmod tempor incididunt
    ut labore et dolore magna aliqua.  Ut enim ad minim
    veniam, quis nostrud exercitation ullamco laboris
    nisi ut aliquip ex ea commodo consequat.\"
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

(define-markup-command (combine layout props arg1 arg2)
  (markup? markup?)
  #:category align
  "
@cindex merging text

Print @var{arg1}, then print @var{arg2} on top of it.

Note: @code{\\combine} cannot take a list of markups enclosed in
curly braces as an argument; for this purpose use @code{\\overlay} instead.

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

(define-markup-command (overlay layout props args)
  (markup-list?)
  #:category align
  "
@cindex merging text

Take a list of markups @var{args} and combine them.

@lilypond[verbatim,quote]
\\markup {
  \\fontsize #5
  \\override #'(thickness . 2)
  \\overlay {
    \\draw-line #'(0 . 4)
    \\arrow-head #Y #DOWN ##f
    \\translate #'(0 . 4) \\arrow-head #Y #UP ##f
  }
}
@end lilypond"
  (apply ly:stencil-add (interpret-markup-list layout props args)))

;;
;; TODO: should extract baseline-skip from each argument somehow..
;;
(define-markup-command (column layout props args)
  (markup-list?)
  #:category align
  #:properties ((baseline-skip))
  "
@cindex stacking text, in a column

Stack the markups in @var{args} vertically.

The property @code{baseline-skip} determines the space between markups in
@var{args} (to be more precise, the space between the baselines of the markups).

@lilypond[verbatim,quote]
\\markup {
  \\column {
    one
    two
    three
  }
}
@end lilypond

The baseline of the output of @code{\\column} is the baseline of its first
line."
  (let ((arg-stencils (interpret-markup-list layout props args)))
    (stack-lines -1 0.0 baseline-skip arg-stencils)))

(define-markup-command (dir-column layout props args)
  (markup-list?)
  #:category align
  #:properties ((direction)
                (baseline-skip))
  "
@cindex changing direction of text column

Make a column of @var{args}.

Depending on the setting of the @code{direction} layout property, the arguments
are stacked upwards or downwards.

@lilypond[verbatim,quote]
\\markup {
  \\override #`(direction . ,UP)
  \\dir-column {
    going up
  }
  \\hspace #1
  \\dir-column {
    going down
  }
  \\hspace #1
  \\override #'(direction . 1)
  \\dir-column {
    going up
  }
}
@end lilypond

The baseline of the output of @code{\\dir-column} is the baseline of its first
line."
  (stack-lines (if (number? direction) direction -1)
               0.0
               baseline-skip
               (interpret-markup-list layout props args)))

(define (general-column align-dir baseline mols)
  "Stack @var{mols} vertically, aligned to @var{align-dir} horizontally."
  (let* ((aligned-mols
          (map (lambda (x) (ly:stencil-aligned-to x X align-dir)) mols))
         (stacked-stencil (stack-lines -1 0.0 baseline aligned-mols))
         (stacked-extent (ly:stencil-extent stacked-stencil X)))
    ;; empty stencils are not moved
    (if (interval-sane? stacked-extent)
        (ly:stencil-translate-axis
         stacked-stencil
         (- (car stacked-extent))
         X)
        stacked-stencil)))

(define-markup-command (center-column layout props args)
  (markup-list?)
  #:category align
  #:properties ((baseline-skip))
  "
@cindex centering column of text

Put @var{args} into a centered column.

See also @code{\\column}.

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

(define-markup-command (left-column layout props args)
  (markup-list?)
  #:category align
  #:properties ((baseline-skip))
  "
@cindex text column, left-aligned

Put @var{args} into a left-aligned column.

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

(define-markup-command (right-column layout props args)
  (markup-list?)
  #:category align
  #:properties ((baseline-skip))
  "
@cindex text column, right-aligned

Put @var{args} into a right-aligned column.

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

(define-markup-command (vcenter layout props arg)
  (markup?)
  #:category align
  "
@cindex vertically centering text

Align @var{arg} to its Y@tie{}center.

@lilypond[verbatim,quote]
\\markup {
  one
  \\vcenter two
  three
}
@end lilypond"
  (let* ((mol (interpret-markup layout props arg)))
    (ly:stencil-aligned-to mol Y CENTER)))

(define-markup-command (center-align layout props arg)
  (markup?)
  #:category align
  "
@cindex horizontally centering text

Align @var{arg} to its X@tie{}center.

@lilypond[verbatim,quote]
\\markup {
  \\column {
    one
    \\center-align two
    three
  }
}
@end lilypond"
  (let* ((mol (interpret-markup layout props arg)))
    (ly:stencil-aligned-to mol X CENTER)))

(define-markup-command (right-align layout props arg)
  (markup?)
  #:category align
  "
@cindex right-aligning text

Align @var{arg} on its right edge.

@lilypond[verbatim,quote]
\\markup {
  \\column {
    one
    \\right-align two
    three
  }
}
@end lilypond"
  (let* ((m (interpret-markup layout props arg)))
    (ly:stencil-aligned-to m X RIGHT)))

(define-markup-command (left-align layout props arg)
  (markup?)
  #:category align
  "
@cindex left-aligning text

Align @var{arg} on its left edge.

@lilypond[verbatim,quote]
\\markup {
  \\column {
    one
    \\left-align two
    three
  }
}
@end lilypond"
  (let* ((m (interpret-markup layout props arg)))
    (ly:stencil-aligned-to m X LEFT)))

(define-markup-command (general-align layout props axis dir arg)
  (integer? number? markup?)
  #:category align
  "
@cindex controlling general text alignment

Align @var{arg} in @var{axis} direction to the @var{dir} side.

@lilypond[verbatim,quote]
\\markup {
  \\column {
    one
    \\general-align #X #LEFT two
    three
    \\null
    one
    \\general-align #X #CENTER two
    three
    \\null
    \\line {
      one
      \\general-align #Y #UP two
      three
    }
    \\null
    \\line {
      one
      \\general-align #Y #3.2 two
      three
    }
  }
}
@end lilypond"
  (let* ((m (interpret-markup layout props arg)))
    (ly:stencil-aligned-to m axis dir)))

(define-markup-command (halign layout props dir arg)
  (number? markup?)
  #:category align
  "
@cindex setting horizontal text alignment

Print @var{arg} with horizontal alignment set to @var{dir}.

If @var{dir} is -1, @var{arg} is left-aligned, while +1 makes it right-aligned.
Values inbetween interpolate the alignment accordingly.

@lilypond[verbatim,quote]
\\markup {
  \\column {
    one
    \\halign #LEFT two
    three
    \\null
    one
    \\halign #CENTER two
    three
    \\null
    one
    \\halign #RIGHT two
    three
    \\null
    one
    \\halign #-5 two
    three
  }
}
@end lilypond"
  (let* ((m (interpret-markup layout props arg)))
    (ly:stencil-aligned-to m X dir)))

(define-markup-command (align-on-other layout props
                                       axis
                                       other-dir other
                                       self-dir self)
  (index? boolean-or-number? markup? boolean-or-number? markup?)
  #:category align
  "Align markup @var{self} on markup @var{other} along @var{axis}.

This function uses @var{self-@/dir} and @var{other-@/dir} for mutual alignment
of @var{self} and @var{other}, respectively, translating @var{self} as requested
relative to its surroundings.  @var{other} is not printed.

If @var{self-@/dir} or @var{other-@/dir} is @code{#f}, use the reference point
of @var{self} or @var{other}, respectively.

@lilypond[verbatim,quote]
\\markup \\column {
  12
  \\align-on-other #X #RIGHT 12
                     #LEFT 12345
  \\align-on-other #X #RIGHT 123
                     #LEFT \\fermata
  123
  \\align-on-other #X #RIGHT 123
                     ##f \\fermata
}
@end lilypond"
  (let* ((self-stil (interpret-markup layout props self))
         (self-ext (ly:stencil-extent self-stil axis))
         (self-offset (if (number? self-dir)
                          (interval-index self-ext self-dir)
                          0))
         (other-stil (interpret-markup layout props other))
         (other-ext (ly:stencil-extent other-stil axis))
         (other-offset (if (number? other-dir)
                           (interval-index other-ext other-dir)
                           0))
         (trans (- other-offset self-offset)))
    (ly:stencil-translate-axis self-stil trans axis)))

(define-markup-command (with-dimensions layout props x y arg)
  (number-pair? number-pair? markup?)
  #:category other
  "
@cindex setting extent of text object

Set the horizontal and vertical dimensions of @var{arg} to @var{x}
and@tie{}@var{y}."
  (ly:stencil-outline
   (interpret-markup layout props arg)
   (make-filled-box-stencil x y)))

(define-markup-command (with-dimension layout props axis val arg)
  (integer? number-pair? markup?)
  #:category other
  "
@cindex setting extent of text object

Set the dimension of @var{arg} along @var{axis} to @var{val}.

If @var{axis} is equal to@tie{}@code{X}, set the horizontal dimension.  If
@var{axis} is equal to@tie{}@code{Y}, set the vertical dimension."
  (let* ((stil (interpret-markup layout props arg))
         (x-orig (ly:stencil-extent stil X))
         (y-orig (ly:stencil-extent stil Y)))
    (ly:stencil-outline
     stil
     (if (= axis X)
         (make-filled-box-stencil val y-orig)
         (make-filled-box-stencil x-orig val)))))

(define-markup-command (with-outline layout props outline arg)
  (markup? markup?)
  #:category other
  #:as-string (markup->string arg #:layout layout #:props props)
  "
@cindex setting extent of text object

Print @var{arg} with the outline and dimensions of @var{outline}.

The outline is used by skylines to resolve collisions (not for whiteout)."
  (ly:stencil-outline (interpret-markup layout props arg)
                      (interpret-markup layout props outline)))

(define-markup-command (with-dimensions-from layout props arg1 arg2)
  (markup? markup?)
  #:category other
  #:as-string (markup->string arg2 #:layout layout #:props props)
  "
@cindex setting extent of text object

Print @var{arg2} with the horizontal and vertical dimensions of @var{arg1}."
  (let* ((stil1 (interpret-markup layout props arg1))
         (x (ly:stencil-extent stil1 0))
         (y (ly:stencil-extent stil1 1)))
    (interpret-markup layout props (make-with-dimensions-markup x y arg2))))

(define-markup-command (with-dimension-from layout props axis arg1 arg2)
  (integer? markup? markup?)
  #:category other
  #:as-string (markup->string arg2 #:layout layout #:props props)
  "
@cindex setting extent of text object

Print @var{arg2} but replace the dimension along @var{axis} with the one from
@var{arg1}.

If @var{axis} is set to@tie{}@code{X}, replace the horizontal dimension.  If
@var{axis} is set to@tie{}@code{Y}, replace the vertical dimension."
  (let* ((stil1 (interpret-markup layout props arg1))
         (stil2 (interpret-markup layout props arg2))
         (x (ly:stencil-extent (if (= axis X) stil1 stil2) X))
         (y (ly:stencil-extent (if (= axis X) stil2 stil1) Y)))
    (interpret-markup layout props (make-with-dimensions-markup x y arg2))))

(define-markup-command (with-true-dimension layout props axis arg)
  (integer? markup?)
  #:category other
  "
@cindex bounding box, of glyph
@cindex glyph, bounding box
@cindex dimension, of bounding box
@cindex extent, of bounding box
@cindex extent, of actual inking

Give @var{arg} its actual dimension (extent) on @var{axis}.

Sometimes, the extents of a markup's printed ink differs from the default
extents.  The main case is if glyphs are involved.  By default, the extents of a
glyph are based on the glyph's @dfn{metrics} (i.e., a default vertical and
horizontal size for the glyph), which, for various reasons, are often not
identical to its @dfn{bounding box} (i.e., the smallest rectangle that
completely encompasses the glyph's outline) -- in most cases, the outline
protrudes the box spanned up by the metrics.

@lilypond[verbatim,quote]
\\markup {
  text
  \\fontsize #10
  \\override #'((box-padding . 0) (thickness . 0.2))
  \\box
    \\musicglyph \"scripts.trill\"
  text
}
@end lilypond

For purposes other than setting text, this behavior may not be wanted.
You can use @code{\\with-@/true-@/dimension} in order to give the markup
its actual printed extent.

@lilypond[verbatim,quote]
\\markup {
  text
  \\fontsize #10
  \\override #'((box-padding . 0) (thickness . 0.2))
  \\box
    \\with-true-dimension #X
    \\musicglyph \"scripts.trill\"
  text
}
@end lilypond"
  (let* ((stencil (interpret-markup layout props arg))
         (true-ext (stencil-true-extent stencil axis))
         ;; Don't use ly:stencil-outline like \with-dimensions et al.:
         ;; the goal is to match the extents to the skyline, so we don't
         ;; want to make the skyline look like a box.
         (other-ext (ly:stencil-extent stencil (other-axis axis)))
         (expr (ly:stencil-expr stencil)))
    (if (eqv? axis X)
        (ly:make-stencil expr true-ext other-ext)
        (ly:make-stencil expr other-ext true-ext))))

(define-markup-command (with-true-dimensions layout props arg)
  (markup?)
  #:category other
  "Give @var{arg} its actual dimensions (extents).

Calling

@example
\\markup \\with-true-dimensions @var{arg}
@end example

@noindent
is short for

@example
\\markup
  \\with-true-dimension #X
  \\with-true-dimension #Y
  @var{arg}
@end example

@noindent
i.e., @code{\\with-@/true-@/dimensions} has the effect of
@code{\\with-@/true-@/dimension} on both axes."
  (interpret-markup
   layout
   props
   (make-with-true-dimension-markup
    X
    (make-with-true-dimension-markup
     Y
     arg))))

(define-markup-command (pad-around layout props amount arg)
  (number? markup?)
  #:category align
  "Add padding @var{amount} all around @var{arg}.

Identical to function @code{\\pad-markup}.

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
  (stencil-pad-around amount (interpret-markup layout props arg)))

(define-markup-command (pad-x layout props amount arg)
  (number? markup?)
  #:category align
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

(define-markup-command (put-adjacent layout props axis dir arg1 arg2)
  (integer? ly:dir? markup? markup?)
  #:category align
  ;; TODO: Do we really want no spaces?  \overlay or \combine
  ;; will return a string with spaces.
  #:as-string (string-append
               (markup->string arg1 #:layout layout #:props props)
               (markup->string arg2 #:layout layout #:props props))
  "Put @var{arg2} next to @var{arg1} along @var{axis} to the @var{dir} side,
without moving @var{arg1}.

@lilypond[verbatim,quote]
\\markup \\column {
  text
  \\put-adjacent #X #LEFT text *
  text
}
@end lilypond"
  (let ((m1 (interpret-markup layout props arg1))
        (m2 (interpret-markup layout props arg2)))
    (ly:stencil-combine-at-edge m1 axis dir m2 0.0)))

(define-markup-command (transparent layout props arg)
  (markup?)
  #:category other
  ;; It's not obvious how this should translate into strings.
  ;; This gives as many spaces as the string representation
  ;; of the argument.
  #:as-string (make-string
               (string-length
                (markup->string arg #:layout layout #:props props))
               #\space)
  "Make @var{arg} transparent.

@lilypond[verbatim,quote]
\\markup {
  \\transparent {
    invisible text
  }
}
@end lilypond"
  (ly:stencil-outline empty-stencil (interpret-markup layout props arg)))

(define-markup-command (pad-to-box layout props x-ext y-ext arg)
  (number-pair? number-pair? markup?)
  #:category align
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
  (ly:stencil-add (make-transparent-box-stencil x-ext y-ext)
                  (interpret-markup layout props arg)))

(define-markup-command (hcenter-in layout props length arg)
  (number? markup?)
  #:category align
  "Center @var{arg} horizontally within a box of extending
@var{length}/2 to the left and right.

@lilypond[verbatim,quote]
\\new StaffGroup <<
  \\new Staff {
    \\set Staff.instrumentName = \\markup {
      \\hcenter-in #12 Oboe
    }
    c''1
  }
  \\new Staff {
    \\set Staff.instrumentName = \\markup {
      \\hcenter-in #12 Bassoon
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

(define-markup-command (property-recursive layout props symbol)
  (symbol?)
  #:category other
  #:as-string (begin
                (ly:warning (G_ "Recursive definition of property ~a detected!")
                            symbol)
                "")
  "Print out a warning when header field markup in @var{symbol} contains some
recursive markup definition."
  (ly:warning (G_ "Recursive definition of property ~a detected!") symbol)
  empty-stencil)

(define-markup-command (fromproperty layout props symbol)
  (symbol?)
  #:category other
  #:as-string (markup->string
               (chain-assoc-get symbol props)
               #:layout layout
               #:props (prepend-alist-chain
                        symbol
                        (make-property-recursive-markup symbol)
                        props))
  "Read @var{symbol} from the property settings and produce a stencil
from the markup contained within.

If @var{symbol} is not defined or is not a markup, return an empty
markup.

Currently, the following properties can be accessed.

@itemize
@item
Within a @code{\\paper} block defining titles, headers, or
footers, or within a @code{\\header} block: all fields from the
@code{\\header} block (that produce markup) are available, with
@code{header:} as a name prefix.

@item
Within a @code{\\paper} block defining headers or footers: the
current page number (symbol @code{page:page-number-string}).

@item
Within the @code{tocItemMarkup} paper variable (or in custom-made
Scheme code that uses function @code{add-toc-item!}) defining a
table of contents entry: the entry's text and page number are
available as @code{toc:text} and @code{toc:page}, respectively.
An entry's indentation markup is available as @code{toc:indent}.
@end itemize

@lilypond[verbatim,quote,line-width=14\\cm]
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
        ;; prevent infinite loops by clearing the interpreted property:
        (interpret-markup layout
                          (prepend-alist-chain
                           symbol
                           (make-property-recursive-markup symbol)
                           props)
                          m)
        empty-stencil)))

(define-markup-command (on-the-fly layout props procedure arg)
  (procedure? markup?)
  #:category other
  "Apply the @var{procedure} markup command to @var{arg}.

@var{procedure} takes the same arguments as @code{interpret-markup}
and returns a stencil."
  (procedure layout props arg))

(define-markup-command (footnote layout props mkup note)
  (markup? markup?)
  #:category other
  #:as-string (format #f "~a [~a]"
                      (markup->string mkup #:layout layout #:props props)
                      (markup->string note #:layout layout #:props props))
  "Have footnote @var{note} act as an annotation to the markup @var{mkup}.

@c we don't want to display \\book
@example
\\markup @{
  \\footnote a b
  \\override #'(padding . 0.2)
  \\footnote c d
@}
@end example
@lilypond[quote,ragged-right,papersize=a8]
\\book {
  \\header { tagline = ##f }
  \\markup {
    \\footnote a b
    \\override #'(padding . 0.2)
    \\footnote c d
  }
}
@end lilypond

The footnote will not be annotated automatically."
  (ly:stencil-combine-at-edge
   (interpret-markup layout props mkup)
   X
   RIGHT
   (ly:make-stencil
    `(footnote (gensym "footnote") #f ,(interpret-markup layout props note))
    '(0 . 0)
    '(0 . 0))
   0.0))

(define-markup-command (auto-footnote layout props mkup note)
  (markup? markup?)
  #:category other
  #:properties ((raise 0.5)
                (padding 0.0))
  #:as-string (format #f "~a [~a]"
                      (markup->string mkup #:layout layout #:props props)
                      (markup->string note #:layout layout #:props props))
  "Have footnote @var{note} act as an annotation to the markup @var{mkup}.

@c we don't want to display \\book, and we also have to avoid incorrect
@c cropping
@example
\\markup @{
  \\auto-footnote a b
  \\override #'(padding . 0.2)
  \\auto-footnote c d
@}
@end example
@lilypond[quote,ragged-right,papersize=a8]
\\book {
  \\header { tagline = ##f }
  \\markup {
    \\vspace #0.6
    \\auto-footnote a b
    \\override #'(padding . 0.2)
    \\auto-footnote c d
  }
}
@end lilypond

The footnote will be annotated automatically."
  (let* ((markup-stencil (interpret-markup layout props mkup))
         (footnote-hash (gensym "footnote"))
         (stencil-seed 0)
         (gauge-stencil (interpret-markup
                         layout
                         props
                         ((ly:output-def-lookup
                           layout
                           'footnote-numbering-function)
                          stencil-seed)))
         (x-ext (ly:stencil-extent gauge-stencil X))
         (y-ext (ly:stencil-extent gauge-stencil Y))
         (footnote-number
          `(delay-stencil-evaluation
            ,(delay
               (ly:stencil-expr
                (let* ((table
                        (ly:output-def-lookup layout
                                              'number-footnote-table))
                       (footnote-stencil (if (list? table)
                                             (assoc-get footnote-hash
                                                        table)
                                             empty-stencil))
                       (footnote-stencil (if (ly:stencil? footnote-stencil)
                                             footnote-stencil
                                             (begin
                                               (ly:programming-error
                                                "Cannot find correct footnote for a markup object.")
                                               empty-stencil)))
                       (gap (- (interval-length x-ext)
                               (interval-length
                                (ly:stencil-extent footnote-stencil X))))
                       (y-trans (- (+ (cdr y-ext)
                                      raise)
                                   (cdr (ly:stencil-extent footnote-stencil
                                                           Y)))))
                  (ly:stencil-translate footnote-stencil
                                        (cons gap y-trans)))))))
         (main-stencil (ly:stencil-combine-at-edge
                        markup-stencil
                        X
                        RIGHT
                        (ly:make-stencil footnote-number x-ext y-ext)
                        padding)))
    (ly:stencil-add
     main-stencil
     (ly:make-stencil
      `(footnote ,footnote-hash #t ,(interpret-markup layout props note))
      '(0 . 0)
      '(0 . 0)))))

(define (prepend-props new-prop props)
  (cons
   (if (pair? (car new-prop))
       new-prop
       (list new-prop))
   props))

(define-markup-command (override layout props new-prop arg)
  (pair? markup?)
  #:category other
  #:as-string (markup->string arg
                              #:layout layout
                              #:props (prepend-props new-prop props))
  "
@cindex overriding property, within text markup

Add the argument @var{new-prop} to the property list for printing @var{arg}.

In general, any property may be overridden that is part of
@code{font-@/interface} (@rinternals{font-interface}),
@code{text-@/interface} (@rinternals{text-interface}), or
@code{instrument-@/specific-@/markup-@/interface}
(@rinternals{instrument-specific-markup-interface}).  Additionally, various
markup commands listen to other properties, too, as described in a markup
function's documentation.

@var{new-prop} is either a single alist pair or a non-empty list of alist pairs.

@lilypond[verbatim,quote]
\\markup {
  \\undertie \"undertied\"
  \\override #'(offset . 15)
  \\undertie \"offset undertied\"
  \\override #'((offset . 15) (thickness . 3))
  \\undertie \"offset thick undertied\"
}
@end lilypond"
  (interpret-markup layout
                    (prepend-props new-prop props)
                    arg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-markup-command (verbatim-file layout props name)
  (string?)
  #:category other
  #:as-string
  (begin
    (ly:note-extra-source-file name)
    (ly:gulp-file-utf8 name))
  "Read the contents of file @var{name} and include it verbatim.

@lilypond[verbatim,quote]
\\markup {
  \\verbatim-file \"en/included/simple.ly\"
}
@end lilypond

Use @code{\\withRelativeDir} as a prefix to @var{name} if the file should be
found relative to the input file."
  (ly:note-extra-source-file name)
  (interpret-markup layout props
                    (make-typewriter-markup
                     (make-column-markup
                      (string-split (ly:gulp-file-utf8 name)
                                    #\nl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fonts.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-markup-command (smaller layout props arg)
  (markup?)
  #:category font
  "Decrease current font size by@tie{}1 to print @var{arg}.

This function adjusts the @code{baseline-skip} and @code{word-space} properties
accordingly.

@lilypond[verbatim,quote]
\\markup {
  \\fontsize #3.5 {
    large text
    \\hspace #2
    \\smaller { smaller text }
    \\hspace #2
    large text
  }
}
@end lilypond"
  (interpret-markup layout props
                    `(,fontsize-markup -1 ,arg)))

(define-markup-command (larger layout props arg)
  (markup?)
  #:category font
  "Increase current font size by@tie{}1 to print @var{arg}.

This function adjusts the @code{baseline-skip} and @code{word-space} properties
accordingly.

@lilypond[verbatim,quote]
\\markup {
  default
  \\hspace #2
  \\larger larger
}
@end lilypond"
  (interpret-markup layout props
                    `(,fontsize-markup 1 ,arg)))

(define-public legacy-figured-bass-markup-fontsize #f)

(define-markup-command (figured-bass layout props arg)
  (markup?)
  #:category font
  "Set @var{arg} as small numbers for figured bass.

Specially slashed digits can be achieved with a trailing backslash (for numbers
6, 7, and@tie{}9) or a trailing plus (for numbers 2, 4,
and@tie{}5).@footnote{Internally, this works by activating the @q{dlig} OpenType
feature of the Emmentaler font.}

The use of a backslash is in analogy to
@code{\\figuremode} (@pxref{Entering figured bass}).  Note that to get
a backslash character in markup it must be escaped by doubling it.
Additionally, it must be put into double quotes.

@lilypond[verbatim,quote]
\\markup {
  \\figured-bass {
    2 3 4+ 7 \"9\\\\\"
  }
}
@end lilypond"
  (interpret-markup layout
                    (cons '((font-encoding . fetaText)
                            (font-features . ("dlig" "tnum" "cv47" "ss01")))
                          (if legacy-figured-bass-markup-fontsize
                              (cons '((font-size . -5)) props)
                              props))
                    (if legacy-figured-bass-markup-fontsize
                        arg
                        (make-fontsize-markup -5 arg))))

(define-public legacy-finger-markup-fontsize #f)

(define-markup-command (finger layout props arg)
  (markup?)
  #:category font
  "Set @var{arg} as small numbers for fingering instructions.

@lilypond[verbatim,quote]
\\markup {
  \\finger {
    1 2 3 4 5
  }
}
@end lilypond"
  (interpret-markup layout
                    (cons '((font-encoding . fetaText)
                            (font-features . ("cv47" "ss01")))
                          (if legacy-finger-markup-fontsize
                              (cons '((font-size . -5)) props)
                              props))
                    (if legacy-finger-markup-fontsize
                        arg
                        (make-fontsize-markup -5 arg))))

(define-markup-command (volta-number layout props arg)
  (markup?)
  #:category font
  "Set @var{arg} in a font appropriate for volta numbers.

@lilypond[verbatim,quote]
\\markup \\volta-number \"4.\"
@end lilypond"
  (interpret-markup layout
                    (cons '((font-encoding . fetaText)
                            (font-features . ("cv47" "ss01")))
                          props)
                    `(,fontsize-markup -2 ,arg)))


(define-markup-command (abs-fontsize layout props size arg)
  (number? markup?)
  #:properties ((word-space 0.6) (baseline-skip 3))
  #:category font
  "Use @var{size} as the absolute font size (in points) to display @var{arg}.

This function adjusts the @code{baseline-skip} and @code{word-space} properties
accordingly.

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
         (magnification (/ size ref-size)))
    (interpret-markup
     layout
     (cons
      `((baseline-skip . ,(* magnification baseline-skip))
        (word-space . ,(* magnification word-space))
        (font-size . ,(magnification->font-size magnification)))
      props)
     arg)))

(define-markup-command (fontsize layout props increment arg)
  (number? markup?)
  #:category font
  #:properties ((font-size 0)
                (word-space 1)
                (baseline-skip 2))
  "Increase current font size by @var{increment} to print @var{arg}.

This function adjusts the @code{baseline-skip} and @code{word-space} properties
accordingly.

@lilypond[verbatim,quote]
\\markup {
  default
  \\hspace #2
  \\fontsize #-1.5 smaller
}
@end lilypond"
  (interpret-markup
   layout
   (cons
    `((baseline-skip . ,(* baseline-skip (magstep increment)))
      (word-space . ,(* word-space (magstep increment)))
      (font-size . ,(+ font-size increment)))
    props)
   arg))

(define-markup-command (magnify layout props sz arg)
  (number? markup?)
  #:category font
  "
@cindex magnifying text

Magnify current font by factor @var{sz} to print @var{arg}.

@lilypond[verbatim,quote]
\\markup {
  default
  \\hspace #2
  \\magnify #1.5 {
    50% larger
  }
}
@end lilypond

Note that magnification only works if a font name is explicitly selected.  Use
@code{\\fontsize} otherwise."
  (interpret-markup
   layout
   (prepend-alist-chain 'font-size (magnification->font-size sz) props)
   arg))

(define-markup-command (bold layout props arg)
  (markup?)
  #:category font
  "Print @var{arg} with a bold face.

@lilypond[verbatim,quote]
\\markup {
  default
  \\hspace #2
  \\bold bold
}
@end lilypond

The code @code{\\markup \\bold @dots{}} is a shorthand for
@code{\\markup \\override #'(font-series . bold) @dots{}} --
using the more verbose form, it is possible to obtain nuances
such as semi-bold, if the text font has such variants.
Refer to the documentation for the @code{font-series}
properties (@rinternals{User backend properties})."
  (interpret-markup layout (prepend-alist-chain 'font-series 'bold props) arg))

(define-markup-command (sans layout props arg)
  (markup?)
  #:category font
  "Print @var{arg} with a sans-serif font.

This command sets the @code{font-family} property to @code{sans}.

@lilypond[verbatim,quote]
\\markup {
  default
  \\hspace #2
  \\sans {
    sans serif
  }
}
@end lilypond"
  (interpret-markup layout
                    (cons '((font-family . sans)
                            (font-encoding . latin1))
                          props)
                    arg))

(define-markup-command (number layout props arg)
  (markup?)
  #:category font
  "Print @var{arg} using the (music) font for numbers.

This font also contains symbols for figured bass, some punctuation, spaces of
various widths, some letters and text variants of accidentals.
Use @code{\\dynamic} to access the (very small number of) letters.
For accidentals you might use @code{\\number} in combination
with Unicode characters to access the text representation
forms of accidental glyphs, as the following table shows.

@lilypond[quote]
\\paper {
  % Avoid that Fontconfig arbitrarily selects a font for the
  % flat, which is not present in the URW typewriter font.
  property-defaults.fonts.sans = \"DejaVu Sans Mono\"
}

\\markuplist {
  \\override #'(padding . 2)
  \\table #'(0 0) {
    \"Unicode value\"          \"Unicode character\"
    \\typewriter \"U+266D \"   \\tiny \\number ♭
    \\typewriter \"U+266E \"   \\tiny \\number ♮
    \\typewriter \"U+266F \"   \\tiny \\number ♯
    \\typewriter \"U+1D12A\"   \\tiny \\number 𝄪
    \\typewriter \"U+1D12B\"   \\tiny \\number 𝄫
  }

  \\vspace #0.5

  \\override #'(baseline-skip . 3)
  \\left-column {
    Examples:
    \\line { \\concat { \\typewriter \" \\\\number \" \\sans ♭ }
            \\typewriter \" → \"
            \\tiny \\number ♭ }
    \\line { \\typewriter \" \\\\number { \\char ##x266F }\"
            \\typewriter \" → \"
            \\tiny \\number \\char ##x266F }
  }
}
@end lilypond

To get accidentals protected against overrides of @code{font-name} it is
preferable to use @code{\\text-doubleflat}, @code{\\text-flat},
@code{\\text-natural}, @code{\\text-sharp}, @code{\\text-doublesharp} or the
general @code{\\text-accidental} for the text variants of accidentals.

@cindex feature, OpenType font
@cindex font feature, OpenType
@cindex OpenType, font feature

The appearance of digits in the Emmentaler font can be controlled with
four OpenType features: @q{tnum}, @q{cv47}, @q{ss01}, and @q{kern},
which can be arbitrarily combined.

@indentedblock
@table @asis
@item tnum
If off (which is the default), glyphs @q{zero} to @q{nine} have no
left and right side bearings.  If on, the glyphs all have the same
advance width by making the bearings non-zero.

@item cv47
If on, glyphs @q{four} and @q{seven} have shorter vertical strokes.
Default is off.

@item ss01
If on, glyphs @q{zero} to @q{nine} have a fatter design, making them
more readable at small sizes.  Default is off.

@item kern
If on (which is the default), provide pairwise kerning between (most)
glyphs.

@end table
@end indentedblock

@lilypond[verbatim,quote]
\\markuplist
  \\number
  \\fontsize #4.5
  \\override #'((padding . 2)
               (baseline-skip . 4)
               (box-padding . 0)
               (thickness . 0.1))
  \\table #'(-1 -1 -1 -1) {
      0123456789 \\box 147 \\concat { \\box 1 \\box 4 \\box 7 }
    \\normal-text \\normalsize \"(time signatures)\"
    \\override #'(font-features .(\"cv47\")) {
      0123456789 \\box 147 \\concat { \\box 1 \\box 4 \\box 7 } }
    \\normal-text \\normalsize \"(alternatives)\"
    \\override #'(font-features .(\"tnum\" \"cv47\" \"-kern\")) {
      0123456789 \\box 147 \\concat { \\box 1 \\box 4 \\box 7 } }
    \\normal-text \\normalsize \"(fixed-width)\"
    \\override #'(font-features . (\"tnum\" \"cv47\" \"ss01\")) {
      0123456789 \\box 147 \\concat { \\box 1 \\box 4 \\box 7 } }
    \\normal-text \\normalsize \"(figured bass)\"
    \\override #'(font-features . (\"cv47\" \"ss01\")) {
      0123456789 \\box 147 \\concat { \\box 1 \\box 4 \\box 7 } }
    \\normal-text \\normalsize \"(fingering)\"
  }
@end lilypond

See also the markup commands @code{\\figured-bass} and
@code{\\finger}, which set the font features accordingly."
  (interpret-markup layout (prepend-alist-chain
                            'font-encoding 'fetaText props) arg))

(define-markup-command (serif layout props arg)
  (markup?)
  #:category font
  "Print @var{arg} with a serif font.

This command sets the @code{font-family} property to @code{serif}.

@lilypond[verbatim,quote]
\\markup {
  \\sans \\bold {
    sans serif, bold
    \\hspace #2
    \\serif {
      text in serif font
    }
    \\hspace #2
    return to sans
  }
}
@end lilypond"
  (interpret-markup layout
                    (cons '((font-family . serif)
                            (font-encoding . latin1))
                          props)
                    arg))

(define-markup-command (huge layout props arg)
  (markup?)
  #:category font
  "Set font size to value@tie{}2 to print @var{arg}.

@lilypond[verbatim,quote]
\\markup {
  default
  \\hspace #2
  \\huge huge
}
@end lilypond"
  (interpret-markup layout (prepend-alist-chain 'font-size 2 props) arg))

(define-markup-command (large layout props arg)
  (markup?)
  #:category font
  "Set font size to value@tie{}1 to print @var{arg}.

@lilypond[verbatim,quote]
\\markup {
  default
  \\hspace #2
  \\large large
}
@end lilypond"
  (interpret-markup layout (prepend-alist-chain 'font-size 1 props) arg))

(define-markup-command (normalsize layout props arg)
  (markup?)
  #:category font
  "Set font size to default (i.e., to value@tie{}0) to print @var{arg}.

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

(define-markup-command (small layout props arg)
  (markup?)
  #:category font
  "Set font size to value@tie{}-1 to print @var{arg}.

@lilypond[verbatim,quote]
\\markup {
  default
  \\hspace #2
  \\small small
}
@end lilypond"
  (interpret-markup layout (prepend-alist-chain 'font-size -1 props) arg))

(define-markup-command (tiny layout props arg)
  (markup?)
  #:category font
  "Set font size to value@tie{}-2 to print @var{arg}.

@lilypond[verbatim,quote]
\\markup {
  default
  \\hspace #2
  \\tiny tiny
}
@end lilypond"
  (interpret-markup layout (prepend-alist-chain 'font-size -2 props) arg))

(define-markup-command (teeny layout props arg)
  (markup?)
  #:category font
  "Set font size to value@tie{}-3 to print @var{arg}.

@lilypond[verbatim,quote]
\\markup {
  default
  \\hspace #2
  \\teeny teeny
}
@end lilypond"
  (interpret-markup layout (prepend-alist-chain 'font-size -3 props) arg))

(define-markup-command (fontCaps layout props arg)
  (markup?)
  #:category font
  "Print @var{arg} in small caps.

This command sets the @code{font-variant} property to @code{small-caps}.

Unlike @code{\\smallCaps}, which merely uses capital letters at a smaller font
size, this uses the actual variant of the font for small caps.  (As a
consequence, if you configure a custom text font, this command has no effect if
that font does not have a small caps variant.)

@lilypond[verbatim,quote]
\\markup \\fontCaps \"Small caps\"
@end lilypond"
  (interpret-markup layout
                    (prepend-alist-chain 'font-variant 'small-caps props)
                    arg))

(define-markup-command (with-string-transformer layout props transformer arg)
  (procedure? markup?)
  #:category font
  "Apply string transformer function @var{transformer} to @var{arg}.

Whenever a string is interpreted inside @var{arg}, function @var{transformer} is
called first, and its result is then interpreted.  The arguments passed to
@var{transformer} are the output definition, the property alist chain, and the
markup @var{arg}.  See @rextend{New markup command definition} about the two
first arguments.

@lilypond[verbatim,quote]
\\markup \\with-string-transformer
  #(lambda (layout props str)
     (string-upcase str))
  \\concat { \"abc\" \\larger \"def\" }
@end lilypond"
  (interpret-markup
   layout
   (prepend-alist-chain 'string-transformers
                        (cons transformer
                              (chain-assoc-get 'string-transformers props))
                        props)
   arg))

;; Poor man's caps
(define-markup-command (smallCaps layout props arg)
  (markup?)
  #:category font
  "Print @var{arg} in (fake) small caps.

Unlike @code{\\fontCaps}, which uses the actual small caps variant of the
current font, this fakes small caps by using capital letters at a smaller font
size.  It can thus be used for fonts that don't have a small caps variant.

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
          (make-fontsize-markup -2 final-string)
          final-string)))
  (define (make-small-caps _layout _props str)
    (let loop ((rest-chars (string->list str))
               (currents '())
               (current-is-lower #f)
               (prev-result '()))
      (if (null? rest-chars)
          (make-concat-markup
           (reverse! (cons (char-list->markup currents current-is-lower)
                           prev-result)))
          (let* ((ch (car rest-chars))
                 (is-lower (char-lower-case? ch)))
            (if (or (and current-is-lower is-lower)
                    (and (not current-is-lower) (not is-lower)))
                (loop (cdr rest-chars)
                      (cons ch currents)
                      is-lower
                      prev-result)
                (loop (cdr rest-chars)
                      (list ch)
                      is-lower
                      (if (null? currents)
                          prev-result
                          (cons (char-list->markup
                                 currents current-is-lower)
                                prev-result))))))))
  (interpret-markup layout props
                    (make-with-string-transformer-markup
                     make-small-caps
                     arg)))

(define-markup-command (caps layout props arg)
  (markup?)
  #:category font
  "Print @var{arg} in (fake) small caps.

This function is a copy of the @code{\\smallCaps} command.

@lilypond[verbatim,quote]
\\markup {
  default
  \\hspace #2
  \\caps {
    Text in small caps
  }
}
@end lilypond

Use @code{\\fontCaps} for real small caps (if the font provides it)."
  (interpret-markup layout props (make-smallCaps-markup arg)))

(define-markup-command (dynamic layout props arg)
  (markup?)
  #:category font
  "Print @var{arg} using the (music) font for dynamics.

This font only contains letters @b{f}, @b{m}, @b{n}, @b{p}, @b{r}, @b{s}, and
@b{z}.  When producing phrases like @q{più@tie{}@b{f}}, the normal words (like
@q{più}) should be done in a different font.  The recommended font for this is
bold and italic.

@lilypond[verbatim,quote]
\\markup {
  \\dynamic {
    sfzp
  }
}
@end lilypond"
  (interpret-markup
   layout (prepend-alist-chain 'font-encoding 'fetaText props) arg))

(define-markup-command (italic layout props arg)
  (markup?)
  #:category font
  "Print @var{arg} in italics.

This command sets the @code{font-shape} property to @code{italic}.

@lilypond[verbatim,quote]
\\markup {
  default
  \\hspace #2
  \\italic italic
}
@end lilypond"
  (interpret-markup layout (prepend-alist-chain 'font-shape 'italic props) arg))

(define-markup-command (typewriter layout props arg)
  (markup?)
  #:category font
  "Print @var{arg} in typewriter.

This command sets the @code{font-family} property to @code{typewriter}, also
switching off the @q{liga} OpenType feature to disable ligatures like @q{ff} or
@q{fi}.

@lilypond[verbatim,quote]
\\markup {
  \"default fi ff\"
  \\hspace #2
  \\typewriter \"typewriter fi ff\"
}
@end lilypond"
  (interpret-markup layout
                    (cons '((font-family . typewriter)
                            (font-encoding . latin1)
                            (font-features . ("-liga")))
                          props)
                    arg))

(define-markup-command (upright layout props arg)
  (markup?)
  #:category font
  "Print @var{arg} upright.

This command is the opposite of @code{\\italic}; it sets the @code{font-shape}
property to @code{upright}.

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

(define-markup-command (normal-weight layout props arg)
  (markup?)
  #:category font
  "Switch to normal weight (in contrast to bold) to print @var{arg}.

This command sets the @code{font-series} property to @code{normal}.

@lilypond[verbatim,quote]
\\markup {
  \\bold {
    some bold text
    \\hspace #2
    \\normal-weight {
      normal font series
    }
    \\hspace #2
    bold again
  }
}
@end lilypond"
  (interpret-markup layout (prepend-alist-chain 'font-series 'normal props)
                    arg))

(define-markup-command (normal-text layout props arg)
  (markup?)
  #:category font
  "Print @var{arg} with default text font.

This resets all font-related properties (except the size), no matter what font
was used earlier.

@lilypond[verbatim,quote]
\\markup {
  \\huge \\bold \\sans \\fontCaps {
    huge bold sans caps
    \\hspace #2
    \\normal-text {
      huge normal
    }
    \\hspace #2
    as before
  }
}
@end lilypond"
  ;; ugh - latin1
  (interpret-markup layout
                    (cons '((font-family . serif) (font-shape . upright)
                            (font-series . normal) (font-encoding . latin1)
                            (font-variant . normal))
                          props)
                    arg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; symbols.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-markup-command (musicglyph layout props glyph-name)
  (string?)
  #:category music
  ;; TODO: as-string?
  "Print music symbol @var{glyph-name}.

See @rnotation{The Emmentaler font} for a complete listing of the possible glyph
names.

@lilypond[verbatim,quote]
\\markup {
  \\musicglyph \"f\"
  \\musicglyph \"rests.2\"
  \\musicglyph \"clefs.G_change\"
}
@end lilypond"
  (let* ((font (ly:paper-get-font layout
                                  (cons '((font-encoding . fetaMusic)
                                          (font-name . #f))

                                        props)))
         (glyph (ly:font-get-glyph font glyph-name)))
    (if (null? (ly:stencil-expr glyph))
        (ly:warning (G_ "Cannot find glyph ~a") glyph-name))

    glyph))

(define-markup-command (coda layout props)
  ()
  #:category music
  ;; TODO: make more general, in \musicglyph?
  #:as-string
  (ly:wide-char->utf-8 #x1d10c)
  "Draw a coda sign.

@lilypond[verbatim,quote]
\\markup {
  \\coda
}
@end lilypond"
  (let ((coda (ly:stencil-scale
               (interpret-markup
                layout props
                (make-musicglyph-markup "scripts.coda")) 0.8 0.8)))
    (ly:stencil-translate-axis
     coda
     (/ (interval-length (ly:stencil-extent coda Y)) 2.7) Y)))

(define-markup-command (varcoda layout props)
  ()
  #:category music
  #:as-string
  (ly:wide-char->utf-8 #x1d10c)
  "Draw a varcoda sign.

@lilypond[verbatim,quote]
\\markup {
  \\varcoda
}
@end lilypond"
  (let ((coda (ly:stencil-scale
               (interpret-markup
                layout props
                (make-musicglyph-markup "scripts.varcoda")) 0.8 0.8)))
    (ly:stencil-translate-axis
     coda
     (/ (interval-length (ly:stencil-extent coda Y)) 2.7) Y)))

(define-markup-command (segno layout props)
  ()
  #:category music
  #:as-string
  (ly:wide-char->utf-8 #x1d10b)
  "Draw a segno symbol.

@lilypond[verbatim,quote]
\\markup {
  \\segno
}
@end lilypond"
  (let ((segno (ly:stencil-scale
                (interpret-markup
                 layout props
                 (make-musicglyph-markup "scripts.segno")) 0.6 0.6)))
    (ly:stencil-translate-axis
     segno
     (/ (interval-length (ly:stencil-extent segno Y)) 2.25) Y)))

(define-markup-command (accidental layout props alteration)
  (exact-rational?)
  #:category music
  #:properties ((alteration-glyph-name-alist))
  "Select an accidental glyph for @var{alteration}, given as a rational number.

Use @code{\\text-accidental} instead if you need glyph representation forms that
fit and align well with text.

@lilypond[verbatim,quote]
\\markup {
  text
  \\tiny { \\accidental #1/2 \\accidental #-1/2 }
  text
  \\tiny { \\text-accidental #1/2 \\text-accidental #-1/2 }
  text
}
@end lilypond"
  (interpret-markup layout props
                    (make-musicglyph-markup
                     (or
                      (assv-ref alteration-glyph-name-alist alteration)
                      (begin
                        (ly:warning (G_ "no accidental glyph found for alteration ~a")
                                    alteration)
                        "noteheads.s1cross")))))

(define-markup-command (doublesharp layout props)
  ()
  #:category music
  "Draw a double sharp symbol.

@lilypond[verbatim,quote]
\\markup {
  \\doublesharp
}
@end lilypond"
  (interpret-markup layout props
                    (make-accidental-markup 1)))

(define-markup-command (sesquisharp layout props)
  ()
  #:category music
  "Draw a 3/2 sharp symbol.

@lilypond[verbatim,quote]
\\markup {
  \\sesquisharp
}
@end lilypond"
  (interpret-markup layout props
                    (make-accidental-markup 3/4)))

(define-markup-command (sharp layout props)
  ()
  #:category music
  "Draw a sharp symbol.

@lilypond[verbatim,quote]
\\markup {
  \\sharp
}
@end lilypond"
  (interpret-markup layout props
                    (make-accidental-markup 1/2)))

(define-markup-command (semisharp layout props)
  ()
  #:category music
  "Draw a semisharp symbol.

@lilypond[verbatim,quote]
\\markup {
  \\semisharp
}
@end lilypond"
  (interpret-markup layout props
                    (make-accidental-markup 1/4)))

(define-markup-command (natural layout props)
  ()
  #:category music
  "Draw a natural symbol.

@lilypond[verbatim,quote]
\\markup {
  \\natural
}
@end lilypond"
  (interpret-markup layout props
                    (make-accidental-markup 0)))

(define-markup-command (semiflat layout props)
  ()
  #:category music
  "Draw a semiflat symbol.

@lilypond[verbatim,quote]
\\markup {
  \\semiflat
}
@end lilypond"
  (interpret-markup layout props
                    (make-accidental-markup -1/4)))

(define-markup-command (flat layout props)
  ()
  #:category music
  "Draw a flat symbol.

@lilypond[verbatim,quote]
\\markup {
  \\flat
}
@end lilypond"
  (interpret-markup layout props
                    (make-accidental-markup -1/2)))

(define-markup-command (sesquiflat layout props)
  ()
  #:category music
  "Draw a 3/2 flat symbol.

@lilypond[verbatim,quote]
\\markup {
  \\sesquiflat
}
@end lilypond"
  (interpret-markup layout props
                    (make-accidental-markup -3/4)))

(define-markup-command (doubleflat layout props)
  ()
  #:category music
  "Draw a double flat symbol.

@lilypond[verbatim,quote]
\\markup {
  \\doubleflat
}
@end lilypond"
  (interpret-markup layout props
                    (make-accidental-markup -1)))

(define-markup-command (text-accidental layout props alteration)
  (exact-rational?)
  #:category music
  #:properties ((alteration-glyph-name-alist))
  "Select an accidental glyph for @var{alteration} (given as a rational number)
that aligns well with text.

@lilypond[verbatim,quote]
\\markup {
  text
  \\tiny { \\text-accidental #1/2 \\text-accidental #-1/2 }
  text
}
@end lilypond"

  (let* ((acc (assv-ref alteration-glyph-name-alist alteration)))
    ;; If no accidental for current alteration can be found, warn and use
    ;; a crossed note head as substitute. Otherwise try to get accidentals for
    ;; text or fall back to music accidentals.
    (if (not acc)
        (begin
          (ly:warning (G_ "no accidental glyph found for alteration ~a")
                      alteration)
          (interpret-markup layout props
                            (make-musicglyph-markup "noteheads.s1cross")))
        (let* ((text-acc (format #f "~a.figbass" acc))
               ;; protect against overrides of 'font-name
               (font (ly:paper-get-font layout (cons '((font-encoding . fetaMusic)
                                                       (font-name . #f))
                                                     props)))
               (text-acc-stil (ly:font-get-glyph font text-acc)))

          (if (ly:stencil-empty? text-acc-stil)
              (begin
                (ly:warning (G_ "no text accidental glyph found for alteration ~a, falling back to music accidental.")
                            alteration)
                (ly:font-get-glyph font acc))
              text-acc-stil)))))

(define-markup-command (text-doublesharp layout props)
  ()
  #:category music
  "Draw a double sharp symbol for text.

@lilypond[verbatim,quote]
\\markup {
  \\text-doublesharp
}
@end lilypond"
  (interpret-markup layout props
                    (make-text-accidental-markup 1)))

(define-markup-command (text-sharp layout props)
  ()
  #:category music
  "Draw a sharp symbol for text.

@lilypond[verbatim,quote]
\\markup {
  \\text-sharp
}
@end lilypond"
  (interpret-markup layout props
                    (make-text-accidental-markup 1/2)))

(define-markup-command (text-natural layout props)
  ()
  #:category music
  "Draw a natural symbol for text.

@lilypond[verbatim,quote]
\\markup {
  \\text-natural
}
@end lilypond"
  (interpret-markup layout props
                    (make-text-accidental-markup 0)))

(define-markup-command (text-flat layout props)
  ()
  #:category music
  "Draw a flat symbol for text.

@lilypond[verbatim,quote]
\\markup {
  \\text-flat
}
@end lilypond"
  (interpret-markup layout props
                    (make-text-accidental-markup -1/2)))

(define-markup-command (text-doubleflat layout props)
  ()
  #:category music
  "Draw a double flat symbol for text.

@lilypond[verbatim,quote]
\\markup {
  \\text-doubleflat
}
@end lilypond"
  (interpret-markup layout props
                    (make-text-accidental-markup -1)))

(define-markup-command (with-color layout props col arg)
  (color? markup?)
  #:category other
  ;; Needed because a color can be a string and the default
  ;; behavior would include it in the result.
  #:as-string (markup->string arg #:layout layout #:props props)
  "
@cindex coloring text

Use color @var{col} to draw @var{arg}.

@xref{Coloring objects} for valid color specifications.

@lilypond[verbatim,quote]
\\markup {
  \\with-color #red red
  \\hspace #2
  \\with-color #green green
  \\hspace #2
  \\with-color \"#0000ff\" blue
}
@end lilypond"
  (stencil-with-color (interpret-markup layout props arg) col))

(define tie-regexp (ly:make-regex "~"))
(define short-tie-regexp (ly:make-regex "~[^.]~"))

(define-markup-command (tied-lyric layout props str)
  (string?)
  #:category music
  #:properties ((word-space))
  ;; Use Unicode undertie character U+203F.
  #:as-string (ly:regex-replace tie-regexp str "\u203f")
  "
@cindex simple text string, with tie characters

Replace @q{~} tilde symbols with tie characters in @var{str}.

@lilypond[verbatim,quote]
\\markup \\column {
  \\tied-lyric
    \"Siam navi~all'onde~algenti Lasciate~in abbandono\"
  \\tied-lyric
    \"Impetuosi venti I nostri~affetti sono\"
  \\tied-lyric
    \"Ogni diletto~e scoglio Tutta la vita~e~un mar.\"
}
@end lilypond"
  (define (replace-ties tie str)
    (if (string-contains str "~")
        (let*
            ((half-space (/ word-space 2))
             (parts (string-split str #\~))
             (tie-str (make-line-markup
                       (list
                        (make-hspace-markup half-space)
                        (make-musicglyph-markup tie)
                        (make-hspace-markup half-space))))
             (joined  (list-join parts tie-str)))
          (make-concat-markup joined))
        str))

  (define (match-short str) (ly:regex-exec short-tie-regexp str))

  (define (replace-short str mkp)
    (let ((match (match-short str)))
      (if (not match)
          (make-concat-markup (list
                               mkp
                               (replace-ties "ties.lyric.default" str)))
          (let ((new-str (ly:regex-match-suffix match))
                (new-mkp (make-concat-markup (list
                                              mkp
                                              (replace-ties "ties.lyric.default"
                                                            (ly:regex-match-prefix match))
                                              (replace-ties "ties.lyric.short"
                                                            (ly:regex-match-substring match))))))
            (replace-short new-str new-mkp)))))

  (interpret-markup layout
                    props
                    (replace-short str (markup))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; glyphs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-markup-command (arrow-head layout props axis dir filled)
  (integer? ly:dir? boolean?)
  #:category graphic
  "Print an arrow head along @var{axis} in direction @var{dir}.

Fill the head if @var{filled} is set to @code{#t}.

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
      ((name (format #f "arrowheads.~a.~a~a"
                     (if filled
                         "close"
                         "open")
                     axis
                     dir)))
    (ly:font-get-glyph
     (ly:paper-get-font layout (cons '((font-encoding . fetaMusic))
                                     props))
     name)))

(define-markup-command (lookup layout props glyph-name)
  (string?)
  #:category other
  "Print a brace glyph with name @var{glyph-name}.

This is a historical command; @code{\\left-brace} (which looks up the glyph by
absolute size and is independent of the font size) is recommended instead.

@lilypond[verbatim,quote]
\\markup \\lookup \"brace200\"
@end lilypond"
  (let ((result
         (ly:font-get-glyph (ly:paper-get-font
                             layout
                             (cons '((font-encoding . fetaBraces)
                                     (font-name . #f))
                                   props))
                            glyph-name)))
    (when (ly:stencil-empty? result)
      (ly:warning (G_ "cannot find brace glyph '~a'") glyph-name)
      (when (not (string-startswith glyph-name "brace"))
        (ly:warning (G_ "use \\musicglyph instead of \\lookup for non-brace \
glyphs"))))
    result))

(define-markup-command (char layout props num)
  (integer?)
  #:category other
  #:as-string (ly:wide-char->utf-8 num)
  "Produce a single Unicode character with code @var{num}.

Characters encoded in hexadecimal format require the prefix @code{#x}.

@lilypond[verbatim,quote]
\\markup {
  \\char #65 \\char ##x00a9
}
@end lilypond"
  (ly:text-interface::interpret-markup layout props (ly:wide-char->utf-8 num)))

(define mark-alphabets
  `((alphabet        . ,(list->vector (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
    (alphabet-omit-i . ,(list->vector (string->list "ABCDEFGHJKLMNOPQRSTUVWXYZ")))
    (alphabet-omit-j . ,(list->vector (string->list "ABCDEFGHIKLMNOPQRSTUVWXYZ")))))

(define (markgeneric-string number alphabet double-letters)
  (let* ((the-alphabet (assq-ref mark-alphabets alphabet))
         (the-alphabet-length (vector-length the-alphabet)))
    (case double-letters
      ((repeat) (let ((the-length (1+ (quotient (1- number) the-alphabet-length)))
                      (the-index     (remainder (1- number) the-alphabet-length)))
                  (make-string the-length (vector-ref the-alphabet the-index))))
      ((combine) (let loop ((num (1- number)))
                   (if (< num the-alphabet-length)
                       (string (vector-ref the-alphabet num))
                       (string-append
                        (loop (1- (quotient num the-alphabet-length)))
                        (loop    (remainder num the-alphabet-length)))))))))

(define-markup-command (markletter layout props num)
  (integer?)
  #:category other
  #:as-string (markgeneric-string num 'alphabet-omit-i 'combine)
  "Make a markup letter for @var{num}.

The letters start with A to@tie{}Z (skipping letter@tie{}I), and continue with
double letters.

@lilypond[verbatim,quote]
\\markup {
  \\markletter #8
  \\hspace #2
  \\markletter #26
}
@end lilypond"
  (ly:text-interface::interpret-markup layout props
                                       (markgeneric-string num 'alphabet-omit-i 'combine)))

(define-markup-command (markalphabet layout props num)
  (integer?)
  #:category other
  #:as-string (markgeneric-string num 'alphabet 'combine)
  "Make a markup letter for @var{num}.

The letters start with A to@tie{}Z and continue with double letters.

@lilypond[verbatim,quote]
\\markup {
  \\markalphabet #8
  \\hspace #2
  \\markalphabet #26
}
@end lilypond"
  (ly:text-interface::interpret-markup layout props
                                       (markgeneric-string num 'alphabet 'combine)))

(define-public (horizontal-slash-interval num forward number-interval mag)
  (if forward
      (cond ;; ((= num 6) (interval-widen number-interval (* mag 0.5)))
       ;; ((= num 5) (interval-widen number-interval (* mag 0.5)))
       (else (interval-widen number-interval (* mag 0.25))))
      (cond ((= num 6) (interval-widen number-interval (* mag 0.5)))
            ;; ((= num 5) (interval-widen number-interval (* mag 0.5)))
            (else (interval-widen number-interval (* mag 0.25))))
      ))

(define-public (adjust-slash-stencil num forward stencil mag)
  (if forward
      (cond ((= num 2)
             (ly:stencil-translate stencil (cons (* mag -0.00) (* mag 0.2))))
            ((= num 3)
             (ly:stencil-translate stencil (cons (* mag -0.00) (* mag 0.2))))
            ;; ((= num 5)
            ;;     (ly:stencil-translate stencil (cons (* mag -0.00) (* mag -0.07))))
            ;; ((= num 7)
            ;;     (ly:stencil-translate stencil (cons (* mag -0.00) (* mag -0.15))))
            (else stencil))
      (cond ((= num 6)
             (ly:stencil-translate stencil (cons (* mag -0.00) (* mag 0.15))))
            ;; ((= num 8)
            ;;     (ly:stencil-translate stencil (cons (* mag -0.00) (* mag -0.15))))
            (else stencil))
      )
  )

(define (slashed-digit-internal layout props num forward font-size thickness)
  (let* ((mag (magstep font-size))
         (thickness (* mag
                       (ly:output-def-lookup layout 'line-thickness)
                       thickness))
         ;; backward slashes might use slope and point in the other direction!
         (dy (* mag (if forward 0.4 -0.4)))
         (number-stencil (interpret-markup
                          layout
                          (cons '((font-encoding . fetaText)
                                  (font-features . ("tnum" "cv47" "ss01")))
                                props)
                          (number->string num)))
         (num-x (horizontal-slash-interval num forward (ly:stencil-extent number-stencil X) mag))
         (center (interval-center (ly:stencil-extent number-stencil Y)))
         ;; Use the real extents of the slash, not the whole number,
         ;; because we might translate the slash later on!
         (num-y (interval-widen (cons center center) (abs dy)))
         (is-sane (and (interval-sane? num-x) (interval-sane? num-y)))
         (slash-stencil (if is-sane
                            (make-line-stencil thickness
                                               (car num-x) (- (interval-center num-y) dy)
                                               (cdr num-x) (+ (interval-center num-y) dy))
                            #f)))
    (if (ly:stencil? slash-stencil)
        (begin
          ;; for some numbers we need to shift the slash/backslash up or
          ;; down to make the slashed digit look better
          (set! slash-stencil (adjust-slash-stencil num forward slash-stencil mag))
          (set! number-stencil
                (ly:stencil-add number-stencil slash-stencil)))
        (ly:warning (G_ "Unable to create slashed digit ~a") num))
    number-stencil))


(define-markup-command (slashed-digit layout props num)
  (integer?)
  #:category other
  ;; TODO: as-string?
  #:properties ((font-size 0)
                (thickness 1.6))
  "
@cindex slashed digit

Print number @var{num} with the Emmentaler font, crossed through with a
slash.

This is for use in the context of figured bass notation.

@lilypond[verbatim,quote]
\\markup {
  \\slashed-digit #5
  \\hspace #2
  \\override #'(thickness . 3)
  \\slashed-digit #7
}
@end lilypond"
  (slashed-digit-internal layout props num #t font-size thickness))

(define-markup-command (backslashed-digit layout props num)
  (integer?)
  #:category other
  #:properties ((font-size 0)
                (thickness 1.6))
  ;; TODO: as-string?
  "
@cindex backslashed digit

Print number @var{num} with the Emmentaler font, crossed through with a
backslash.

This is for use in the context of figured bass notation.

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
(define eyeglassespath
  '((moveto 0.42 0.77)
    (rcurveto 0 0.304 -0.246 0.55 -0.55 0.55)
    (rcurveto -0.304 0 -0.55 -0.246 -0.55 -0.55)
    (rcurveto 0 -0.304 0.246 -0.55 0.55 -0.55)
    (rcurveto 0.304 0 0.55 0.246 0.55 0.55)
    (closepath)
    (moveto 2.07 0.77)
    (rcurveto 0 0.304 -0.246 0.55 -0.55 0.55)
    (rcurveto -0.304 0 -0.55 -0.246 -0.55 -0.55)
    (rcurveto 0 -0.304 0.246 -0.55 0.55 -0.55)
    (rcurveto 0.304 0 0.55 0.246 0.55 0.55)
    (closepath)
    (moveto 1.025 0.935)
    (rcurveto 0 0.182 -0.148 0.33 -0.33 0.33)
    (rcurveto -0.182 0 -0.33 -0.148 -0.33 -0.33)
    (moveto -0.68 0.77)
    (rlineto 0.66 1.43)
    (rcurveto 0.132 0.286 0.55 0.44 0.385 -0.33)
    (moveto 2.07 0.77)
    (rlineto 0.66 1.43)
    (rcurveto 0.132 0.286 0.55 0.44 0.385 -0.33)))

(define-markup-command (eyeglasses layout props)
  ()
  #:category other
  ;; TODO: as-string?
  "Prints out eyeglasses, indicating strongly to look at the conductor.
@lilypond[verbatim,quote]
\\markup { \\eyeglasses }
@end lilypond"
  (interpret-markup layout props
                    (make-override-markup '(line-cap-style . butt)
                                          (make-path-markup 0.15 eyeglassespath))))

(define-markup-command (left-brace layout props size)
  (number?)
  #:category other
  #:as-string "{"
  "
@cindex brace, in markup
@cindex staff brace, in markup

Print a brace from the music font, of height @var{size} (in points).

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
          (ly:warning (ice9-format #f (G_ "no brace found for point size ~,1f ") size))
          (ly:warning (ice9-format #f
                                   (G_ "defaulting to ~,1f pt")
                                   (/ (* scale (interval-length
                                                (ly:stencil-extent glyph-found Y)))
                                      (ly:pt 1))))))
    glyph-found))

(define-markup-command (right-brace layout props size)
  (number?)
  #:category other
  #:as-string "}"
  "A music brace in point size @var{size}, rotated 180 degrees.

@lilypond[verbatim,quote]
\\markup {
  \\right-brace #45
  \\hspace #2
  \\right-brace #35
}
@end lilypond"
  (interpret-markup layout props
                    (make-rotate-markup
                     180 (make-left-brace-markup size))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the bar line command.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-markup-command (bar-line layout props strg)(string?)
  #:category music
  #:properties ((font-size 0)
                (height 4)
                (dot-count 4)
                (dash-count 5)
                (hair-thickness 1.9)
                (kern 3.0)
                (thick-thickness 6.0))
  "
@cindex bar line, in markup

Print a bar line in markup.

The allowed characters for input string @var{strg} are @samp{;|.:!S[]@{@}},
having the same meaning as with the @code{\\bar} command.
The additional characters @samp{@{} and @samp{@}} denote left and right braces,
respectively.

The output is vertically centered.

Changes of @code{font-size} are respected.

The default of @code{height} is 4@tie{}staff-space units.
Apart from the bracket tips of a bracket bar line and the segno bar line all
other bar lines are scaled with @code{height}.  We don't scale bracket tips and
segno to meet the behaviour of @code{SystemStartBracket} and the segno barline.

@code{\\bar-line} is further customizable by overriding @code{dot-count} and
@code{dash-count} for dotted and dashed bar lines.
The values for @code{hair-thickness}, @code{kern} and @code{thick-thickness} are
customizable as well; defaults are the same as the values of the corresponding
@code{BarLine} grob properties.

@lilypond[verbatim,quote]
\\markup {
   \\override #'(word-space . 2)
   \\column {
      \\line {
        Examples
        \\fontsize #-5 \\translate-scaled #'(0 . 2) {
          \\bar-line \":|.|:\"
          \\bar-line \";!S!;\"
          \\bar-line \"]{|}[\"
        }
      }
      \\line {
        Examples
        \\fontsize #0 \\translate-scaled #'(0 . 2) {
          \\bar-line \":|.|:\"
          \\bar-line \";!S!;\"
          \\bar-line \"]{|}[\"
        }
      }
      \\line {
        Examples
        \\fontsize #5 \\translate-scaled #'(0 . 2) {
          \\bar-line \":|.|:\"
          \\bar-line \";!S!;\"
          \\bar-line \"]{|}[\"
        }
      }
   }
}
@end lilypond"

  ;; simple bar-line
  (define (make-bar-line thickness height font-size blot-diameter)
    "Draw a simple bar line."
    (ly:round-filled-box
     (cons 0 (* thickness (magstep font-size)))
     (cons 0 (* height (magstep font-size)))
     blot-diameter))

  ;; colon bar-line
  (define (make-colon-bar-line height font-size)
    (let* ((font
            (ly:paper-get-font layout
                               (cons '((font-encoding . fetaMusic)) props)))
           (dot (ly:font-get-glyph font "dots.dot")))
      (ly:stencil-add
       dot
       (ly:stencil-translate-axis
        dot
        (* (magstep font-size) (/ height 4))
        Y))))

  ;; dotted bar-line
  (define (make-dotted-bar-line height font-size)
    "Draw a dotted barline."
    ;; Usually dots are printed between the five staff lines.  We keep this
    ;; behaviour even in the absence of said staff lines and draw a line of
    ;; four dots.
    ;; To get more dots override the `dot-count' property.
    (let* ((font
            (ly:paper-get-font layout
                               (cons '((font-encoding . fetaMusic)) props)))
           (dot (ly:font-get-glyph font "dots.dot"))
           (dot-height
            (interval-length (ly:stencil-extent dot Y)))
           (scaled-height (* (magstep font-size) height))
           (space-between-dots
            (/ (- scaled-height (* dot-count dot-height)) dot-count)))
      (stack-stencils Y UP space-between-dots (make-list dot-count dot))))

  ;; dashed bar-line
  (define (make-dashed-bar-line thickness height font-size blot-diameter)
    "Draw a dashed bar line."
    ;; According to the most common 5-lines staff we draw a dashed line with
    ;; three full dashes and two half-sized dashes at top/bottom.
    ;; To get more dashes override the `dash-count' property.
    ;; In the absence of staff-lines using the `draw-dashed-line-markup' looks
    ;; ugly, especially with larger font size.
    ;; Thus dashes are always drawn by `ly:round-filled-box'.
    (let* ((scaled-thick (* (magstep font-size) thickness))
           (scaled-height (* (magstep font-size) height))
           ;; `dash-size' is the printed dash plus the white-space
           ;; |-------------------|
           ;; |xxxxxxxxxx         |
           ;; |-------------------|
           (dash-size (/ scaled-height (1- dash-count)))
           (inked-dash-size (* dash-size 2/3))
           (white-space (* dash-size 1/3))
           (dash
            (ly:round-filled-box
             (cons 0 scaled-thick)
             (cons 0 inked-dash-size)
             blot-diameter))
           (half-dash
            (ly:round-filled-box
             (cons 0 scaled-thick)
             (cons 0 (/ inked-dash-size 2))
             blot-diameter))
           (dashes
            (cons half-dash
                  (append
                   (make-list (- dash-count 2) dash)
                   (list half-dash)))))
      (stack-stencils Y UP white-space dashes)))

  ;; bracket bar-line
  (define (make-bracket-bar-line height font-size dir)
    "Draw a bracket-style bar line.  If @var{dir} is set to @code{LEFT}, the
     opening bracket will be drawn, for @code{RIGHT} we get the
     closing bracket."
    ;; The bracket-tips scale with `font-size' but not with `height'.
    (let* ((font
            (ly:paper-get-font layout
                               (cons '((font-encoding . fetaMusic)) props)))
           (brackettips-up (ly:font-get-glyph font "brackettips.up"))
           (brackettips-down (ly:font-get-glyph font "brackettips.down"))
           (line-thickness (ly:output-def-lookup layout 'line-thickness))
           (thick-thickness (* line-thickness thick-thickness))
           (blot (ly:output-def-lookup layout 'blot-diameter))
           ;; The x-extent of the brackettips must not be taken into account
           ;; for bar line constructs like "[|:", so we set new bounds:
           (tip-up-stil
            (ly:make-stencil
             (ly:stencil-expr brackettips-up)
             (cons 0 0)
             (ly:stencil-extent brackettips-up Y)))
           (tip-down-stil
            (ly:make-stencil
             (ly:stencil-expr brackettips-down)
             (cons 0 0)
             (ly:stencil-extent brackettips-down Y)))
           (stencil
            (ly:stencil-add
             (make-bar-line thick-thickness height font-size blot)
             (ly:stencil-translate-axis
              tip-up-stil
              (* (magstep font-size) height)
              Y)
             tip-down-stil)))
      (if (eq? dir LEFT)
          stencil
          (flip-stencil X stencil))))

  ;; brace bar-line
  (define (make-brace-bar-line height dir font-size)
    "Draw a brace bar line.  If @var{dir} is set to @code{LEFT}, the
     opening brace will be drawn, for @code{RIGHT} we get the
     closing brace."
    ;; The default brace with zero `fontsize' and `default' height has size 20.
    ;; We adjust it with other `fontsize' and `height' (as opposed to
    ;; bracket-tips and segno), in order to have the brace the same height as a
    ;; simple bar line.
    ;; Sometimes warnings of kind
    ;; \"no brace found for point size ...\" \"defaulting to ... pt\" are
    ;; expected.
    (if (negative? dir)
        (left-brace-markup layout props
                           (* (magstep font-size) (/ height 4) 20.0))
        (right-brace-markup layout props
                            (* (magstep font-size) (/ height 4) 20.0))))

  ;; segno bar-line
  (define (make-segno-bar-line height font-size blot-diameter)
    "Draw a segno bar line.
     The segno sign is drawn over a double bar line."
    ;; The segno scales with `font-size', but not with `height'.
    (let* ((line-thickness (ly:output-def-lookup layout 'line-thickness))
           (thinkern (* line-thickness kern))
           (thin-thickness (* line-thickness hair-thickness))
           (thin-stil
            (make-bar-line
             thin-thickness
             height
             font-size
             blot-diameter))
           (double-line-stil
            (ly:stencil-combine-at-edge
             thin-stil
             X
             LEFT
             thin-stil
             (* (magstep font-size) thinkern)))
           (font
            (ly:paper-get-font layout
                               (cons '((font-encoding . fetaMusic)) props)))
           (segno (ly:font-get-glyph font "scripts.varsegno")))
      (ly:stencil-add
       segno
       (centered-stencil double-line-stil))))

  (let* ((line-thickness (ly:output-def-lookup layout 'line-thickness))
         (thin-thick (* line-thickness hair-thickness))
         (scaled-kern (* (magstep font-size) (* line-thickness kern)))
         (thick-thickness (* line-thickness thick-thickness))
         (blot (ly:output-def-lookup layout 'blot-diameter))
         (strg-list (string->string-list strg))
         (raw-bar-line-stencils
          (lambda (x)
            (ly:stencil-aligned-to
             (cond
              ((string=? ":" x)
               (make-colon-bar-line height font-size))
              ((string=? "|" x)
               (make-bar-line thin-thick height font-size blot))
              ((string=? "." x)
               (make-bar-line thick-thickness height font-size blot))
              ((string=? ";" x)
               (make-dotted-bar-line height font-size))
              ((string=? "!" x)
               (make-dashed-bar-line thin-thick height font-size blot))
              ((string=? "[" x)
               (make-bracket-bar-line height font-size LEFT))
              ((string=? "]" x)
               (make-bracket-bar-line height font-size RIGHT))
              ((string=? "{" x)
               (make-brace-bar-line height LEFT font-size))
              ((string=? "}" x)
               (make-brace-bar-line height RIGHT font-size))
              ((string=? "S" x)
               (make-segno-bar-line height font-size blot))
              (else
               (begin
                 (ly:warning "No bar line stencil found for '~a'" x)
                 empty-stencil)))
             Y CENTER)))
         (single-bar-line-stils
          (remove ly:stencil-empty? (map raw-bar-line-stencils strg-list))))
    (stack-stencils X RIGHT scaled-kern single-bar-line-stils)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the note command.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: better syntax.

(define-markup-command (note-by-number layout props log dot-count dir)
  (number? number? number?)
  #:category music
  #:properties ((font-size 0)
                (flag-style '())
                (dots-direction 0)
                (style '()))
  ;; TODO: as-string?
  "
@cindex note, within text, by @code{log} and @code{dot-count}

Draw a note of length @var{log}, with @var{dot-count} dots and a stem pointing
into direction @var{dir}.

By using fractional values for @var{dir}, longer or shorter stems can be
obtained.

Ancient note-head styles (via the @code{style} property, @pxref{Note head
styles}) get mensural-style flags by default; use @code{flag-style} to override
this.  Supported flag styles are @code{default}, @code{old-straight-flag},
@code{modern-straight-flag}, @code{flat-flag}, @code{stacked}, @code{mensural}, and
@code{neomensural}.  The last flag style is the same as @code{mensural} and
provided for convenience.

@lilypond[verbatim,quote]
\\markup {
  \\note-by-number #3 #0 #DOWN
  \\hspace #2
  \\note-by-number #1 #2 #0.8
  \\hspace #2
  \\override #'(style . petrucci)
  \\note-by-number #3 #0 #UP
  \\hspace #2
  \\override #'(flag-style . modern-straight-flag)
  \\note-by-number #4 #0 #DOWN
}
@end lilypond"
  (define (get-glyph-name-candidates dir log style)
    (map (lambda (dir-name)
           (format #f "noteheads.~a~a"
                   dir-name
                   (if (and (symbol? style)
                            (not (equal? 'default style)))
                       (select-head-glyph style (min log 2))
                       (min log 2))))
         (list (if (= dir UP) "u" "d")
               "s")))

  (define (get-glyph-name font cands)
    (if (null? cands)
        ""
        (if (ly:stencil-empty? (ly:font-get-glyph font (car cands)))
            (get-glyph-name font (cdr cands))
            (car cands))))

  (define (buildflags flag-stencil remain curr-stencil spacing)
    ;; Function to recursively create a stencil with @code{remain} flags
    ;; from the single-flag stencil @code{curr-stencil}, which is already
    ;; translated to the position of the previous flag position.
    ;;
    ;; Copy and paste from /scm/flag-styles.scm
    (if (> remain 0)
        (let* ((translated-stencil
                (ly:stencil-translate-axis curr-stencil spacing Y))
               (new-stencil (ly:stencil-add flag-stencil translated-stencil)))
          (buildflags new-stencil (- remain 1) translated-stencil spacing))
        flag-stencil))

  (define (straight-flag-mrkp flag-thickness flag-spacing
                              upflag-angle upflag-length
                              downflag-angle downflag-length
                              dir)
    ;; Create a stencil for a straight flag.  @var{flag-thickness} and
    ;; @var{flag-spacing} are given in staff spaces, @var{upflag-angle} and
    ;; @var{downflag-angle} are given in degrees, and @var{upflag-length} and
    ;; @var{downflag-length} are given in staff spaces.
    ;;
    ;; All lengths are scaled according to the font size of the note.
    ;;
    ;; From /scm/flag-styles.scm, modified to fit here.

    (let* ((stem-up (> dir 0))
           (staff-space (ly:output-def-lookup layout 'staff-space))
           ;; scale with font-size and staff-space
           (factor (* staff-space (magstep font-size)))
           (stem-thickness (* factor 0.1))
           (line-thickness (ly:output-def-lookup layout 'line-thickness))
           (half-stem-thickness (/ (* stem-thickness line-thickness) 2))
           (raw-length (if stem-up upflag-length downflag-length))
           (angle (if stem-up upflag-angle downflag-angle))
           (flag-length (+ (* raw-length factor) half-stem-thickness))
           (flag-end (polar->rectangular flag-length angle))
           (thickness (* flag-thickness factor))
           (thickness-offset (cons 0 (* -1 thickness dir)))
           (spacing (* -1 flag-spacing factor dir))
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
           (flag-stencil (buildflags stencil (- log 3) stencil spacing)))
      flag-stencil))

  (let* ((font (ly:paper-get-font layout (cons '((font-encoding . fetaMusic)
                                                 (font-name . #f))
                                               props)))
         ;; To make the stem scale properly with changes in
         ;; set-global-staff-size and/or set-layout-staff-size, we need to catch
         ;; text-font-size from current layout and $defaultpaper and scale
         ;; stem-thickness and -length with the division
         ;; (/ layout-text-font-size paper-text-font-size) later.
         ;; Default for text-font-size is 11.
         (layout-text-font-size
          (ly:output-def-lookup layout 'text-font-size 11))
         (paper-text-font-size
          (ly:output-def-lookup
           (ly:parser-lookup '$defaultpaper)
           'text-font-size 11))
         (blot (ly:output-def-lookup layout 'blot-diameter))
         (layout-output-scale (ly:output-def-lookup layout 'output-scale))
         (paper-output-scale
          (ly:output-def-lookup
           (ly:parser-lookup '$defaultpaper)
           'output-scale))
         (staff-space (ly:output-def-lookup layout 'staff-space))
         ;; While `layout-set-staff-size', applied in a score-layout, changes
         ;; staff-space, it does not change staff-space while applied in \paper
         ;; of an explicit book.
         ;; Thus we compare the actual staff-space with the values of
         ;; output-scale from current layout and $defaultpaper
         (size-factor
          (if (eqv? (/ layout-output-scale paper-output-scale) staff-space)
              (magstep font-size)
              (/ (* paper-output-scale staff-space (magstep font-size))
                 layout-output-scale)))
         (head-glyph-name
          (let ((result (get-glyph-name font
                                        (get-glyph-name-candidates
                                         (sign dir) log style))))
            (if (string-null? result)
                ;; If no glyph name can be found, select default heads.
                ;; Though this usually means an unsupported style has been
                ;; chosen, it also prevents unrelated 'style settings from
                ;; other grobs (e.g., TextSpanner and TimeSignature) leaking
                ;; into markup.
                (get-glyph-name font
                                (get-glyph-name-candidates
                                 (sign dir) log 'default))
                result)))
         (head-glyph (ly:font-get-glyph font head-glyph-name))
         (ancient-flags?
          (member style
                  '(mensural neomensural petrucci semipetrucci blackpetrucci)))
         (attach-indices (ly:note-head::stem-attachment font head-glyph-name))
         (stem-length
          (* size-factor
             (/ layout-text-font-size paper-text-font-size)
             (max 3 (- log 1))))
         ;; With ancient-flags we want a tighter stem
         (stem-thickness
          (* size-factor
             (/ layout-text-font-size paper-text-font-size)
             (if ancient-flags? 0.1 0.13)))
         (stemy (* dir stem-length))
         (attach-off (cons (interval-index
                            (ly:stencil-extent head-glyph X)
                            (* (sign dir) (car attach-indices)))
                           ;; fixme, this is inconsistent between X & Y.
                           (* (sign dir)
                              (interval-index
                               (ly:stencil-extent head-glyph Y)
                               (cdr attach-indices)))))
         ;; For a tighter stem (with ancient-flags) the stem-width has to be
         ;; adjusted.
         (stem-X-corr
          (if (or ancient-flags?
                  (member flag-style '(mensural neomensural)))
              (* 0.5 dir stem-thickness) 0))
         (stem-glyph (and (> log 0)
                          (ly:round-filled-box
                           (ordered-cons (+ stem-X-corr (car attach-off))
                                         (+ stem-X-corr (car attach-off)
                                            (* (- (sign dir)) stem-thickness)))
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
         ;; Straight-flags. Values taken from /scm/flag-style.scm
         (modern-straight-flag (straight-flag-mrkp 0.48 1 -18 1.1 22 1.2 dir))
         (old-straight-flag (straight-flag-mrkp 0.48 1 -45 1.2 45 1.4 dir))
         (flat-flag (straight-flag-mrkp 0.48 1.0 0 1.0 0 1.0 dir))
         ;; Calculate a corrective to avoid a gap between
         ;; straight-flags and the stem.
         (flag-style-Y-corr (if (or (eq? flag-style 'modern-straight-flag)
                                    (eq? flag-style 'old-straight-flag)
                                    (eq? flag-style 'flat-flag))
                                (/ blot 10 (* -1 dir))
                                0))
         (flaggl
          (and (> log 2)
               (ly:stencil-translate
                (cond ((eq? flag-style 'modern-straight-flag)
                       modern-straight-flag)
                      ((eq? flag-style 'old-straight-flag)
                       old-straight-flag)
                      ((eq? flag-style 'flat-flag)
                       flat-flag)
                      (else
                       (ly:font-get-glyph
                        font
                        (format #f
                                (if (or (member flag-style
                                                '(mensural neomensural))
                                        (and ancient-flags?
                                             (null? flag-style)))
                                    ;; We use the '2' variants of the mensural
                                    ;; flags that don't have a quantized stem
                                    ;; length.
                                    "flags.mensural~a2~a"
                                    (if (eq? flag-style 'stacked)
                                        "flags.stacked~a~a"
                                        "flags.~a~a"))
                                (if (> dir 0) "u" "d")
                                log))))
                (cons (+ (car attach-off)
                         ;; For tighter stems (with ancient-flags) the
                         ;; flag has to be adjusted different.
                         (if (and (not ancient-flags?) (< dir 0))
                             stem-thickness
                             0))
                      (+ stemy flag-style-Y-corr))))))
    ;; If there is a flag on an upstem and the stem is short, move the dots
    ;; to avoid the flag.  16th notes get a special case because their flags
    ;; hang lower than any other flags.
    ;; Not with ancient flags or straight-flags.
    (if (and dots (> dir 0) (> log 2)
             (or (member flag-style '(default stacked)) (null? flag-style))
             (not ancient-flags?)
             (or (< dir 1.15) (and (= log 4) (< dir 1.3))))
        (set! dots (ly:stencil-translate-axis dots 0.5 X)))
    (if flaggl
        (set! stem-glyph (ly:stencil-add flaggl stem-glyph)))
    (if (ly:stencil? stem-glyph)
        (set! stem-glyph (ly:stencil-add stem-glyph head-glyph))
        (set! stem-glyph head-glyph))
    (if (ly:stencil? dots)
        (set! stem-glyph
              (ly:stencil-add
               (ly:stencil-translate
                dots
                (cons (+ (cdr (ly:stencil-extent head-glyph X)) dotwid)
                      ;; dots-direction unit is _half_ staff spaces
                      (/ dots-direction 2)))
               stem-glyph)))
    stem-glyph))

(define-markup-command (note layout props duration dir)
  (ly:duration? number?)
  #:category music
  #:properties (note-by-number-markup)
  ;; TODO: as-string?
  "
@cindex note, within text, by duration

Draw a note of given @var{duration} with a stem pointing into direction
@var{dir}.

@var{duration} gives the note head type and augmentation dots; @var{dir}
controls both the direction and length of the stem.

See also function @code{\\note-by-number}.

@lilypond[verbatim,quote]
\\markup {
  \\note {4..} #UP
  \\hspace #2
  \\override #'(style . cross)
  \\note {4..} #0.75
  \\hspace #2
  \\note {\\breve} #0
}
@end lilypond"
  (note-by-number-markup layout props
                         (ly:duration-log duration)
                         (ly:duration-dot-count duration)
                         dir))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the rest command.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-markup-command (rest-by-number layout props log dot-count)
  (integer? integer?)
  #:category music
  #:properties ((font-size 0)
                (ledgers '(-1 0 1))
                (style '()))
  ;; TODO: as-string?
  "
@cindex rest, within text, by @code{log} and @code{dot-count}

Draw a rest of length @var{log}, with @var{dot-count} dots.

For duration logs that appear in the @code{ledgers} property, rest symbols with
ledger lines are selected.

@lilypond[verbatim,quote]
\\markup {
  \\rest-by-number #3 #2
  \\hspace #2
  \\rest-by-number #0 #1
  \\hspace #2
  \\rest-by-number #-1 #0
  \\hspace #2
  \\override #'(ledgers . ())
  \\rest-by-number #-1 #0
}
@end lilypond"

  (define (get-glyph-name-candidates log style)
    (let* (;; Choose the style-string to be added.
           ;; If no glyph exists, select others for the specified styles
           ;; otherwise defaulting.
           (style-strg
            (cond (
                   ;; 'baroque needs to be special-cased, otherwise
                   ;; `select-head-glyph' would catch neomensural-glyphs for
                   ;; this style, if (< log 0).
                   (eq? style 'baroque)
                   (string-append (number->string log) ""))
                  ((eq? style 'petrucci)
                   (string-append (number->string log) "mensural"))
                  ;; In other cases `select-head-glyph' from output-lib.scm
                  ;; works for rest-glyphs, too.
                  ((and (symbol? style) (not (eq? style 'default)))
                   (select-head-glyph style log))
                  (else log)))
           ;; Choose ledgered glyphs for whole and half rest.
           ;; Except for the specified styles and logs.
           (ledger-style-rests
            (if (and (or (list? style)
                         (not (member style
                                      '(neomensural mensural petrucci))))
                                        ;(or (= log -1) (= log 0) (= log 1))
                     (member log ledgers)
                     )
                "o"
                "")))
      (format #f "rests.~a~a" style-strg ledger-style-rests)))

  (define (get-glyph-name font cands)
    (if (ly:stencil-empty? (ly:font-get-glyph font cands))
        ""
        cands))

  (let* ((font
          (ly:paper-get-font layout
                             (cons '((font-encoding . fetaMusic)
                                     (font-name . #f))
                                   props)))
         (rest-glyph-name-candidate
          (get-glyph-name font
                          (get-glyph-name-candidates log style)))
         (rest-glyph-name
          (if (string-null? rest-glyph-name-candidate)
              ;; If no glyph name can be found, select default rests.  Though
              ;; this usually means an unsupported style has been chosen, it
              ;; also prevents unrelated 'style settings from other grobs
              ;; (e.g., TextSpanner and TimeSignature) leaking into markup.
              ;; If even for default style no rest can be found, warn and return
              ;; an empty string.
              (let* ((default-candidate
                       (get-glyph-name-candidates log 'default))
                     (default-glyph
                       (get-glyph-name font default-candidate)))
                (if (string-null? default-glyph)
                    (ly:warning "Cannot find glyph ~a" default-candidate))
                default-glyph)
              rest-glyph-name-candidate))
         (rest-glyph (ly:font-get-glyph font rest-glyph-name))
         (dot (ly:font-get-glyph font "dots.dot"))
         (dot-width (interval-length (ly:stencil-extent dot X)))
         (dots (and (> dot-count 0)
                    (apply ly:stencil-add
                           (map (lambda (x)
                                  (ly:stencil-translate-axis
                                   dot (* 2 x dot-width) X))
                                (iota dot-count))))))
    ;; Apart from mensural-, neomensural- and petrucci-style ledgered
    ;; glyphs are taken for whole and half rests.
    ;; If they are dotted, move the dots in X-direction to avoid collision.
    (if (and dots
             (< log 2)
             (>= log 0)
             (not (member style '(neomensural mensural petrucci))))
        (set! dots (ly:stencil-translate-axis dots dot-width X)))

    ;; Add dots to the rest-glyph.
    ;;
    ;; Not sure how to vertical align dots.
    ;; For now the dots are centered for half, whole or longer rests.
    ;; Otherwise placed near the top of the rest.
    ;;
    ;; Dots for rests with (< log 0) dots are allowed.
    (if dots
        (set! rest-glyph
              (ly:stencil-add
               (ly:stencil-translate
                dots
                (cons
                 (+ (cdr (ly:stencil-extent rest-glyph X)) dot-width)
                 (if (< log 2)
                     (interval-center (ly:stencil-extent rest-glyph Y))
                     (- (interval-end (ly:stencil-extent rest-glyph Y))
                        (/ (* 2 dot-width) 3)))))
               rest-glyph)))
    rest-glyph))

(define-markup-command
  (multi-measure-rest-by-number layout props length)
  (index?)
  #:category music
  #:properties ((font-size 0)
                (style '())
                (word-space)
                (thick-thickness 6.6)
                (hair-thickness 2.0)
                (expand-limit 10)
                (width 8)
                (multi-measure-rest-number #t))
  ;; TODO: as-string?
  "
@cindex multi-measure rest, within text, by number of measures

Return a multi-measure rest symbol for @var{length} measures.

If the number of measures is greater than the number given by
@code{expand-limit} a thick horizontal line is printed.  For every multi-measure
rest lasting more than one measure a number is printed on top.  However, if
property @code{multi-@/measure-@/rest-@/number} is set to @code{#t}, this number
gets suppressed.

@lilypond[verbatim,quote]
\\markup {
  Multi-measure rests may look like
  \\multi-measure-rest-by-number #12
  or
  \\override #'(multi-measure-rest-number . #f)
    \\multi-measure-rest-by-number #7
  (church rests)
}
@end lilypond"

  (define (mmr-numbers nmbr)
    "A multi-measure rest may contain glyphs representing durations of 8, 4, 2
and 1 measure.  Calculates a list containing the amounts of each glyph needed
for a multi-measure rest of the length given with @var{nmbr}.
Example: A multi-measure rest of 15 measures contains one glyphs for
8@tie{}bars, one glyph for 4@tie{}bars, one glyph for 2@tie{}bars and one glyph
for 1@tie{}bar, i.e.
@code{(mmr-numbers 15)} returns @code{'(1 1 1 1)}."
    (define (helper i init l)
      (if (not (integer? init))
          (reverse l)
          (helper (remainder i init) (/ init 2) (cons (quotient i init) l))))
    ;; longest mmr-glyph represents eight measures, thus init is 8
    (helper nmbr 8 '()))

  (define (get-glyph-name-candidates dur-log style)
    "Returns a string with the name of a rest glyph corresponding to
@var{dur-log}.  @var{style} specifies the suffix of the glyph: If @var{style} is
a symbol but not @code{'default}, choose this @var{style}.  @code{'petrucci} is
special-cased to return @code{'mensural}.  If @var{style} is @code{'()} or
@code{'default} no suffix is used.  The found glyph may not exist in the current
font.  In this case it gets replaced by a glyph with @var{style] set to
@code{'default} in a different procedure later on."
    (let* ((style-strg
            (cond ((eq? style 'petrucci) 'mensural)
                  ((and (symbol? style) (not (eq? style 'default)))
                   style)
                  (else ""))))
      (format #f "rests.~a~a~a"
              (if (zero? dur-log) "" "M")
              dur-log
              style-strg)))

  (let ((mmr-stil empty-stencil)
        (staff-space (ly:output-def-lookup layout 'staff-space)))
    ;; if the MMR is longer then the amount of measures provided by
    ;; `expand-limit` print a horizontal line
    ;; otherwise compose the MMR from selected glyphs
    (if (> length expand-limit)
        (let* ((blot (ly:output-def-lookup layout 'blot-diameter))
               (line-thickness (ly:output-def-lookup layout 'line-thickness))
               (thick-thick (* thick-thickness line-thickness))
               (half-thick-thick (/ thick-thick 2))
               (hair-thick (* hair-thickness line-thickness))
               (half-hair-thick (/ hair-thick 2)))
          (set! mmr-stil
                (ly:stencil-add
                 (ly:round-filled-box
                  (cons 0 width)
                  (cons (- half-thick-thick) half-thick-thick)
                  blot)
                 (ly:round-filled-box
                  (cons (- half-hair-thick) half-hair-thick)
                  (cons (- staff-space) staff-space)
                  blot)
                 (ly:round-filled-box
                  (cons (- width half-hair-thick) (+ width half-hair-thick))
                  (cons (- staff-space) staff-space)
                  blot))))
        (let* (;; get a list containing the multipliers of the needed glyphs for
               ;; 8-, 4-, 2-, 1-measure.
               (counted-glyphs-list (mmr-numbers length))
               ;; get a nested list for the duration-log of each needed glyph.
               ;; example: for a 7-bar MMR it returns '(() (2) (1) (0))
               ;; the sublist may contain multiple entries if needed
               ;; example: for a 16-bar MMR it returns '((3 3) () () ())
               (dur-log-amounts
                ;; (iota 4 3 -1) is the list of possible duration-logs for MMRs
                (map make-list counted-glyphs-list (iota 4 3 -1)))
               ;; get a flat list of found MMR-glyphs-candidates
               (glyph-string-list
                (append-map
                 (lambda (x)
                   (if (null? x)
                       (list "")
                       (map
                        (lambda (y) (get-glyph-name-candidates y style))
                        x)))
                 dur-log-amounts))
               ;; ensure current font is 'fetaMusic, deny any font-name setting
               ;; from elsewhere
               (font
                (ly:paper-get-font
                 layout
                 (cons '((font-encoding . fetaMusic)
                         (font-name . #f))
                       props)))
               ;; get a list of glyph-stencils, ready to build the final MMR
               (glyph-stils
                (map
                 (lambda (count cand)
                   ;; examine the glyph-candidate:
                   ;; if not found in current font replace it with a
                   ;; default-style glyph
                   (let* ((stil-cand (ly:font-get-glyph font cand))
                          (stil
                           (if (ly:stencil-empty? stil-cand)
                               (ly:font-get-glyph
                                font
                                (get-glyph-name-candidates count 'default))
                               stil-cand)))
                     ;; Return false for a string-null-candidate, will be
                     ;; filtered later on.
                     ;; If duration-log of the MMR-glyph is zero move it up by
                     ;; one staff-space
                     (if (string-null? cand)
                         #f
                         (ly:stencil-translate-axis
                          stil
                          (if (zero? count) staff-space 0)
                          Y))))
                 (iota 4 3 -1)
                 glyph-string-list)))
          ;; `stack-stencil-line` removes non-stencils
          (set! mmr-stil (stack-stencil-line word-space glyph-stils))))

    ;; Print the number above a multi-measure-rest.
    ;; Depends on duration, style and multi-measure-rest-number set #t
    (if (or (> length expand-limit)
            (and multi-measure-rest-number
                 (> length 1)
                 (not (member style '(neomensural mensural petrucci)))))
        (let* ((mmr-stil-x-center
                (interval-center (ly:stencil-extent mmr-stil X)))
               (duration-markup
                (make-fontsize-markup -2
                                      (make-override-markup '(font-encoding . fetaText)
                                                            (number->string length))))
               (mmr-number-stil
                (interpret-markup layout props duration-markup))
               (mmr-number-stil-x-center
                (interval-center (ly:stencil-extent mmr-number-stil X))))

          (set! mmr-stil
                (ly:stencil-combine-at-edge
                 mmr-stil
                 Y UP
                 (ly:stencil-translate-axis
                  mmr-number-stil
                  (- mmr-stil-x-center mmr-number-stil-x-center)
                  X)
                 ;; Ugh, hard-coded
                 (if (> length expand-limit) 0 0.8)))))
    mmr-stil))

(define-markup-command (rest layout props duration)
  (ly:duration?)
  #:category music
  #:properties (rest-by-number-markup
                multi-measure-rest-by-number-markup)
  ;; TODO: as-string?
  "
@cindex rest, within text, by duration
@cindex multi-measure rest, within text, by duration

Return a rest symbol with length @var{duration}.

If the @code{multi-measure-rest} property is set to @code{#t}, a multi-measure
rest symbol may be returned.  In this case the duration needs to be entered as
@code{@{ 1*@var{N} @}} to get a multi-measure rest for @var{N}@tie{}bars.
Actually, only the scaling factor (i.e., the number after @samp{*}) determines
the length; the basic duration is disregarded.

See also functions @code{\\rest-@/by-@/number} and
@code{\\multi-@/measure-@/rest-@/by-@/number} for more information on the used
properties.

@lilypond[verbatim,quote]
\\markup {
  Rests:
  \\hspace #2
  \\rest { 4.. }
  \\hspace #2
  \\rest { \\breve }
  \\hspace #2
  Multi-measure rests:
  \\override #'(multi-measure-rest . #t)
  {
    \\hspace #2
    \\rest { 1*7 }
    \\hspace #2
    \\rest { 1*12 }
  }
}
@end lilypond"
  (let ((duration-scale (ly:duration-scale duration))
        (mmr? (chain-assoc-get 'multi-measure-rest props)))
    (if (and (index? duration-scale) mmr?)
        (multi-measure-rest-by-number-markup layout props duration-scale)
        (rest-by-number-markup layout props
                               (ly:duration-log duration)
                               (ly:duration-dot-count duration)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fermata markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-markup-command (fermata layout props) ()
  #:category music
  #:properties ((direction UP))
  #:as-string (ly:wide-char->utf-8
               (if (eqv? direction UP)
                   #x1d110
                   #x1d111))
  "Create a fermata glyph.

If property @code{direction} is @code{DOWN}, use an inverted glyph.

Note that within music, one would normally use the @code{\\fermata} articulation
instead of a markup.

@lilypond[verbatim,quote]
{ c''1^\\markup \\fermata d''1_\\markup \\fermata }

\\markup { \\fermata \\override #`(direction . ,DOWN) \\fermata }
@end lilypond
"
  (interpret-markup layout props
                    (make-musicglyph-markup
                     (if (eqv? direction DOWN)
                         "scripts.dfermata"
                         "scripts.ufermata"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; translating.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-markup-command (lower layout props amount arg)
  (number? markup?)
  #:category align
  "
@cindex lowering text

Lower @var{arg} by the distance @var{amount}.

A negative @var{amount} indicates raising; see also @code{\\raise}.

The argument to @code{\\lower} is the vertical displacement amount, measured
in (global) staff spaces, which is independent of the markup's current font
size.  If you need vertical movement that takes the font size into account, use
@code{\\translate-scaled} instead.

This function is normally used to move one element inside of a markup relative to
the other elements.  When using it on the whole markup, bear in mind that
spacing mechanisms that place the markup itself on the page could cancel this
shift.  Consider using grob properties such as @code{padding}, @code{Y-offset},
or @code{extra-@/offset}, or spacing variables such as
@code{markup-@/system-@/spacing}.

@lilypond[verbatim,quote]
\\markup {
  one
  \\lower #3 two
  three
}
@end lilypond"
  (ly:stencil-translate-axis (interpret-markup layout props arg)
                             (- amount) Y))

(define-markup-command (translate-scaled layout props offset arg)
  (number-pair? markup?)
  #:category align
  #:properties ((font-size 0))
  "
@cindex translating text
@cindex scaling text

Translate @var{arg} by @var{offset}, scaling the offset by the font size.

This function is normally used to move one element inside of a markup relative to
the other elements.  When using it on the whole markup, bear in mind that
spacing mechanisms that place the markup itself on the page could cancel this
shift.  Consider using grob properties such as @code{padding}, @code{X-offset},
@code{Y-offset} or @code{extra-@/offset}, or spacing variables such as
@code{markup-@/system-@/spacing}.

See also @code{\\translate}.

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

(define-markup-command (raise layout props amount arg)
  (number? markup?)
  #:category align
  "
@cindex raising text

Raise @var{arg} by the distance @var{amount}.

A negative @var{amount} indicates lowering, see also @code{\\lower}.

The argument to @code{\\raise} is the vertical displacement amount, measured
in (global) staff spaces, which is independent of the markup's current font
size.  If you need vertical movement that takes the font size into account, use
@code{\\translate-scaled} instead.

This function is normally used to move one element inside of a markup relative to
the other elements.  When using it on the whole markup, bear in mind that
spacing mechanisms that place the markup itself on the page could cancel this
shift.  Consider using grob properties such as @code{padding}, @code{Y-offset},
or @code{extra-@/offset}, or spacing variables such as
@code{markup-@/system-@/spacing}.

@lilypond[verbatim,quote]
\\markup { C \\small \\bold \\raise #1.0 9/7+ }
@end lilypond"
  (ly:stencil-translate-axis (interpret-markup layout props arg) amount Y))

(define-markup-command (fraction layout props arg1 arg2)
  (markup? markup?)
  #:category other
  #:properties ((font-size 0))
  #:as-string (format #f "~a/~a"
                      (markup->string arg1 #:layout layout #:props props)
                      (markup->string arg2 #:layout layout #:props props))
  "
@cindex creating text fraction

Make a fraction of markups @var{arg1} and @var{arg2}.

@lilypond[verbatim,quote]
\\markup {
  π ≈ \\fraction 355 113
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

(define-markup-command (normal-size-super layout props arg)
  (markup?)
  #:category font
  #:properties ((font-size 0))
  "
@cindex setting superscript, in standard font size

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
   (* 1.0 (magstep font-size)) Y))

(define-markup-command (super layout props arg)
  (markup?)
  #:category font
  #:properties ((font-size 0))
  "
@cindex superscript text

Set @var{arg} in superscript.

@lilypond[verbatim,quote]
\\markup { E = \\concat { mc \\super 2 } }
@end lilypond

See also @code{\\sub}."
  (ly:stencil-translate-axis
   (interpret-markup
    layout
    (cons `((font-size . ,(- font-size 3))) props)
    arg)
   (* 1.0 (magstep font-size)) ; original font-size
   Y))

(define-markup-command (translate layout props offset arg)
  (number-pair? markup?)
  #:category align
  "
@cindex translating text

Translate @var{arg} relative to its surroundings.

@var{offset} is a pair of numbers representing the displacement in the X and
Y@tie{}axes.  See also @code{\\translate-scaled}.

This function is normally used to move one element inside of a markup relative to
the other elements.  When using it on the whole markup, bear in mind that
spacing mechanisms that place the markup itself on the page could cancel this
shift.  Consider using grob properties such as @code{padding}, @code{X-offset},
@code{Y-offset} or @code{extra-@/offset}, or spacing variables such as
@code{markup-@/system-@/spacing}.

@lilypond[verbatim,quote]
\\markup {
  *
  \\translate #'(2 . 3)
    \\line { translated two spaces right, three up }
}
@end lilypond"
  (ly:stencil-translate (interpret-markup layout props arg)
                        offset))

(define-markup-command (sub layout props arg)
  (markup?)
  #:category font
  #:properties ((font-size 0))
  "
@cindex subscript text

Set @var{arg} in subscript.

@lilypond[verbatim,quote]
\\markup { \\concat { H \\sub 2 O } }
@end lilypond

See also @code{\\super}."
  (ly:stencil-translate-axis
   (interpret-markup
    layout
    (cons `((font-size . ,(- font-size 3))) props)
    arg)
   (* -0.75 (magstep font-size)) ; original font-size
   Y))

(define-markup-command (normal-size-sub layout props arg)
  (markup?)
  #:category font
  #:properties ((font-size 0))
  "
@cindex setting subscript, in standard font size

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
   (* -0.75 (magstep font-size))
   Y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; brackets.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-markup-command (hbracket layout props arg)
  (markup?)
  #:category graphic
  "
@cindex placing horizontal brackets, around text

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

(define-markup-command (bracket layout props arg)
  (markup?)
  #:category graphic
  "
@cindex placing vertical brackets, around text

Draw vertical brackets around @var{arg}.

@lilypond[verbatim,quote]
\\markup {
  \\bracket {
    \\note {2.} #UP
  }
}
@end lilypond"
  (let ((th 0.1) ;; todo: take from GROB.
        (m (interpret-markup layout props arg)))
    (bracketify-stencil m Y th (* 2.5 th) th)))

(define-markup-command (parenthesize layout props arg)
  (markup?)
  #:category graphic
  #:properties ((angularity 0)
                (padding)
                (size 1)
                (thickness 1)
                (line-thickness 0.1)
                (width 0.25))
  #:as-string (format #f "(~a)"
                      (markup->string arg #:layout layout #:props props))
  "
@cindex placing parentheses, around text

Draw parentheses around @var{arg}.

This is useful for parenthesizing a column containing several lines of text.

@lilypond[verbatim,quote]
\\markup {
  \\parenthesize
  \\column {
    foo
    bar
  }
  \\override #'(angularity . 2)
  \\parenthesize
  \\column {
    bah
    baz
  }
}
@end lilypond"
  (let* ((m (interpret-markup layout props arg))
         (scaled-width (* size width))
         (scaled-thickness
          (* line-thickness thickness))
         (half-thickness
          (min (* size 0.5 scaled-thickness)
               (* (/ 4 3.0) scaled-width)))
         (padding (or padding half-thickness)))
    (parenthesize-stencil
     m half-thickness scaled-width angularity padding)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Delayed markup evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-markup-command (page-ref layout props label gauge default)
  (symbol? markup? markup?)
  #:properties ((x-align 1))
  #:category other
  #:as-string ""
  "
@cindex referencing page number, in text

Print a page number reference.

@var{label} is the label set on the referenced page (using @code{\\label} or
@code{\\tocItem}), @var{gauge} a markup used to estimate the maximum width of
the page number, and @var{default} the value to display when @var{label} is not
found.

If the current book or book part is set to use roman numerals for page numbers,
the reference will be formatted accordingly -- in which case the @var{gauge}'s
width may require additional tweaking."
  (let* ((gauge-stencil (interpret-markup layout props gauge))
         (x-ext (ly:stencil-extent gauge-stencil X))
         (y-ext (ly:stencil-extent gauge-stencil Y))
         ;; Ugh -- code duplication with ly/toc-init.ly -vv
         (assoc-name-get
          (lambda (name ls)
            (do ((ls ls (cdr ls)) (result '() result))
                ((null? ls) result)
              (if (and (car ls) (eq? name (assoc-get 'name (cdar ls))))
                  (set! result (cons (car ls) result)))))))

    (ly:stencil-outline
     (ly:make-stencil
      `(delay-stencil-evaluation
        ,(delay (ly:stencil-expr
                 (let* ((table (ly:output-def-lookup layout 'label-page-table))
                        (alist-table (ly:output-def-lookup layout 'label-alist-table))
                        (retrieve-id (if (list? alist-table)
                                         (let ((entry (assoc-name-get label alist-table)))
                                           (if (null? entry)
                                               #f
                                               (caar entry)))
                                         #f))
                        (page-number (if (list? table)
                                         (assoc-get (or retrieve-id label) table)
                                         #f))
                        (number-type (ly:output-def-lookup layout 'page-number-type))
                        (page-markup (if page-number
                                         (number-format number-type page-number)
                                         default))
                        (page-stencil (interpret-markup layout props page-markup))
                        (gap (- (interval-length x-ext)
                                (interval-length (ly:stencil-extent page-stencil X))))
                        (space-from-left-gauge-edge (* (/ (1+ x-align) 2) gap)))
                   (interpret-markup layout props
                                     (make-line-markup
                                      (list
                                       (make-hspace-markup space-from-left-gauge-edge)
                                       page-markup)))))))
      x-ext
      y-ext)
     (make-filled-box-stencil x-ext y-ext))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scaling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-markup-command (scale layout props factor-pair arg)
  (number-pair? markup?)
  #:category graphic
  "
@cindex scaling markup
@cindex mirroring markup

Scale @var{arg}.

@var{factor-pair} is a pair of numbers representing the scaling factor of the X
and Y@tie{}axes.  Negative values may be used to produce mirror images.

@lilypond[verbatim,quote]
\\markup {
  \\line {
    \\scale #'(2 . 1)
    stretched
    \\scale #'(1 . -1)
    mirrored
  }
}
@end lilypond"
  (let ((stil (interpret-markup layout props arg))
        (sx (car factor-pair))
        (sy (cdr factor-pair)))
    (ly:stencil-scale stil sx sy)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repeating
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-markup-command (pattern layout props count axis space pattern)
  (index? index? number? markup?)
  #:category other
  #:as-string (string-join (make-list count
                                      (markup->string pattern
                                                      #:layout layout
                                                      #:props props))
                           " ")
  "Print a @var{pattern} markup @var{count} times.

Patterns are spaced apart by @var{space} (defined as for @code{\\hspace} or
@code{\\vspace}, respectively) and distributed on @var{axis}.

@lilypond[verbatim,quote]
\\markup \\column {
  \"Horizontally repeated:\"
  \\pattern #7 #X #2 \\flat
  \\null
  \"Vertically repeated:\"
  \\pattern #3 #Y #0.5 \\flat
}
@end lilypond"
  (let* ((pattern-stencil (interpret-markup layout props pattern))
         ;; \vspace uses a factor of 3 in contrast to \hspace
         (space (if (= axis X) space (* 3.0 space))))
    (stack-stencils axis 1 space (make-list count pattern-stencil))))

(define-markup-command (fill-with-pattern layout props space dir pattern left right)
  (number? ly:dir? markup? markup? markup?)
  #:category align
  #:properties ((word-space)
                (line-width))
  #:as-string (markup->string (list left right)
                              #:layout layout
                              #:props props)
  "Put @var{left} and @var{right} at the start and end of a line, respectively, and
fill the space inbetween with repeated @var{pattern} markups.

Patterns are spaced apart by @var{space} and aligned to direction @var{dir}.
The width of the line is given by the @code{line-width} property.

@lilypond[verbatim,quote,line-width=14\\cm]
\\markup \\column {
  \"right-aligned:\"
  \\fill-with-pattern #1 #RIGHT . first right
  \\fill-with-pattern #1 #RIGHT . second right
  \\null
  \"center-aligned:\"
  \\fill-with-pattern #1.5 #CENTER - left right
  \\null
  \"left-aligned:\"
  \\override #'(line-width . 50) {
    \\fill-with-pattern #2 #LEFT : left first
    \\fill-with-pattern #2 #LEFT : left second
  }
}
@end lilypond"
  (let* ((pattern-stencil (interpret-markup layout props pattern))
         (pattern-x-extent (ly:stencil-extent pattern-stencil X))
         (pattern-width (interval-length pattern-x-extent))
         (left-stencil (interpret-markup layout props left))
         (left-width (interval-length (ly:stencil-extent left-stencil X)))
         (right-stencil (interpret-markup layout props right))
         (right-width (interval-length (ly:stencil-extent right-stencil X)))
         (middle-width (max 0 (- line-width (+ (+ left-width right-width) (* word-space 2)))))
         (period (+ space pattern-width))
         (count (inexact->exact (truncate (/ (- middle-width pattern-width) period))))
         (x-offset (+ (* (- (- middle-width (* count period)) pattern-width) (/ (1+ dir) 2)) (abs (car pattern-x-extent)))))
    (interpret-markup layout props
                      (make-line-markup
                       (list
                        (make-stencil-markup left-stencil)
                        (make-with-dimensions-markup
                         (cons 0 middle-width)
                         '(0 . 0)
                         (make-translate-markup
                          (cons x-offset 0)
                          (make-pattern-markup
                           (1+ count) X space
                           (make-stencil-markup pattern-stencil))))
                        (make-stencil-markup right-stencil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Replacements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-markup-command (replace layout props replacements arg)
  (list? markup?)
  #:category font
  #:properties ((replacement-alist))
  ;; TODO: is there something we can do for #:as-string?
  "Use @var{replacements} to replace strings in @var{arg}.

Each @code{(@var{key} . @var{value})} pair of the @var{replacements} alist
specifies what should be replaced; @var{key} gets replaced by @var{value}.
Note the quasi-quoting syntax with a backquote in the second example.

@lilypond[verbatim,quote]
\\markup \\replace #'((\"2nd\" . \"Second\"))
  \"2nd time\"
\\markup \\replace
  #`((\"2nd\" . ,#{ \\markup \\concat { 2 \\super nd } #}))
  \\center-column {
    \\line { Play only }
    \\line { the 2nd time }
  }
@end lilypond"
  (interpret-markup
   layout
   (prepend-alist-chain 'replacement-alist
                        (append replacement-alist replacements)
                        props)
   arg))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; conditionals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-markup-command (if layout props condition? argument)
  (procedure? markup?)
  #:category conditionals
  "Test @var{condition?}, and only insert @var{argument} if it is true.

The condition is provided as a procedure taking an output definition
and a property alist chain.  The procedure is applied, and its result
determines whether to print the markup.  This command is most useful inside
@code{oddHeaderMarkup} or similar.  Here is an example printing page
numbers in bold:

@example
\\paper @{
  oddHeaderMarkup =
    \\markup \\fill-line @{
      \"\"
      \\if #print-page-number
           \\bold \\fromproperty #'page:page-number-string
    @}
  evenHeaderMarkup =
    \\markup \\fill-line @{
      \\if #print-page-number
           \\bold \\fromproperty #'page:page-number-string
      \"\"
    @}
@}
@end example"
  (if (condition? layout props)
      (interpret-markup layout props argument)
      empty-stencil))

(define-markup-command (unless layout props condition? argument)
  (procedure? markup?)
  #:category conditionals
  "Test @var{condition?}, and only insert @var{argument} if it is false.

This function is similar to @code{\\if}; the following example shows how to
print the copyright notice on all pages but the last instead of just the first
page.

@example
\\paper @{
  oddFooterMarkup = \\markup @{
    \\unless #on-last-page-of-part \\fill-line @{
      \\fromproperty #'header:copyright
    @}
  @}
@}

\\header @{
  copyright = \"© LilyPond Authors. License: GFDL.\"
  tagline = \"© LilyPond Authors.  Documentation placed
under the GNU Free Documentation License
version 1.3.\"
@}
@end example"
  (if (condition? layout props)
      empty-stencil
      (interpret-markup layout props argument)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (props-with-tags-to-keep props tags)
  (let ((tags-list
          (ensure-list tags))
        (tags-to-keep (chain-assoc-get 'tags-to-keep props '())))
    (prepend-alist-chain 'tags-to-keep (cons tags-list tags-to-keep) props)))

(define-markup-command (keep-with-tag layout props tags arg)
  (symbol-list-or-symbol? markup?)
  #:category other
  #:properties ((tags-to-keep '()))
  #:as-string (markup->string
                arg
                #:layout layout
                #:props (props-with-tags-to-keep props tags))
  "Keep markup from @var{arg} that is untagged or tagged with @var{tags}.

All other parts of @var{arg} that are using a different tag
are replaced with empty stencils.
It works similar to @code{\\keepWithTag} for music,
but only with markups.

@lilypond[verbatim,quote]
tagged = \\markup {
  untagged
  \\tag #'foo A
  \\tag #'bar B
}

\\markup { \\keep-with-tag #'bar \\tagged }
\\markup { \\keep-with-tag #'foo \\tagged }
@end lilypond"
  (interpret-markup layout
    (props-with-tags-to-keep props tags)
    arg))

(define (props-with-tags-to-remove props tags)
  (let* ((tags-list
           (ensure-list tags))
         (tags-to-remove (chain-assoc-get 'tags-to-remove props '()))
         (new-tags-to-remove-list
           (lset-union eq? tags-to-remove tags-list)))
    (prepend-alist-chain 'tags-to-remove new-tags-to-remove-list props)))

(define-markup-command (remove-with-tag layout props tags arg)
  (symbol-list-or-symbol? markup?)
  #:category other
  #:properties ((tags-to-remove '()))
  #:as-string (markup->string
                arg
                #:layout layout
                #:props (props-with-tags-to-remove props tags))
  "Remove markup from @var{arg} that is tagged with @var{tags}.

The removed markup is replaced with empty stencils.
It works similar to @code{\\removeWithTag} for music,
but only with markups.

@lilypond[verbatim,quote]
tagged = \\markup {
  \\tag #'foo A
  \\tag #'bar B
}

\\markup { \\remove-with-tag #'foo \\tagged }
\\markup { \\remove-with-tag #'bar \\tagged }
@end lilypond"
  (interpret-markup layout
    (props-with-tags-to-remove props tags)
    arg))

(define (props-with-tag-additions mode props tag more)
  (let* ((prop-name (if (eq? mode 'push) 'tags-with-pushes-alist 'tags-with-appends-alist))
         (tags-with-additions-alist (chain-assoc-get prop-name props '()))
         (previous-additions (assq-ref tags-with-additions-alist tag))
         (additions (if (list? previous-additions)
                        (if (eq? mode 'push)
                            (append previous-additions (list more))
                            (cons more previous-additions))
                        (list more)))
         (new-tags-with-additions-alist
           (acons tag additions tags-with-additions-alist)))
    (prepend-alist-chain prop-name new-tags-with-additions-alist props)))

(define-markup-command (push-to-tag layout props tag more arg)
  (symbol? markup? markup?)
  #:category other
  #:properties ((tags-with-pushes-alist '()))
  #:as-string (markup->string
                arg
                #:layout layout
                #:props (props-with-tag-additions 'push props tag more))
  "Prepend @var{more} to all markup in @var{arg} tagged with @var{tag}.

It works similar to @code{\\pushToTag} for music,
but only with markups.

@lilypond[verbatim,quote]
tagged = \\markup {
  \\tag #'foo A
  \\tag #'bar B
}

\\markup { \\push-to-tag #'foo prefoo \\tagged }
@end lilypond"
  (interpret-markup layout
    (props-with-tag-additions 'push props tag more)
    arg))

(define-markup-command (append-to-tag layout props tag more arg)
  (symbol? markup? markup?)
  #:category other
  #:properties ((tags-with-appends-alist '()))
  #:as-string (markup->string
                arg
                #:layout layout
                #:props (props-with-tag-additions 'append props tag more))
  "Append @var{more} to all markup in var @var{arg} tagged with @var{tag}.

It works similar to @code{\\appendToTag} for music,
but only with markups.

@lilypond[verbatim,quote]
tagged = \\markup {
  \\tag #'foo A
  \\tag #'bar B
}

\\markup { \\append-to-tag #'foo postfoo \\tagged }
@end lilypond"
  (interpret-markup layout
    (props-with-tag-additions 'append props tag more)
    arg))

(define (tags-visible? tags tags-to-keep tags-to-remove)
  (let* ((tags-list
           (ensure-list tags))
         (should-remove-tag?
           (pair? (lset-intersection eq? tags-to-remove tags-list)))
         (should-keep-tag?
           (every
             (lambda (keep-tags)
               (or
                 (any (lambda (t) (memq t keep-tags)) tags-list)
                 (let ((groups (delete-duplicates (map tag-group-get keep-tags) eq?)))
                   (not (any (lambda (t) (memq (tag-group-get t) groups)) tags-list)))))
             tags-to-keep)))
    (and (not should-remove-tag?)
         should-keep-tag?)))

(define (combine-tag-markup-additions tags tags-with-pushes-alist tags-with-appends-alist tagged-markup-list)
  (let* ((tags-list (ensure-list tags))
         (tag-markup-additions
           (lambda (mode)
             (let ((tags-with-additions-alist
                     (if (eq? mode 'tags-with-pushes-alist)
                         tags-with-pushes-alist
                         tags-with-appends-alist)))
               (append-map
                 (lambda (tag)
                   (let ((new-tag-prop
                           (cons mode (acons tag '() tags-with-additions-alist)))
                         (markups
                           (assq-ref tags-with-additions-alist tag)))
                     (if (list? markups)
                         (make-override-lines-markup-list new-tag-prop markups)
                         '())))
                 tags-list)))))
    (append
      (tag-markup-additions 'tags-with-pushes-alist)
      tagged-markup-list
      (tag-markup-additions 'tags-with-appends-alist))))

(define-markup-command (tag layout props tags arg)
  (symbol-list-or-symbol? markup?)
  #:category other
  #:properties ((tags-to-keep '())
                (tags-to-remove '())
                (tags-with-pushes-alist '())
                (tags-with-appends-alist '()))
  #:as-string (if (tags-visible? tags tags-to-keep tags-to-remove)
                  (markup->string
                    (make-line-markup
                      (combine-tag-markup-additions
                        tags
                        tags-with-pushes-alist
                        tags-with-appends-alist
                        (list arg))))
                  "")
  "Tag markup @var{arg} with @var{tag}.

@var{tag} can be one or multiple tags.
This allows later on to reference @var{arg};
for example, to remove it or to add markup before or after
the tagged markup.
It works similar to @code{\\tag} for music,
but only with markups.

@lilypond[verbatim,quote]
tagged = \\markup {
  \\tag #'foo A
  \\tag #'bar B
}

\\markup { \\keep-with-tag #'bar \\tagged }
\\markup { \\keep-with-tag #'foo \\tagged }
@end lilypond"
  (if (tags-visible? tags tags-to-keep tags-to-remove)
    (interpret-markup layout props
      (make-line-markup
        (combine-tag-markup-additions
          tags
          tags-with-pushes-alist
          tags-with-appends-alist
          (list arg))))
    empty-stencil))


(define-markup-list-command (tag-list layout props tags arg)
  (symbol-list-or-symbol? markup-list?)
  #:category other
  #:properties ((tags-to-keep '())
                (tags-to-remove '())
                (tags-with-pushes-alist '())
                (tags-with-appends-alist '()))
  "Tag markup list @var{arg} with @var{tag}.

@var{tag} can be one or multiple tags.
This allows later on to reference @var{arg};
for example, to remove it or to add markup before or after
the tagged markup list.

It works like the @code{\\tag} command for markups but with markup lists.
You will need it if you have to reference a whole list; for example,
to use @code{\\push-to-tag} and @code{\\append-to-tag} without
pushing or appending before or after every single item of the list,
but before or after the whole list instead.

@lilypond[verbatim,quote]
tagged = \\markuplist {
  \\tag-list #'foo { foo bar }
}

\\markup { \\push-to-tag #'foo test \\tagged }
@end lilypond
"
  (if (tags-visible? tags tags-to-keep tags-to-remove)
    (interpret-markup-list layout props
      (combine-tag-markup-additions
        tags
        tags-with-pushes-alist
        tags-with-appends-alist
        arg))
    '()))

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

(define-markup-list-command (justified-lines layout props args)
  (markup-list?)
  #:properties ((baseline-skip)
                wordwrap-internal-markup-list)
  "
@cindex justifying lines of text

Print @var{args} as lines aligned both at the left and the right.

Like @code{\\justify}, but return a list of lines instead of a single markup.
Use @code{\\override-lines #'(line-width . @var{X})} to set the line width;
@var{X}@tie{}is the number of staff spaces."
  (space-lines baseline-skip
               (interpret-markup-list layout props
                                      (make-wordwrap-internal-markup-list #t args))))

(define-markup-list-command (wordwrap-lines layout props args)
  (markup-list?)
  #:properties ((baseline-skip)
                wordwrap-internal-markup-list)
  "Print @var{args} as left-aligned lines.

Like @code{\\wordwrap}, but return a list of lines instead of a single markup.
Use @code{\\override-lines #'(line-width . @var{X})} to set the line width,
where @var{X} is the number of staff spaces."
  (space-lines baseline-skip
               (interpret-markup-list layout props
                                      (make-wordwrap-internal-markup-list #f args))))

(define-markup-list-command (column-lines layout props args)
  (markup-list?)
  #:properties ((baseline-skip))
  "Stack the markups in @var{args} vertically.

Like @code{\\column}, but return a list of lines instead of a single markup.
The property @code{baseline-skip} determines the space between each markup in
@var{args}."
  (space-lines baseline-skip
               (interpret-markup-list layout props args)))

(define-markup-list-command (override-lines layout props new-prop args)
  (pair? markup-list?)
  "Add the argument @var{new-prop} to the property list for printing @var{args}.

Like @code{\\override} but for markup lists."
  (interpret-markup-list layout
                         (cons (if (pair? (car new-prop)) new-prop (list new-prop))
                               props)
                         args))

(define-markup-list-command (table layout props column-align lst)
  (number-list? markup-list?)
  #:properties ((padding 0)
                (baseline-skip))
  "
@cindex creating a table

Print a table.

@var{column-align} specifies how each column is aligned; possible values are -1,
0, and@tie{}1.  The number of elements in @var{column-align} determines how many
columns will be printed.

The entries to print are given by @var{lst}, a markup list.  If needed, the last
row is filled up with @code{point-stencil}s.

Override the @code{padding} property to increase the horizontal distance between
columns.  Override @code{baseline-skip} to increase the vertical distance
between rows.

@lilypond[verbatim,quote]
% A markup command to print a fixed-width number.
\\markup fwnum =
  \\markup \\override #'(font-features . (\"ss01\" \"-kern\"))
    \\number \\etc

\\markuplist {
  \\override #'(padding . 2)
  \\table #'(0 1 0 -1) {
    \\underline { center-aligned right-aligned
                 center-aligned left-aligned }
    one      \\fwnum    1 thousandth \\fwnum 0.001
    eleven   \\fwnum   11 hundredth  \\fwnum 0.01
    twenty   \\fwnum   20 tenth      \\fwnum 0.1
    thousand \\fwnum 1000 one        \\fwnum 1.0
  }
}
@end lilypond"

  (define (split-lst initial-lst lngth result-lst)
    ;; split a list into a list of sublists of length lngth
    ;; eg. (split-lst '(1 2 3 4 5 6) 2 '())
    ;; -> ((1 2) (3 4) (5 6))
    (cond ((not (integer? (/ (length initial-lst) lngth)))
           (ly:warning
            "Can't split list of length ~a into ~a parts, returning empty list"
            (length initial-lst) lngth)
           '())
          ((null? initial-lst)
           (reverse result-lst))
          (else
           (split-lst
            (drop initial-lst lngth)
            lngth
            (cons (take initial-lst lngth) result-lst)))))

  (define (dists-list init padding lst)
    ;; Returns a list, where each element of `lst' is
    ;; added to the sum of the previous elements of `lst' plus padding.
    ;; `init' will be the first element of the resulting list. The addition
    ;; starts with the values of `init', `padding' and `(car lst)'.
    ;; eg. (dists-list 0.01 0.1 '(1 2 3 4)))
    ;; -> (0.01 1.11 3.21 6.31 10.41)
    (if (or (not (number? init))
            (not (number? padding))
            (not (number-list? lst)))
        (begin
          (ly:warning
           "not fitting argument for `dists-list', return empty lst ")
          '())
        (reverse
         (fold (lambda (elem rl) (cons (+ elem padding (car rl)) rl))
               (list init)
               lst))))

  (let* (;; get the number of columns
         (columns (length column-align))
         (init-stils (interpret-markup-list layout props lst))
         ;; If the given markup-list is the result of a markup-list call, their
         ;; length may not be easily predictable, thus we add point-stencils
         ;; to fill last row of the table.
         (rem (remainder (length init-stils) columns))
         (filled-stils
          (if (zero? rem)
              init-stils
              (append init-stils (make-list (- columns rem) point-stencil))))
         ;; get the stencils in sublists of length `columns'
         (stils
          (split-lst filled-stils columns '()))
         ;; procedure to return stencil-length
         ;; If it is nan, return 0
         (lengths-proc
          (lambda (m)
            (let ((lngth (interval-length (ly:stencil-extent m X))))
              (if (nan? lngth) 0 lngth))))
         ;; get the max width of each column in a list
         (columns-max-x-lengths
          (map
           (lambda (x)
             (apply max 0
                    (map
                     lengths-proc
                     (map (lambda (l) (list-ref l x)) stils))))
           (iota columns)))
         ;; create a list of (basic) distances, which each column should
         ;; moved, using `dists-list'. Some padding may be added.
         (dist-sequence
          (dists-list 0 padding columns-max-x-lengths))
         ;; Get all stencils of a row, moved accurately to build columns.
         ;; If the items of a column are aligned other than left, we need to
         ;; move them to avoid collisions:
         ;; center aligned: move all items half the width of the widest item
         ;; right aligned: move all items the full width of the widest item.
         ;; Added to the default-offset calculated in `dist-sequence'.
         ;; `stencils-for-row-proc' needs four arguments:
         ;;    stil    - a stencil
         ;;    dist    - a numerical value as basic offset in X direction
         ;;    column  - a numerical value for the column we're in
         ;;    x-align - a numerical value how current column should be
         ;;              aligned, where (-1, 0, 1) means (LEFT, CENTER, RIGHT)
         (stencils-for-row-proc
          (lambda (stil dist column x-align)
            (ly:stencil-translate-axis
             (ly:stencil-aligned-to stil X x-align)
             (cond ((member x-align '(0 1))
                    (let* (;; get the stuff for relevant column
                           (stuff-for-column
                            (map
                             (lambda (s) (list-ref s column))
                             stils))
                           ;; get length of every column-item
                           (lengths-for-column
                            (map lengths-proc stuff-for-column))
                           (widest
                            (apply max 0 lengths-for-column)))
                      (+ dist (/ widest (if (= x-align 0) 2 1)))))
                   (else dist))
             X)))
         ;; get a list of rows using `ly:stencil-add' on a list of stencils
         (rows
          (map
           (lambda (stil-list)
             (apply ly:stencil-add
                    (map
                     ;; the procedure creating the stencils:
                     stencils-for-row-proc
                     ;; the procedure's args:
                     stil-list
                     dist-sequence
                     (iota columns)
                     column-align)))
           stils)))
    (space-lines baseline-skip rows)))

(define-markup-list-command (string-lines layout props str)(string?)
  #:properties ((split-char #\newline))
  "Split string @var{str} into lines.

The character to split at is specified by the property @code{split-char},
defaulting to @code{#\\newline}.  Surrounding whitespace is removed from every
resulting string.  The returned list of markups is ready to be formatted by
other markup or markup list commands like @code{\\column}, @code{\\line}, etc.

@lilypond[verbatim,quote]
\\markup {
  \\column
    \\string-lines
     \"foo, foo,
     bar, bar,
     buzz, buzz!\"
}
@end lilypond"
  (interpret-markup-list layout props
                         (map string-trim-both (string-split str split-char))))

(define-markup-list-command (map-markup-commands layout props compose args)
  (procedure? markup-list?)
  #:internal? #t
  ;; This applies the function @var{compose} to every markup in
  ;; args (including elements of markup list command calls) in order
  ;; to produce a new markup list.  Since the return value from a markup
  ;; list command call is not a markup list but rather a list of stencils,
  ;; this requires passing those stencils off as the results of individual
  ;; markup calls.  That way, the results should work out as long as no
  ;; markups rely on side effects.
  (let ((key (make-symbol "key")))
    (catch
     key
     (lambda ()
       ;; if `compose' does not actually interpret its markup
       ;; argument, we still need to return a list of stencils,
       ;; created from the single returned stencil
       (list
        (interpret-markup layout props
                          (compose
                           (make-on-the-fly-markup
                            (lambda (layout props m)
                              ;; here all effects of `compose' on the
                              ;; properties should be visible, so we
                              ;; call interpret-markup-list at this
                              ;; point of time and harvest its
                              ;; stencils
                              (throw key
                                     (interpret-markup-list
                                      layout props args)))
                            (make-null-markup))))))
     (lambda (key stencils)
       (map
        (lambda (sten)
          (interpret-markup layout props
                            (compose (make-stencil-markup sten))))
        stencils)))))
