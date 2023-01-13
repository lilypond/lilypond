;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2000--2022  Han-Wen Nienhuys <hanwen@xs4all.nl>
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
;;;     for documentation purpose, builtin markup commands are grouped by
;;;     category.  This can be any symbol.  When documentation is generated,
;;;     the symbol is converted to a capitalized string, where hyphens are
;;;     replaced by spaces.
;;;
;;;   property-bindings
;;;     this is used both for documentation generation, and to ease
;;;     programming the command itself.  It is list of
;;;        (property-name default-value)
;;;     or (property-name)
;;;     elements.  Each property is looked-up in the `props' argument, and
;;;     the symbol naming the property is bound to its value.
;;;     When the property is not found in `props', then the symbol is bound
;;;     to the given default value.  When no default value is given, #f is
;;;     used instead.
;;;     Thus, using the following property bindings:
;;;       ((thickness 0.1)
;;;        (font-size 0))
;;;     is equivalent to writing:
;;;       (let ((thickness (chain-assoc-get 'thickness props 0.1))
;;;             (font-size (chain-assoc-get 'font-size props 0)))
;;;         ..body..)
;;;     When a command `B' internally calls an other command `A', it may
;;;     desirable to see in `B' documentation all the properties and
;;;     default values used by `A'.  In that case, add `A-markup' to the
;;;     property-bindings of B.  (This is used when generating
;;;     documentation, but won't create bindings.)
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

(use-modules (ice-9 regex))

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
    "Put @var{args} in a horizontal line.  The property @code{word-space}
determines the space between markups in @var{args}.

@lilypond[verbatim,quote]
\\markup {
  \\line {
    one two three
  }
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

A simple line.
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

A dashed line.

If @code{full-length} is set to #t (default) the dashed-line extends to the
whole length given by @var{dest}, without white space at beginning or end.
@code{off} will then be altered to fit.
To insist on the given (or default) values of @code{on}, @code{off} use
@code{\\override #'(full-length . #f)}
Manual settings for @code{on},@code{off} and @code{phase} are possible.
@lilypond[verbatim,quote]
\\markup {
  \\draw-dashed-line #'(5.1 . 2.3)
  \\override #'((on . 0.3) (off . 0.5))
  \\draw-dashed-line #'(5.1 . 2.3)
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
    ;; ghostscript-error occurs while calling
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

A dotted line.

The dotted-line always extends to the whole length given by @var{dest}, without
white space at beginning or end.
Manual settings for @code{off} are possible to get larger or smaller space
between the dots.
The given (or default) value of @code{off} will be altered to fit the
line-length.
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

A squiggled line.

If @code{eq-end?} is set to @code{#t}, it is ensured the squiggled line ends
with a bow in same direction as the starting one.  @code{sq-length} is the
length of the first bow.  @code{dest} is the end point of the squiggled line.
To match @code{dest} the squiggled line is scaled accordingly.
Its appearance may be customized by overrides for @code{thickness},
@code{angularity}, @code{height} and @code{orientation}.
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
    \\draw-squiggle-line #0.5 #'(6 . 0) ##t
    \\override #'(angularity . 2)
    \\draw-squiggle-line #0.5 #'(6 . 0) ##t
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
  #:properties ((draw-line-markup)
                (line-width)
                (span-factor 1))
  "
@cindex drawing line, across a page

Draws a line across a page, where the property @code{span-factor}
controls what fraction of the page is taken up.
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

A circle of radius @var{radius} and thickness @var{thickness},
optionally filled.

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

A polygon delimited by the list of @var{points}.  @var{extroversion}
defines how the shape of the polygon is adapted to its thickness.
If it is@tie{}0, the polygon is traced as-is.  If@tie{}-1, the outer side
of the line is just on the given points.  If@tie{}1, the line has its
inner side on the points.  The @var{thickness} property controls the
thickness of the line; for filled polygons, this means the diameter
of the blot.

@lilypond[verbatim,quote]
regularPentagon =
  #'((1 . 0) (0.31 . 0.95) (-0.81 . 0.59)
     (-0.81 . -0.59) (0.31 . -0.95))

\\markup {
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

A triangle, either filled or empty.

@lilypond[verbatim,quote]
\\markup {
  \\triangle ##t
  \\hspace #2
  \\triangle ##f
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
                (circle-padding 0.2))
  ;; \circle <digit> and \circle <english_letter> could be transformed
  ;; to U+2460 etc., but there are a limited number of them, so it
  ;; would create inconsistencies when things outside the set are used.
  #:as-string (format #f "(~a)"
                      (markup->string arg #:layout layout #:props props))
  "
@cindex circling text

Draw a circle around @var{arg}.  Use @code{thickness},
@code{circle-padding} and @code{font-size} properties to determine line
thickness and padding around the markup.

@lilypond[verbatim,quote]
\\markup {
  \\circle {
    Hi
  }
}
@end lilypond"
  (let ((th (* (ly:output-def-lookup layout 'line-thickness)
               thickness))
        (pad (* (magstep font-size) circle-padding))
        (m (interpret-markup layout props arg)))
    (circle-stencil m th pad)))

(define-markup-command (ellipse layout props arg)
  (markup?)
  #:category graphic
  #:properties ((thickness 1)
                (font-size 0)
                (x-padding 0.2)
                (y-padding 0.2))
  "
@cindex drawing ellipse, around text

Draw an ellipse around @var{arg}.  Use @code{thickness},
@code{x-padding}, @code{y-padding} and @code{font-size} properties to determine
line thickness and padding around the markup.

@lilypond[verbatim,quote]
\\markup {
  \\ellipse {
    Hi
  }
}
@end lilypond"
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

Draw an oval around @var{arg}.  Use @code{thickness},
@code{x-padding}, @code{y-padding} and @code{font-size} properties to determine
line thickness and padding around the markup.

@lilypond[verbatim,quote]
\\markup {
  \\oval {
    Hi
  }
}
@end lilypond"
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

Add a link to URL @var{url} around @var{arg}.  This only works in
the PDF backend.

@lilypond[verbatim,quote]
\\markup {
  \\with-url #\"https://lilypond.org/\" {
    LilyPond ... \\italic {
      music notation for everyone
    }
  }
}
@end lilypond"
  (let* ((stil (interpret-markup layout props arg))
         (xextent (ly:stencil-extent stil X))
         (yextent (ly:stencil-extent stil Y))
         (old-expr (ly:stencil-expr stil))
         (url-expr `(url-link ,url ,xextent ,yextent)))

    (ly:stencil-add (ly:make-stencil url-expr xextent yextent) stil)))

(define-markup-command (page-link layout props page-number arg)
  (number? markup?)
  #:category other
  "
@cindex referencing page number, in text

Add a link to the page @var{page-number} around @var{arg}.  This only works
in the PDF backend.

@lilypond[verbatim,quote]
\\markup {
  \\page-link #2  { \\italic { This links to page 2... } }
}
@end lilypond"
  (let* ((stil (interpret-markup layout props arg))
         (xextent (ly:stencil-extent stil X))
         (yextent (ly:stencil-extent stil Y))
         (old-expr (ly:stencil-expr stil))
         (link-expr `(page-link ,page-number ,xextent ,yextent)))

    (ly:stencil-add (ly:make-stencil link-expr xextent yextent) stil)))

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

Add a link to the page holding label @var{label} around @var{arg}.  This
only works in the PDF backend.

@verbatim
\\markup {
  \\with-link #'label {
    \\italic { This links to the page
               containing the label... }
  }
}
@end verbatim"
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

Create a beam with the specified parameters.
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

Underline @var{arg}.  Looks at @code{thickness} to determine line
thickness, @code{offset} to determine line y-offset from @var{arg} and
@code{underline-skip} to determine the distance of additional lines from the
others.
@code{underline-shift} is used to get subsequent calls correct.  Overriding it
makes little sense, it would end up adding the provided value to the one of
@code{offset}.

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
  \\underline \\underline \\underline \"multiple underlined\"
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

Adds a horizontal bow created with @code{make-tie-stencil} at bottom or top
of @var{arg}.  Looks at @code{thickness} to determine line thickness, and
@code{offset} to determine y-offset.  The added bow fits the extent of
@var{arg}, @code{shorten-pair} may be used to modify this.
@var{direction} may be set using an @code{override} or direction-modifiers or
@code{voiceOne}, etc.

@lilypond[verbatim,quote]
\\markup {
  \\override #'(direction . 1)
  \\tie \"above\"
  \\override #'(direction . -1)
  \\tie \"below\"
}
@end lilypond"
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
@cindex enclosing text within a box

Draw a box round @var{arg}.  Looks at @code{thickness},
@code{box-padding} and @code{font-size} properties to determine line
thickness and padding around the markup.

@lilypond[verbatim,quote]
\\markup {
  \\override #'(box-padding . 0.5)
  \\box
  \\line { V. S. }
}
@end lilypond"
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

Draw a box with rounded corners of dimensions @var{xext} and
@var{yext}.  For example,
@verbatim
\\filled-box #'(-.3 . 1.8) #'(-.3 . 1.8) #0
@end verbatim
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
  "@cindex enclosing text in box, with rounded corners
   @cindex drawing box, with rounded corners, around text
Draw a box with rounded corners around @var{arg}.  Looks at @code{thickness},
@code{box-padding} and @code{font-size} properties to determine line
thickness and padding around the markup; the @code{corner-radius} property
makes it possible to define another shape for the corners (default is 1).

@lilypond[verbatim,quote,relative=2]
c4^\\markup {
  \\rounded-box {
    Overtura
  }
}
c,8. c16 c4 r
@end lilypond"
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

Rotate object with @var{ang} degrees around its center.

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
                (thickness '()))
  "
@cindex adding white background, to text

Provide a white background for @var{arg}.  The shape of the white
background is determined by @code{style}.  The default
is @code{box} which produces a rectangle.  @code{rounded-box}
produces a rounded rectangle.  @code{outline} approximates the
outline of the markup.

@lilypond[verbatim,quote]
\\markup {
  \\combine
    \\filled-box #'(-1 . 15) #'(-3 . 4) #1
    \\override #'(thickness . 1.5)
    \\whiteout whiteout-box
}
\\markup {
  \\combine
    \\filled-box #'(-1 . 24) #'(-3 . 4) #1
    \\override #'((style . rounded-box) (thickness . 3))
    \\whiteout whiteout-rounded-box
}
\\markup {
  \\combine
    \\filled-box #'(-1 . 18) #'(-3 . 4) #1
    \\override #'((style . outline) (thickness . 3))
    \\whiteout whiteout-outline
}
@end lilypond"
  (stencil-whiteout
   (interpret-markup layout props arg)
   style
   thickness
   (ly:output-def-lookup layout 'line-thickness)))

(define-markup-command (pad-markup layout props amount arg)
  (number? markup?)
  #:category align
  "
@cindex padding text
@cindex putting space around text

Add space around a markup object.
Identical to @code{pad-around}.

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
  (let* ((m (interpret-markup layout props arg))
         (x (interval-widen (ly:stencil-extent m X) amount))
         (y (interval-widen (ly:stencil-extent m Y) amount)))
    (ly:stencil-add (make-transparent-box-stencil x y)
                    m)))

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

Create an invisible object taking up horizontal space @var{amount}.

@lilypond[verbatim,quote]
\\markup {
  one
  \\hspace #2
  two
  \\hspace #8
  three
}
@end lilypond"
  (ly:make-stencil "" (cons 0 amount) empty-interval))

(define-markup-command (vspace layout props amount)
  (number?)
  #:category align
  "
@cindex creating vertical space, in text

Create an invisible object taking up vertical space
of @var{amount} multiplied by 3.

@lilypond[verbatim,quote]
\\markup {
    \\center-column {
    one
    \\vspace #2
    two
    \\vspace #5
    three
  }
}
@end lilypond"
  (let ((amount (* amount 3.0)))
    (ly:make-stencil "" empty-interval (cons 0 amount))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; importing graphics.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-markup-command (stencil layout props stil)
  (ly:stencil?)
  #:category other
  "
@cindex importing stencil, into text

Use a stencil as markup.

@lilypond[verbatim,quote]
\\markup {
  \\stencil #(make-circle-stencil 2 0 #t)
}
@end lilypond"
  stil)

(define bbox-regexp
  (make-regexp "%%BoundingBox:[ \t]+([0-9-]+)[ \t]+([0-9-]+)[ \t]+([0-9-]+)[ \t]+([0-9-]+)"))

(define-public (get-postscript-bbox string)
  "Extract the bounding box from @var{string}, or return @code{#f} if not
present."
  (let*
      ((match (regexp-exec bbox-regexp string)))

    (if match
        (map (lambda (x)
               (string->number (match:substring match x)))
             (cdr (iota 5)))

        #f)))

(define-markup-command (epsfile layout props axis size file-name)
  (number? number? string?)
  #:category graphic
  #:as-string ""
  "
@cindex inlining an Encapsulated PostScript image

Inline an EPS image.  The image is scaled along @var{axis} to
@var{size}.

@lilypond[verbatim,quote]
\\markup {
  \\general-align #Y #DOWN {
    \\epsfile #X #20 #\"context-example.eps\"
    \\epsfile #Y #20 #\"context-example.eps\"
  }
}
@end lilypond"
  (eps-file->stencil axis size file-name))

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

@item There are no stability guarantees on the details of how LilyPond produces
its own output (i.e., the context into which the PostScript code is inserted).
They may change substantially across versions.

@item LilyPond cannot understand the shape of the drawing, leading to
suboptimal spacing.

@item Depending on how you install LilyPond, the version of the PostScript
interpreter (GhostScript) can vary, and some of its features may be disabled.

@end itemize

@lilypond[verbatim,quote]
ringsps = #\"
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
Draws a path with line @var{thickness} according to the
directions given in @var{commands}.  @var{commands} is a list of
lists where the @code{car} of each sublist is a drawing command and
the @code{cdr} comprises the associated arguments for each command.

There are seven commands available to use in the list
@code{commands}: @code{moveto}, @code{rmoveto}, @code{lineto},
@code{rlineto}, @code{curveto}, @code{rcurveto}, and
@code{closepath}.  Note that the commands that begin with @emph{r}
are the relative variants of the other three commands.  You may also
use the standard SVG single-letter equivalents: @code{moveto} = @code{M},
@code{lineto} = @code{L}, @code{curveto} = @code{C},
@code{closepath} = @code{Z}.  The relative commands are written
lowercase: @code{rmoveto} = @code{r}, @code{rlineto} = @code{l},
@code{rcurveto} = @code{c}.

The commands @code{moveto}, @code{rmoveto}, @code{lineto}, and
@code{rlineto} take 2 arguments; they are the X and Y coordinates
for the destination point.

The commands @code{curveto} and @code{rcurveto} create cubic
Bézier curves, and take 6 arguments; the first two are the X and Y
coordinates for the first control point, the second two are the X
and Y coordinates for the second control point, and the last two
are the X and Y coordinates for the destination point.

The @code{closepath} command takes zero arguments and closes the
current subpath in the active path.

Line-cap styles and line-join styles may be customized by
overriding the @code{line-cap-style} and @code{line-join-style}
properties, respectively.  Available line-cap styles are
@code{'butt}, @code{'round}, and @code{'square}.  Available
line-join styles are @code{'miter}, @code{'round}, and
@code{'bevel}.

The property @code{filled} specifies whether or not the path is
filled with color.

@lilypond[verbatim,quote]
samplePath =
  #'((lineto -1 1)
     (lineto 1 1)
     (lineto 1 -1)
     (curveto -5 -5 -5 5 -1 0)
     (closepath))

\\markup {
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

(define-markup-list-command (score-lines layout props score)
  (ly:score?)
  "This is the same as the @code{\\score} markup but delivers its
systems as a list of lines.  Its @var{score} argument is entered in
braces like it would be for @code{\\score}."
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

Inline an image of music.  The reference point (usually the middle
staff line) of the lowest staff in the top system is placed on the
baseline.

@lilypond[verbatim,quote,line-width=14\\cm,staffsize=16]
\\markup {
  \\score {
    \\new PianoStaff <<
      \\new Staff \\relative c' {
        \\key f \\major
        \\time 3/4
        \\mark \\markup { Allegro }
        f2\\p( a4)
        c2( a4)
        bes2( g'4)
        f8( e) e4 r
      }
      \\new Staff \\relative c {
        \\clef bass
        \\key f \\major
        \\time 3/4
        f8( a c a c a
        f c' es c es c)
        f,( bes d bes d bes)
        f( g bes g bes g)
      }
    >>
    \\layout {
      indent = 0.0\\cm
      \\context {
        \\Score
        \\override RehearsalMark.break-align-symbols =
           #'(time-signature key-signature)
        \\override RehearsalMark.self-alignment-X = #LEFT
      }
      \\context {
        \\Staff
        \\override TimeSignature
                   .break-align-anchor-alignment = #LEFT
      }
    }
  }
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

An embedded rhythmic pattern.

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

@code{\\rhythm} works by creating a @code{StandaloneRhythmVoice}
context.  The parents of this context are @code{StandaloneRhythmStaff}
and @code{StandaloneRhythmScore}.  It is possible to apply global
tweaks to the output by using a @code{\\layout} block.

@lilypond[verbatim,quote]
\\layout {
  \\context {
    \\StandaloneRhythmVoice
    \\xNotesOn
  }
}

\\markup \\rhythm { 8 16 8 }
@end lilypond

@warning{@code{\\rhythm} does not work when its argument is a single
duration, e.g., @code{\\rhythm @{ 8 @}}.  Use extra braces:
@code{\\rhythm @{ @{ 8 @} @}}.}
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
  "@code{\\markup \\simple \"x\"} is equivalent to @code{\\markup \"x\"}.  This
command was previously used internally, but no longer is, and is being kept for
backwards compatibility only.
"
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

(define-public empty-markup
  (make-simple-markup ""))

;; helper for justifying lines.
(define (get-fill-space
         word-count line-width word-space text-widths constant-space?)
  "Calculate the necessary paddings between adjacent texts in a
single justified line.  The lengths of all texts are stored in
@var{text-widths}.
When @var{constant-space?} is @code{#t}, the formula for the padding
between texts is:
padding = (line-width - total-text-width)/(word-count - 1)
When @var{constant-space?} is @code{#f}, the formula for the
padding between interior texts a and b is:
padding = line-width/(word-count - 1) - (length(a) + length(b))/2
In this case, the first and last padding have to be calculated
specially using the whole length of the first or last text.
All paddings are checked to be at least word-space, to ensure that
no texts collide.
Return a list of paddings."
  (cond
   ((null? text-widths) '())
   (constant-space?
    (make-list
     (1- word-count)
     ;; Ensure that space between words cannot be
     ;; less than word-space.
     (max
      word-space
      (/ (- line-width (apply + text-widths))
         (1- word-count)))))

   ;; special case first padding
   ((= (length text-widths) word-count)
    (cons
     (- (- (/ line-width (1- word-count)) (car text-widths))
        (/ (cadr text-widths) 2))
     (get-fill-space
      word-count line-width word-space (cdr text-widths)
      constant-space?)))
   ;; special case last padding
   ((= (length text-widths) 2)
    (list (- (/ line-width (1- word-count))
             (+ (/ (car text-widths) 2) (cadr text-widths)))
          0))
   (else
    (let ((default-padding
            (- (/ line-width (1- word-count))
               (/ (+ (car text-widths) (cadr text-widths)) 2))))
      (cons
       (if (> word-space default-padding)
           word-space
           default-padding)
       (get-fill-space
        word-count line-width word-space (cdr text-widths)
        constant-space?))))))

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
            (get-fill-space
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
  "Put @var{markups} in a horizontal line of width @var{line-width}.
The markups are spaced or flushed to fill the entire line.
If there are no arguments, return an empty stencil.

@lilypond[verbatim,quote,line-width=14\\cm]
\\markup {
  \\column {
    \\fill-line {
      Words evenly spaced across the page
    }
    \\null
    \\fill-line {
      \\line { Text markups }
      \\line {
        \\italic { evenly spaced }
      }
      \\line { across the page }
    }
    \\null
    \\override #'(line-width . 50)
    \\fill-line {
      Width explicitly specified
    }
  }
}
@end lilypond"
  (justify-line-helper
   layout props args text-direction word-space line-width #f))

(define-markup-command (justify-line layout props args)
  (markup-list?)
  #:category align
  #:properties ((text-direction RIGHT)
                (word-space 0.6)
                (line-width #f))
  "Put @var{markups} in a horizontal line of width @var{line-width}.
The markups are spread to fill the entire line and separated by equal
space.  If there are no arguments, return an empty stencil.

@lilypond[verbatim,quote,line-width=14\\cm]
\\markup {
  \\justify-line {
    Constant space between neighboring words
  }
}
@end lilypond"
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
    one
    two
    three
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
  "Perform simple wordwrap, return stencil of each line."
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
  "Internal markup list command used to define @code{\\justify} and @code{\\wordwrap}."
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
@end lilypond"
  (stack-lines DOWN 0.0 baseline-skip
               (wordwrap-internal-markup-list layout props #t args)))

(define-markup-command (wordwrap layout props args)
  (markup-list?)
  #:category align
  #:properties ((baseline-skip)
                wordwrap-internal-markup-list)
  "Simple wordwrap.  Use @code{\\override #'(line-width . @var{X})} to set
the line width, where @var{X} is the number of staff spaces.

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
@end lilypond"
  (stack-lines DOWN 0.0 baseline-skip
               (wordwrap-internal-markup-list layout props #f args)))

(define-markup-list-command (wordwrap-string-internal layout props justify arg)
  (boolean? string?)
  #:properties ((line-width)
                (word-space)
                (text-direction RIGHT))
  "Internal markup list command that is used to define @code{\\justify-string}
and @code{\\wordwrap-string}."
  (let* ((para-strings (regexp-split
                        (string-regexp-substitute
                         "\r" "\n"
                         (string-regexp-substitute "\r\n" "\n" arg))
                        "\n[ \t\n]*\n[ \t\n]*"))
         (list-para-words (map (lambda (str)
                                 (regexp-split str "[ \t\n]+"))
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
  "Wordwrap a string.  Paragraphs may be separated with double newlines.

@lilypond[verbatim,quote]
\\markup {
  \\override #'(line-width . 40)
  \\wordwrap-string #\"Lorem ipsum dolor sit amet,
      consectetur adipisicing elit, sed do eiusmod tempor
      incididunt ut labore et dolore magna aliqua.


      Ut enim ad minim veniam, quis nostrud exercitation
      ullamco laboris nisi ut aliquip ex ea commodo
      consequat.


      Excepteur sint occaecat cupidatat non proident,
      sunt in culpa qui officia deserunt mollit anim id
      est laborum\"
}
@end lilypond"
  (stack-lines DOWN 0.0 baseline-skip
               (wordwrap-string-internal-markup-list layout props #f arg)))

(define-markup-command (justify-string layout props arg)
  (string?)
  #:category align
  #:properties ((baseline-skip)
                wordwrap-string-internal-markup-list)
  "Justify a string.  Paragraphs may be separated with double newlines

@lilypond[verbatim,quote]
\\markup {
  \\override #'(line-width . 40)
  \\justify-string #\"Lorem ipsum dolor sit amet, consectetur
      adipisicing elit, sed do eiusmod tempor incididunt ut
      labore et dolore magna aliqua.


      Ut enim ad minim veniam, quis nostrud exercitation
      ullamco laboris nisi ut aliquip ex ea commodo
      consequat.


      Excepteur sint occaecat cupidatat non proident, sunt
      in culpa qui officia deserunt mollit anim id est
      laborum\"
}
@end lilypond"
  (stack-lines DOWN 0.0 baseline-skip
               (wordwrap-string-internal-markup-list layout props #t arg)))

(define-markup-command (wordwrap-field layout props symbol)
  (symbol?)
  #:category align
  #:as-string (markup->string (chain-assoc-get symbol props)
                              #:layout layout
                              #:props props)
  "Wordwrap the data which has been assigned to @var{symbol}.

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
  "Justify the data which has been assigned to @var{symbol}.

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

Print two markups on top of each other.

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

Takes a list of markups combining them.

@lilypond[verbatim,quote]
\\markup {
  \\fontsize #5
  \\override #'(thickness . 2)
  \\overlay {
    \\draw-line #'(0 . 4)
    \\arrow-head #Y #DOWN ##f
    \\translate #'(0 . 4)\\arrow-head #Y #UP ##f
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
@cindex stacking text in a column

Stack the markups in @var{args} vertically.  The property
@code{baseline-skip} determines the space between markups
in @var{args}.

@lilypond[verbatim,quote]
\\markup {
  \\column {
    one
    two
    three
  }
}
@end lilypond"
  (let ((arg-stencils (interpret-markup-list layout props args)))
    (stack-lines -1 0.0 baseline-skip arg-stencils)))

(define-markup-command (dir-column layout props args)
  (markup-list?)
  #:category align
  #:properties ((direction)
                (baseline-skip))
  "
@cindex changing direction of text column

Make a column of @var{args}, going up or down, depending on the
setting of the @code{direction} layout property.

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
@end lilypond"
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

Put @code{args} in a centered column.

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

Put @code{args} in a left-aligned column.

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

Put @code{args} in a right-aligned column.

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

Align @code{arg} to its Y@tie{}center.

@lilypond[verbatim,quote]
\\markup {
  one
  \\vcenter
  two
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

Align @code{arg} to its X@tie{}center.

@lilypond[verbatim,quote]
\\markup {
  \\column {
    one
    \\center-align
    two
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
    \\right-align
    two
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
    \\left-align
    two
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
    \\general-align #X #LEFT
    two
    three
    \\null
    one
    \\general-align #X #CENTER
    two
    three
    \\null
    \\line {
      one
      \\general-align #Y #UP
      two
      three
    }
    \\null
    \\line {
      one
      \\general-align #Y #3.2
      two
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

Set horizontal alignment.  If @var{dir} is @w{@code{-1}}, then it is
left-aligned, while @code{+1} is right.  Values in between interpolate
alignment accordingly.

@lilypond[verbatim,quote]
\\markup {
  \\column {
    one
    \\halign #LEFT
    two
    three
    \\null
    one
    \\halign #CENTER
    two
    three
    \\null
    one
    \\halign #RIGHT
    two
    three
    \\null
    one
    \\halign #-5
    two
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
  (index? number? markup? number? markup?)
  #:category align
  "
Align markup @var{self} on markup @var{other} along axis @var{axis},
using @var{self-@/dir} and @var{other-@/dir} for mutual alignment of
@var{self} and @var{other}, respectively.  This command translates
@var{self} as requested relative to its surroundings; @var{other} is
not printed.

@lilypond[verbatim,quote]
\\markup \\column {
  1
  12
  \\align-on-other #X #RIGHT 12
                     #LEFT 12345
  123
}
@end lilypond"
  (let* ((self-stil (interpret-markup layout props self))
         (self-ext (ly:stencil-extent self-stil axis))
         (other-stil (interpret-markup layout props other))
         (other-ext (ly:stencil-extent other-stil axis))
         (trans (- (interval-index other-ext other-dir)
                   (interval-index self-ext self-dir))))
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

Set the horizontal dimension of @var{arg} to @var{val} if @var{axis}
is equal to@tie{}@code{X}.  If @var{axis} is equal to@tie{}@code{Y},
set the vertical dimension of @var{arg} to @var{val} instead."
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

Print @var{arg} with the outline and dimensions of @var{outline}. The outline
is used by skylines to resolve collisions (not for whiteout)."
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

Print @var{arg2} but replace the horizontal dimension with the one
from @var{arg1} if @var{axis} is set to@tie{}@code{X}.  If @var{axis}
is set to@tie{}@code{Y}, replace the vertical dimension with the one
from @var{arg1} instead."
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
@cindex dimensions, of bounding box
@cindex extent, of bounding box
@cindex extent, of actual inking

Give @var{arg} its actual dimension (extent) on @var{axis}.  Sometimes, the
extents of a markup's printed ink differs from the default extents.  The main
case is if glyphs are involved.  By default, the extents of a glyph are based on
the glyph's @dfn{metrics} (i.e., a default vertical and horizontal size for the
glyph), which, for various reasons, are often not identical to its @dfn{bounding
box} (i.e., the smallest rectangle that completely encompasses the glyph's
outline) -- in most cases, the outline protrudes the box spanned up by the
metrics.

@lilypond[verbatim,quote]
\\markup
  \\fontsize #10
  \\override #'((box-padding . 0) (thickness . 0.2))
  \\box
  \\musicglyph \"scripts.trill\"
@end lilypond

For purposes other than setting text, this behavior may not be wanted.
You can use @code{\\with-true-dimension} in order to give the markup
its actual printed extent.

@lilypond[verbatim,quote]
\\markup
  \\fontsize #10
  \\override #'((box-padding . 0) (thickness . 0.2))
  \\box
  \\with-true-dimension #X
  \\musicglyph \"scripts.trill\"
@end lilypond
"
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
  "@code{\\markup \\with-@/true-@/dimensions @var{arg}} is short for
@code{\\markup \\with-@/true-@/dimension #X \\with-@/true-@/dimension #Y
@var{arg}}, i.e., @code{\\with-@/true-@/dimensions} has the effect of
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
  (let* ((m (interpret-markup layout props arg))
         (x (interval-widen (ly:stencil-extent m X) amount))
         (y (interval-widen (ly:stencil-extent m Y) amount)))
    (ly:stencil-add (make-transparent-box-stencil x y)
                    m)))

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
  "Put @var{arg2} next to @var{arg1}, without moving @var{arg1}."
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
      \\hcenter-in #12
      Oboe
    }
    c''1
  }
  \\new Staff {
    \\set Staff.instrumentName = \\markup {
      \\hcenter-in #12
      Bassoon
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
  "Print out a warning when a header field markup contains some recursive
markup definition."
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
  "Read the @var{symbol} from property settings, and produce a stencil
from the markup contained within.  If @var{symbol} is not defined, it
returns an empty markup.

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

@c we don't want to display \\book
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
@cindex overriding property within text markup

Add the argument @var{new-prop} to the property list.  Properties
may be any property supported by @rinternals{font-interface},
@rinternals{text-interface} and
@rinternals{instrument-specific-markup-interface}.

@var{new-prop} may be either a single alist pair, or non-empty alist
of its own.

@lilypond[verbatim,quote]
\\markup {
  \\undertie \"undertied\"
  \\override #'(offset . 15)
  \\undertie \"offset undertied\"
  \\override #'((offset . 15)(thickness . 3))
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
  "Read the contents of file @var{name}, and include it verbatim.

@lilypond[verbatim,quote]
\\markup {
  \\verbatim-file #\"en/included/simple.ly\"
}
@end lilypond"
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
  "Decrease the font size relative to the current setting.

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
  "Increase the font size relative to the current setting.

@lilypond[verbatim,quote]
\\markup {
  default
  \\hspace #2
  \\larger
  larger
}
@end lilypond"
  (interpret-markup layout props
                    `(,fontsize-markup 1 ,arg)))

(define-markup-command (figured-bass layout props arg)
  (markup?)
  #:category font
  "Set @var{arg} as small numbers for figured bass.  Specially slashed
digits can be achieved with a trailing backslashes (for numbers 6,
7, and@tie{}9) or a trailing plus (for numbers 2, 4,
and@tie{}5).@footnote{Internally, this works by activating the
@q{dlig} OpenType feature of the Emmentaler font.}

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
                    (cons '((font-size . -5)
                            (font-encoding . fetaText)
                            (font-features . ("dlig" "tnum" "cv47" "ss01")))
                          props)
                    arg))

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
                    (cons '((font-size . -5)
                            (font-encoding . fetaText)
                            (font-features . ("cv47" "ss01")))
                          props)
                    arg))

(define-markup-command (abs-fontsize layout props size arg)
  (number? markup?)
  #:properties ((word-space 0.6) (baseline-skip 3))
  #:category font
  "Use @var{size} as the absolute font size (in points) to display @var{arg}.
Adjusts @code{baseline-skip} and @code{word-space} accordingly.

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
         (text-props (list (ly:output-def-lookup layout 'text-font-defaults)))
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
  "Add @var{increment} to the font-size.  Adjusts @code{baseline-skip}
accordingly.

@lilypond[verbatim,quote]
\\markup {
  default
  \\hspace #2
  \\fontsize #-1.5
  smaller
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

Set the font magnification for its argument.  In the following
example, the middle@tie{}A is 10% larger:

@example
A \\magnify #1.1 @{ A @} A
@end example

Note: Magnification only works if a font name is explicitly selected.
Use @code{\\fontsize} otherwise.

@lilypond[verbatim,quote]
\\markup {
  default
  \\hspace #2
  \\magnify #1.5 {
    50% larger
  }
}
@end lilypond"
  (interpret-markup
   layout
   (prepend-alist-chain 'font-size (magnification->font-size sz) props)
   arg))

(define-markup-command (bold layout props arg)
  (markup?)
  #:category font
  "Switch to bold font-series.

@lilypond[verbatim,quote]
\\markup {
  default
  \\hspace #2
  \\bold
  bold
}
@end lilypond"
  (interpret-markup layout (prepend-alist-chain 'font-series 'bold props) arg))

(define-markup-command (sans layout props arg)
  (markup?)
  #:category font
  "Switch to the sans serif font family.

@lilypond[verbatim,quote]
\\markup {
  default
  \\hspace #2
  \\sans {
    sans serif
  }
}
@end lilypond"
  (interpret-markup layout (prepend-alist-chain 'font-family 'sans props) arg))

(define-markup-command (number layout props arg)
  (markup?)
  #:category font
  "Set font family to @code{number}, which yields the font used for digits.
This font also contains some punctuation; it has no letters.

@cindex feature, OpenType font
@cindex font feature, OpenType
@cindex OpenType, font feature

The appearance of digits in the Emmentaler font can be controlled with
four OpenType features: @q{tnum}, @q{cv47}, @q{ss01}, and @q{kern},
which can be arbitrarily combined.

@indentedBlock
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
@endIndentedBlock

@lilypond[verbatim,quote]
\\markuplist
  \\number
  \\fontsize #5
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

(define-markup-command (roman layout props arg)
  (markup?)
  #:category font
  "Set font family to @code{roman}.

@lilypond[verbatim,quote]
\\markup {
  \\sans \\bold {
    sans serif, bold
    \\hspace #2
    \\roman {
      text in roman font family
    }
    \\hspace #2
    return to sans
  }
}
@end lilypond"
  (interpret-markup layout (prepend-alist-chain 'font-family 'roman props) arg))

(define-markup-command (huge layout props arg)
  (markup?)
  #:category font
  "Set font size to +2.

@lilypond[verbatim,quote]
\\markup {
  default
  \\hspace #2
  \\huge
  huge
}
@end lilypond"
  (interpret-markup layout (prepend-alist-chain 'font-size 2 props) arg))

(define-markup-command (large layout props arg)
  (markup?)
  #:category font
  "Set font size to +1.

@lilypond[verbatim,quote]
\\markup {
  default
  \\hspace #2
  \\large
  large
}
@end lilypond"
  (interpret-markup layout (prepend-alist-chain 'font-size 1 props) arg))

(define-markup-command (normalsize layout props arg)
  (markup?)
  #:category font
  "Set font size to default.

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
  "Set font size to -1.

@lilypond[verbatim,quote]
\\markup {
  default
  \\hspace #2
  \\small
  small
}
@end lilypond"
  (interpret-markup layout (prepend-alist-chain 'font-size -1 props) arg))

(define-markup-command (tiny layout props arg)
  (markup?)
  #:category font
  "Set font size to -2.

@lilypond[verbatim,quote]
\\markup {
  default
  \\hspace #2
  \\tiny
  tiny
}
@end lilypond"
  (interpret-markup layout (prepend-alist-chain 'font-size -2 props) arg))

(define-markup-command (teeny layout props arg)
  (markup?)
  #:category font
  "Set font size to -3.

@lilypond[verbatim,quote]
\\markup {
  default
  \\hspace #2
  \\teeny
  teeny
}
@end lilypond"
  (interpret-markup layout (prepend-alist-chain 'font-size -3 props) arg))

(define-markup-command (fontCaps layout props arg)
  (markup?)
  #:category font
  "Set @code{font-shape} to @code{caps}

Note: @code{\\fontCaps} requires the installation and selection of
fonts which support the @code{caps} font shape."
  (interpret-markup layout (prepend-alist-chain 'font-shape 'caps props) arg))

(define-markup-command (with-string-transformer layout props transformer arg)
  (procedure? markup?)
  #:category font
  "Interpret the markup @var{arg} with a string transformer installed.
Whenever a string is interpreted inside @var{arg}, the transformer
is first called, and it is the result that is interpreted.  The arguments
passed to the transformer are the output definition, the property alist
chain, and the string.  See @rextend{New markup command definition}
about the two first arguments.

@lilypond[verbatim,quote]
\\markup \\with-string-transformer
  #(lambda (layout props str)
     (string-upcase str))
  \"abc\"
@end lilypond
"
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
  "Emit @var{arg} as small caps.

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
  "Copy of the @code{\\smallCaps} command.

@lilypond[verbatim,quote]
\\markup {
  default
  \\hspace #2
  \\caps {
    Text in small caps
  }
}
@end lilypond"
  (interpret-markup layout props (make-smallCaps-markup arg)))

(define-markup-command (dynamic layout props arg)
  (markup?)
  #:category font
  "Use the dynamic font.  This font only contains @b{s}, @b{f}, @b{m},
@b{z}, @b{p}, and @b{r}.  When producing phrases, like
@q{pi@`{u}@tie{}@b{f}}, the normal words (like @q{pi@`{u}}) should be
done in a different font.  The recommended font for this is bold and italic.
@lilypond[verbatim,quote]
\\markup {
  \\dynamic {
    sfzp
  }
}
@end lilypond"
  (interpret-markup
   layout (prepend-alist-chain 'font-encoding 'fetaText props) arg))

(define-markup-command (text layout props arg)
  (markup?)
  #:category font
  "Use a text font instead of music symbol or music alphabet font.

@lilypond[verbatim,quote]
\\markup {
  \\number {
    1, 2,
    \\text {
      three, four,
    }
    5
  }
}
@end lilypond"

  ;; ugh - latin1
  (interpret-markup layout (prepend-alist-chain 'font-encoding 'latin1 props)
                    arg))

(define-markup-command (italic layout props arg)
  (markup?)
  #:category font
  "Use italic @code{font-shape} for @var{arg}.

@lilypond[verbatim,quote]
\\markup {
  default
  \\hspace #2
  \\italic
  italic
}
@end lilypond"
  (interpret-markup layout (prepend-alist-chain 'font-shape 'italic props) arg))

(define-markup-command (typewriter layout props arg)
  (markup?)
  #:category font
  "Use @code{font-family} typewriter for @var{arg}.

@lilypond[verbatim,quote]
\\markup {
  default
  \\hspace #2
  \\typewriter
  typewriter
}
@end lilypond"
  (interpret-markup
   layout (prepend-alist-chain 'font-family 'typewriter props) arg))

(define-markup-command (upright layout props arg)
  (markup?)
  #:category font
  "Set @code{font-shape} to @code{upright}.  This is the opposite
of @code{italic}.

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

(define-markup-command (medium layout props arg)
  (markup?)
  #:category font
  "Switch to medium font-series (in contrast to bold).

@lilypond[verbatim,quote]
\\markup {
  \\bold {
    some bold text
    \\hspace #2
    \\medium {
      medium font series
    }
    \\hspace #2
    bold again
  }
}
@end lilypond"
  (interpret-markup layout (prepend-alist-chain 'font-series 'medium props)
                    arg))

(define-markup-command (normal-text layout props arg)
  (markup?)
  #:category font
  "Set all font related properties (except the size) to get the default
normal text font, no matter what font was used earlier.

@lilypond[verbatim,quote]
\\markup {
  \\huge \\bold \\sans \\caps {
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
                    (cons '((font-family . roman) (font-shape . upright)
                            (font-series . medium) (font-encoding . latin1))
                          props)
                    arg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; symbols.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-markup-command (musicglyph layout props glyph-name)
  (string?)
  #:category music
  ;; TODO: as-string?
  "@var{glyph-name} is converted to a musical symbol; for example,
@code{\\musicglyph #\"accidentals.natural\"} selects the natural sign from
the music font.  See @ruser{The Emmentaler font} for a complete listing of
the possible glyphs.

@lilypond[verbatim,quote]
\\markup {
  \\musicglyph #\"f\"
  \\musicglyph #\"rests.2\"
  \\musicglyph #\"clefs.G_change\"
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
  "Select an accidental glyph from an alteration, given as
rational number.

@lilypond[verbatim,quote]
\\markup \\accidental #1/2
@end lilypond"
  (let* ((defs (ly:output-def-lookup layout 'font-defaults))
         (glyph-alist (or alteration-glyph-name-alist
                          (assq-ref defs 'alteration-glyph-name-alist))))
    (interpret-markup layout props
                      (make-musicglyph-markup
                       (or
                        (assv-ref glyph-alist alteration)
                        (begin
                          (ly:warning (G_ "no accidental glyph found for alteration ~a")
                                      alteration)
                          "noteheads.s1cross"))))))

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

(define-markup-command (with-color layout props color arg)
  (color? markup?)
  #:category other
  ;; Needed because a color can be a string and the default
  ;; behavior would include it in the result.
  #:as-string (markup->string arg #:layout layout #:props props)
  "
@cindex coloring text

Draw @var{arg} in color specified by @var{color}.

@lilypond[verbatim,quote]
\\markup {
  \\with-color #red
  red
  \\hspace #2
  \\with-color #green
  green
  \\hspace #2
  \\with-color \"#0000ff\"
  blue
}
@end lilypond"
  (stencil-with-color (interpret-markup layout props arg) color))

(define-markup-command (tied-lyric layout props str)
  (string?)
  #:category music
  #:properties ((word-space))
  ;; Use Unicode undertie character U+203F.
  #:as-string (regexp-substitute/global #f "~" str 'pre "\u203f" 'post)
  "
@cindex simple text string, with tie characters

Replace @q{~} tilde symbols with tie characters in the argument.

@lilypond[verbatim,quote]
\\markup \\column {
  \\tied-lyric
    #\"Siam navi~all'onde~algenti Lasciate~in abbandono\"
  \\tied-lyric
    #\"Impetuosi venti I nostri~affetti sono\"
  \\tied-lyric
    #\"Ogni diletto~e scoglio Tutta la vita~e~un mar.\"
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

  (define short-tie-regexp (make-regexp "~[^.]~"))
  (define (match-short str) (regexp-exec short-tie-regexp str))

  (define (replace-short str mkp)
    (let ((match (match-short str)))
      (if (not match)
          (make-concat-markup (list
                               mkp
                               (replace-ties "ties.lyric.default" str)))
          (let ((new-str (match:suffix match))
                (new-mkp (make-concat-markup (list
                                              mkp
                                              (replace-ties "ties.lyric.default"
                                                            (match:prefix match))
                                              (replace-ties "ties.lyric.short"
                                                            (match:substring match))))))
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
  "Produce an arrow head in specified direction and axis.
Use the filled head if @var{filled} is specified.
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
  "Lookup a glyph by name.

@lilypond[verbatim,quote]
\\markup {
  \\override #'(font-encoding . fetaBraces) {
    \\lookup #\"brace200\"
    \\hspace #2
    \\rotate #180
    \\lookup #\"brace180\"
  }
}
@end lilypond"
  (ly:font-get-glyph (ly:paper-get-font layout props)
                     glyph-name))

(define-markup-command (char layout props num)
  (integer?)
  #:category other
  #:as-string (ly:wide-char->utf-8 num)
  "Produce a single character.  Characters encoded in hexadecimal
format require the prefix @code{#x}.

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
  "Make a markup letter for @var{num}.  The letters start with A
to@tie{}Z (skipping letter@tie{}I), and continue with double letters.

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
  "Make a markup letter for @var{num}.  The letters start with A to@tie{}Z
and continue with double letters.

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

A feta number, with slash.  This is for use in the context of
figured bass notation.
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

A feta number, with backslash.  This is for use in the context of
figured bass notation.
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
A feta brace in point size @var{size}.

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
  "
A feta brace in point size @var{size}, rotated 180 degrees.

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

Construct a note symbol, with stem and flag.  By using fractional values for
@var{dir}, longer or shorter stems can be obtained.
Supports all note-head-styles.  Ancient note-head-styles will get
mensural-style-flags.  @code{flag-style} may be overridden independently.
Supported flag-styles are @code{default}, @code{old-straight-flag},
@code{modern-straight-flag}, @code{flat-flag}, @code{mensural} and
@code{neomensural}.  The latter two flag-styles will both result in
mensural-flags.  Both are supplied for convenience.

@lilypond[verbatim,quote]
\\markup {
  \\note-by-number #3 #0 #DOWN
  \\hspace #2
  \\note-by-number #1 #2 #0.8
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
         (modern-straight-flag (straight-flag-mrkp 0.55 1 -18 1.1 22 1.2 dir))
         (old-straight-flag (straight-flag-mrkp 0.55 1 -45 1.2 45 1.4 dir))
         (flat-flag (straight-flag-mrkp 0.55 1.0 0 1.0 0 1.0 dir))
         ;; Calculate a corrective to avoid a gap between
         ;; straight-flags and the stem.
         (flag-style-Y-corr (if (or (eq? flag-style 'modern-straight-flag)
                                    (eq? flag-style 'old-straight-flag)
                                    (eq? flag-style 'flat-flag))
                                (/ blot 10 (* -1 dir))
                                0))
         (flaggl (and (> log 2)
                      (ly:stencil-translate
                       (cond ((eq? flag-style 'modern-straight-flag)
                              modern-straight-flag)
                             ((eq? flag-style 'old-straight-flag)
                              old-straight-flag)
                             ((eq? flag-style 'flat-flag)
                              flat-flag)
                             (else
                              (ly:font-get-glyph font
                                                 (format #f
                                                         (if (or (member flag-style
                                                                         '(mensural neomensural))
                                                                 (and ancient-flags?
                                                                      (null? flag-style)))
                                                             "flags.mensural~a2~a"
                                                             "flags.~a~a")
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
             (or (eq? flag-style 'default) (null? flag-style))
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

This produces a note with a stem pointing in @var{dir} direction, with
the @var{duration} for the note head type and augmentation dots.  For
example, @code{\\note @{4.@} #-0.75} creates a dotted quarter note, with
a shortened down stem.

@lilypond[verbatim,quote]
\\markup {
  \\override #'(style . cross)
  \\note {4..} #UP
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

A rest symbol.

For duration logs specified with property @code{ledgers}, rest symbols with
ledger lines are selected.

@lilypond[verbatim,quote]
\\markup {
  \\rest-by-number #3 #2
  \\hspace #2
  \\rest-by-number #0 #1
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
  (multi-measure-rest-by-number layout props duration-scale)
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
@cindex multi-measure rest, within text, by @code{duration-scale}

Returns a multi-measure rest symbol.

If the number of measures is greater than the number given by
@code{expand-limit} a horizontal line is printed.  For every multi-measure rest
lasting more than one measure a number is printed on top.

@lilypond[verbatim,quote]
\\markup {
  Multi-measure rests may look like
  \\multi-measure-rest-by-number #12
  or
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
    (if (> duration-scale expand-limit)
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
               (counted-glyphs-list (mmr-numbers duration-scale))
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
                     ;; filtered lateron.
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
    (if (or (> duration-scale expand-limit)
            (and multi-measure-rest-number
                 (> duration-scale 1)
                 (not (member style '(neomensural mensural petrucci)))))
        (let* ((mmr-stil-x-center
                (interval-center (ly:stencil-extent mmr-stil X)))
               (duration-markup
                (make-fontsize-markup -2
                                      (make-override-markup '(font-encoding . fetaText)
                                                            (number->string duration-scale))))
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
                 ;; Ugh, hardcoded
                 (if (> duration-scale expand-limit) 0 0.8)))))
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

Returns a rest symbol.

If @code{multi-measure-rest} is set to true, a multi-measure
rest symbol my be returned.  In this case the duration needs to be entered as
@code{@{ 1*2 @}}to get a multi-measure rest for two bars.  Actually, it's only
the scaling factor that determines the length, the basic duration is
disregarded.
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
  \\override #'(multi-measure-rest-number . #f)
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
  "Create a fermata glyph.  When @var{direction} is @code{DOWN}, use
an inverted glyph.  Note that within music, one would usually use the
@code{\\fermata} articulation instead of a markup.

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

@lilypond[verbatim,quote]
\\markup {
  one
  \\lower #3
  two
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

Translate @var{arg} by @var{offset}, scaling the offset by the
@code{font-size}.

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

The argument to @code{\\raise} is the vertical displacement amount,
measured in (global) staff spaces.  @code{\\raise} and @code{\\super}
raise objects in relation to their surrounding markups.

If the text object itself is positioned above or below the staff, then
@code{\\raise} cannot be used to move it, since the mechanism that
positions it next to the staff cancels any shift made with
@code{\\raise}.  For vertical positioning, use the @code{padding}
and/or @code{extra-offset} properties.

@lilypond[verbatim,quote]
\\markup {
  C
  \\small
  \\bold
  \\raise #1.0
  9/7+
}
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

Make a fraction of two markups.
@lilypond[verbatim,quote]
\\markup {
  π ≈
  \\fraction 355 113
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
\\markup {
  E =
  \\concat {
    mc
    \\super
    2
  }
}
@end lilypond"
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

Translate @var{arg} relative to its surroundings.  @var{offset}
is a pair of numbers representing the displacement in the X and Y axis.

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
\\markup {
  \\concat {
    H
    \\sub {
      2
    }
    O
  }
}
@end lilypond"
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

Draw parentheses around @var{arg}.  This is useful for parenthesizing
a column containing several lines of text.

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
  #:category other
  #:as-string ""
  "
@cindex referencing page number, in text

Reference to a page number.  @var{label} is the label set on the referenced
page (using @code{\\label} or @code{\\tocItem}), @var{gauge} a markup used to estimate
the maximum width of the page number, and @var{default} the value to display
when @var{label} is not found.

(If the current book or bookpart is set to use roman numerals for page numbers,
the reference will be formatted accordingly -- in which case the @var{gauge}'s
width may require additional tweaking.)"
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
                                (interval-length (ly:stencil-extent page-stencil X)))))
                   (interpret-markup layout props
                                     (make-line-markup
                                      (list
                                       (make-hspace-markup gap)
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

Scale @var{arg}.  @var{factor-pair} is a pair of numbers
representing the scaling-factor in the X and Y axes.
Negative values may be used to produce mirror images.

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
  "
Prints @var{count} times a @var{pattern} markup.
Patterns are spaced apart by @var{space} (defined as for
@code{\\hspace} or @code{\\vspace}, respectively).
Patterns are distributed on @var{axis}.

@lilypond[verbatim,quote]
\\markup \\column {
  \"Horizontally repeated :\"
  \\pattern #7 #X #2 \\flat
  \\null
  \"Vertically repeated :\"
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
  "
Put @var{left} and @var{right} in a horizontal line of width @code{line-width}
with a line of markups @var{pattern} in between.
Patterns are spaced apart by @var{space}.
Patterns are aligned to the @var{dir} markup.

@lilypond[verbatim,quote,line-width=14\\cm]
\\markup \\column {
  \"right-aligned :\"
  \\fill-with-pattern #1 #RIGHT . first right
  \\fill-with-pattern #1 #RIGHT . second right
  \\null
  \"center-aligned :\"
  \\fill-with-pattern #1.5 #CENTER - left right
  \\null
  \"left-aligned :\"
  \\override #'(line-width . 50)
  \\fill-with-pattern #2 #LEFT : left first
  \\override #'(line-width . 50)
  \\fill-with-pattern #2 #LEFT : left second
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
  "
Used to automatically replace a string by another in the markup @var{arg}.
Each pair of the alist @var{replacements} specifies what should be replaced.
The @code{key} is the string to be replaced by the @code{value} markup.
Note the quasiquoting syntax with a backquote in the second example.

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
  "Test @var{condition}, and only insert @var{argument} if it is true.
The condition is provided as a procedure taking an output definition
and a property alist chain.  The procedure is applied, and its result
determines whether to print the markup.  This command is most useful inside
@code{odd@/Header@/Markup} or similar.  Here is an example printing page
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
  "Similar to @code{\\if}, printing the argument if the condition
is false.

The following example shows how to print the copyright notice on
all pages but the last instead of just the first page.

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
  "Like @code{\\wordwrap}, but return a list of lines instead of a single markup.
Use @code{\\override-lines #'(line-width . @var{X})} to set the line width,
where @var{X} is the number of staff spaces."
  (space-lines baseline-skip
               (interpret-markup-list layout props
                                      (make-wordwrap-internal-markup-list #f args))))

(define-markup-list-command (column-lines layout props args)
  (markup-list?)
  #:properties ((baseline-skip))
  "Like @code{\\column}, but return a list of lines instead of a single markup.
@code{baseline-skip} determines the space between each markup in @var{args}."
  (space-lines baseline-skip
               (interpret-markup-list layout props args)))

(define-markup-list-command (override-lines layout props new-prop args)
  (pair? markup-list?)
  "Like @code{\\override}, for markup lists."
  (interpret-markup-list layout
                         (cons (if (pair? (car new-prop)) new-prop (list new-prop))
                               props)
                         args))

(define-markup-list-command (table layout props column-align lst)
  (number-list? markup-list?)
  #:properties ((padding 0)
                (baseline-skip))
  "@cindex creating a table

Returns a table.

@var{column-align} specifies how each column is aligned, possible values are
-1, 0, 1.  The number of elements in @var{column-align} determines how many
columns will be printed.
The entries to print are given by @var{lst}, a markup-list.  If needed, the last
row is filled up with @code{point-stencil}s.
Overriding @code{padding} may be used to increase columns horizontal distance.
Overriding @code{baseline-skip} to increase rows vertical distance.
@lilypond[verbatim,quote]
% A markup command to print a fixed-width number.
\\markup fwnum =
  \\markup \\override #'(font-features . (\"ss01\" \"-kern\"))
    \\number \\etc

\\markuplist {
  \\override #'(padding . 2)
  \\table
    #'(0 1 0 -1)
    {
      \\underline { center-aligned right-aligned
                    center-aligned left-aligned }
      one      \\fwnum    1 thousandth \\fwnum 0.001
      eleven   \\fwnum   11 hundredth  \\fwnum 0.01
      twenty   \\fwnum   20 tenth      \\fwnum 0.1
      thousand \\fwnum 1000 one        \\fwnum 1.0
    }
}
@end lilypond
"

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

(define-markup-list-command (string-lines layout props strg)(string?)
  #:properties ((split-char #\newline))
  "
Takes the string @var{strg} and splits it at the character provided by the
property @code{split-char}, defaulting to @code{#\\newline}.
Surrounding whitespace is removed from every resulting string.
The returned list of markups is ready to be formatted by other markup or markup
list commands like @code{\\column}, @code{\\line}, etc.

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
                         (map string-trim-both (string-split strg split-char))))

(define-markup-list-command (map-markup-commands layout props compose args)
  (procedure? markup-list?)
  "This applies the function @var{compose} to every markup in
@var{args} (including elements of markup list command calls) in order
to produce a new markup list.  Since the return value from a markup
list command call is not a markup list but rather a list of stencils,
this requires passing those stencils off as the results of individual
markup calls.  That way, the results should work out as long as no
markups rely on side effects."
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
