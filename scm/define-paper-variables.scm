;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 1998--2023 Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;;                 Jan Nieuwenhuizen <janneke@gnu.org>
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

(define-public all-paper-variable-descriptions
  `(

;;; Sorting below ignores case and character '-'.
;;;
;;; The remark 'scaled to paper size' means that the value given is
;;; for the A4 paper format (which is the default); it gets scaled
;;; accordingly to fit other paper formats.  In the `\paper` block,
;;; properties that are directly connected to the paper format are
;;; handled on three levels:
;;;
;;;   * `<foo>`: The user-defined value.
;;;   * `<foo>-default`: The default value, to be used if `<foo>` is
;;;     not set.
;;;   * `<foo>-default-scaled`: `<foo>-default` scaled to the current
;;;     paper format; this is what LilyPond actually uses as the
;;;     fallback value; such variables are not explicitly mentioned
;;;     below.


;;
;; a
;;
    (annotate-spacing ,boolean? "If this value is set to @code{#t},
dimensions of vertical layout variables that may be altered for page
formatting are graphically displayed.  Default: @code{#f}.")
    (auto-first-page-number ,boolean? "The page-breaking algorithm is
affected by the first page number being odd or even.  If set to
@code{#t}, the algorithm decides whether to start with an odd or even
number.  This results in the first page number remaining as is or
being increased by one.  Default: @code{#f}.")

;;
;; b
;;
    (binding-offset ,number? "This amount gets added to
@code{inner-margin} so that nothing is hidden by the binding.  Has
only an effect if @code{two-sided} is set to @code{#t}.  Default:
@code{binding-offset-default} (5@dmn{mm}, scaled to paper size).")
    (blank-after-score-page-penalty ,number? "The penalty for having
a blank page after the end of one score and before the next.  By
default, this is smaller than @code{blank-page-penalty} so that
LilyPond prefers blank pages after scores to blank pages within a
score.  Default:@tie{}2.")
    (blank-last-page-penalty ,number? "The penalty for ending the
score on an odd-numbered page.  Default:@tie{}0.")
    (blank-page-penalty ,number? "The penalty for having a blank page
in the middle of a score.  Note that this is not used by
@code{ly:optimal-breaking}, which never considers blank pages in the
middle of a score.  Default:@tie{}5.")
    (book-title ,procedure? "Internal.  This is what LilyPond
actually uses for handling @code{bookTitleMarkup}.")
    (bookTitleMarkup ,markup? "The titling markup within a
@code{\\book}, using standard fields from the @code{\\header} block.
The default value is defined in file @file{titling-init.ly}.")
    (bookpart-level-page-numbering ,boolean? "If set to @code{#t},
restart page numbering for each @code{\\bookpart} block.  Otherwise
all pages in the document are enumerated continuously.  Default:
@code{#f}.")
    (book-title-properties ,alist? "Internal.")
    (blot-diameter ,non-negative-number? "This value globally defines
the smallest @q{round} feature LilyPond uses while constructing almost
all non-glyph elements like beams or stems.  Essentially, it sets up
how round the corners and line ends are.  It only makes sense to
change the value if you use a different music glyph font with crisper
corners, say.  Default: 0.4@dmn{pt}.")
    (bottom-margin ,non-negative-number? "The margin between the
bottom of the printable area and the bottom of the page.  Default:
@code{bottom-margin-default} (10@dmn{mm}, scaled to paper size).")
    (bp ,positive-number? "The big point unit, also called
@dfn{desktop publishing point} (DTP point).  It is 1/72 of an inch,
approx.@: 0.353@dmn{mm} (0.0138@dmn{in}).")

;;
;; c
;;
    (check-consistency ,boolean? "If set to @code{#t}, print a
warning if the left margin, line width, and right margin do not
exactly add up to @code{paper-width}, and replace each of
these (except @code{paper-width}) with its default value (scaled to
the paper size if necessary).  If set to @code{#f}, ignore any
inconsistencies and allow systems to run off the edge of the page.
Default: @code{#t}.")
    (clip-regions ,alist? "A list of rhythm location pairs to
output fragments of a score.

@example
clip-regions
= #(list (cons (make-rhythmic-location 2 0 1)
               (make-rhythmic-location 4 0 1))
         (cons (make-rhythmic-location 5 1 2)
               (make-rhythmic-location 7 3 4)))
@end example

The above example defines two regions: the first is from the beginning
of bar@tie{}2 to the beginning of bar@tie{}4, the second starts after
a half note duration in bar@tie{}5 and ends after the third quarter in
bar@tie{}7.

Has only an effect if LilyPond's command-line option
@option{-dclip-systems} is active.  Default: unset.")
    (cm ,positive-number? "The centimeter unit, approx.@:
0.39@dmn{in}.")

;;
;; d
;;
    (debug-beam-scoring ,boolean? "If set to @code{#t}, print demerits
together with their cause, followed by the number of configurations
that have been scored before concluding.  Default: @code{#f}.

Example:

@indentedblock
@code{L 18.95 C 655.12 c19/625} @arrow{} demerits for stem lengths
(@samp{L}) and collisions (@samp{C}), scored 19 out of 625 initially
considered configurations.
@end indentedblock

Possible demerit causes: collision (@samp{C}), inappropriate stem
length (@samp{L}), beam direction different from damping
direction (@samp{Sd}), difference between beam slope and musical
slope (@samp{Sm}), deviation from ideal slope (@samp{Si}), horizontal
inter-quants (@samp{H}), forbidden quants (@samp{Fl}/@samp{Fs}).

@c TODO: What are 'horizontal inter-quants' and 'forbidden quants'?

Demerits are configurable, see @rinternals{beam-interface} for a list
of tunable parameters.")
    (debug-slur-scoring ,boolean? "If set to @code{#t}, print demerits
together with their cause, followed by the sum of all demerits and the
index of the slur configuration finally chosen.  Default: @code{#f}.

Example:

@indentedblock
@code{slope=2.00, R edge=10.51, variance=0.03 TOTAL=12.54 idx=4}
@arrow{} demerits for slope, distance of the right edge to the
attachment point, variance of distance between note heads and slur.
Total demerits: 12.54, index of the chosen configuration:@tie{}4.
@end indentedblock

Possible demerit causes: distance of the left/right slur edge to the
attachment points (@samp{L edge}/@samp{R edge}), inappropriate
slope (@samp{slope}), distance variations between note heads and
slur (@samp{variance}), distances for heads that are between the slur
and an imaginary line between the attachment points
(@samp{encompass}), too small distance between slur and tie extrema
(@samp{extra}).

Demerits are configurable, see @rinternals{slur-interface} for a list
of tunable parameters.")
    (debug-tie-scoring ,boolean? "If set to @code{#t}, print the basic
configuration of ties, followed by demerits and their corresponding
causes and the total sum of demerits.  Default: unset.

Example:

@indentedblock
@code{0 (0.23) u: vdist=1.08 lhdist=1.79 tie/stem dir=8.00
TOTAL=10.87} @arrow{} offset from the center of the staff according
tie specification: 0@tie{}staff-spaces, vertical distance of the tie's
center in y-direction to the bottom (or top) of the tie: 0.23,
direction: up.  Demerits for vertical and horizontal distance to note
head, same direction of stem and tie.  Total demerits: 10.87.
@end indentedblock

@c TODO: Which unit does the vertical distance have? staff-spaces?

Possible demerit causes: wrong tie direction (@samp{wrong dir}),
vertical distance to note heads (@samp{vdist}), horizontal distance to
left or right note head (@samp{lhdist}/@samp{rhdist}), same direction
of stem and tie (@samp{tie/stem dir}), position and direction of tie
not matching, e.g., tie is in the upper half of the staff but has
direction @code{DOWN} (@samp{tie/pos dir}), tie is too short
(@samp{minlength}), tip of tie collides with staff line
(@samp{tipline}), collision with dot (@samp{dot collision}), center of
tie is too close to a staff line (@samp{line center}), y-position
(edge or center) of currently considered tie is less than the
y-position of the previous tie (@samp{monoton edge}/ @samp{monoton
cent}), edge or center of tie is too close to the one considered
previously (@samp{tietie center}/@samp{tietie edge}), unsymmetrical
horizontal positioning with respect to the note heads (@samp{length
symm}), unsymmetrical vertical positioning with respect to the note
heads (@samp{pos symmetry}).

Demerits are configurable, see @rinternals{tie-interface} for a
list of tunable parameters.")
    (dimension-variables ,list? "Internal.")

;;
;; e
;;
    (evenFooterMarkup ,markup? "The footer markup used for
even-numbered pages.  If not set, @code{oddFooterMarkup} is used
instead.  The default value is defined in file @file{titling-init.ly}.")
    (evenHeaderMarkup ,markup? "The header markup used for
even-numbered pages.  If not set, @code{oddHeaderMarkup} is used
instead.  The default value is defined in file @file{titling-init.ly}.")

;;
;; f
;;
    (first-page-number ,integer? "The value of the page number on the
first page.  Default:@tie{}1.")
    (footnote-footer-padding ,number? "The padding between the footer
and the bottom-most footnote.  Default: 0.5@dmn{mm}.")
    (footnote-numbering-function ,procedure? "This variable holds the
name of the function that formats both footnote and in-note
numbers (without positioning it).  LilyPond provides two predefined
functions (in file @file{output-lib.scm}):
@code{numbered-footnotes} (@q{1}, @q{2}, etc.)  and
@code{symbol-footnotes} (@q{*}, @q{†}, etc.).  Default:
@code{numbered-footnotes}.

The function takes an integer as an argument (starting with
value@tie{}0) and returns the appropriate markup.  You can create your
own function, for example,

@example
footnote-numbering-function =
#(lambda (x)
  #@{ \\markup \\concat @{ \"[\" #(number->string (1+ x)) \"]\" @} #@})
@end example
")
    (footnote-number-raise ,number? "This controls how high the
annotation numbers of both footnotes and in-notes are raised relative
to the footnote or in-note text.  Default: 0.5@dmn{mm}.")
    (footnote-padding ,number? "The padding between two footnotes.
Default: 0.5@dmn{mm}.")
    (footnote-separator-markup ,markup? "Markup to separate the music
from the footnotes, usually a horizontal line.  The default value is
defined in file @file{paper-defaults-init.ly}.")

;;
;; h
;;
    (horizontal-shift ,number? "If set, the main content block of all
pages (but not headers and footers) is shifted horizontally.  Positive
values shift to the right (on both even and odd pages).  Default:
0@dmn{mm}.")

;;
;; i
;;
    (in ,positive-number? "The inch unit, equal to 2.54@dmn{cm}.")
    (incipit-width ,positive-number? "The width of an incipit as
created by the @code{\\incipit} command.  Must be smaller than or
equal to @code{indent}.  Default: @code{(indent / 2)}.")
    ;; This is declared as a non-negative number to make it work
    ;; smoothlessly with incipits.  Hanging indentation can be
    ;; achieved in combination with `short-indent`.
    (indent ,non-negative-number? "The indentation of the first
system in a score.  The space within the @code{line-width} available
for the first system is reduced by this amount.  Default:
@code{indent-default} (15@dmn{mm}, scaled to paper size).")
    (inner-margin ,non-negative-number? "The margin all pages have at
the inner side if they are part of a book.  Has only an effect if
@code{two-sided} is set to @code{#t}.  If unset,
@code{inner-margin-default} (15@dmn{mm}, scaled to paper size) is used
in computations.  Default: unset.")
    (in-note-padding ,number? "The padding between two in-notes.
Default: 0.5@dmn{mm}.")
    (in-note-system-padding ,number? "The padding between an in-note
and the associated music system.  Default: 0.5@dmn{mm}.")
    (is-last-bookpart ,boolean? "To be documented.")

;;
;; l
;;
    (label-alist-table ,list? "Internal.  LilyPond uses this to
construct a table of contents.")
    (label-page-table ,alist? "Internal.  LilyPond uses this to
implement the @code{\\with-link} functionality and to construct a
table of contents.")
    (landscape ,boolean? "Internal.  Use
@code{set-default-paper-size} or @code{set-paper-size} to set the
paper orientation.")
    (last-bottom-spacing ,alist? "The distance from the last system
or top-level markup on a page to the bottom of the printable
area (i.e., the top of the bottom margin).  The default value is
defined in file @file{paper-defaults-init.ly}.")
    (left-margin ,non-negative-number? "The margin between the left
edge of the page and the start of the staff lines in unindented
systems.  If @code{left-margin} is not set, and both @code{line-width}
and @code{right-margin} are set, then @code{left-margin} is set to
@code{(paper-width - line-width - right-margin)}.  If only
@code{line-width} is set, then both margins are set to
@code{((paper-width - line-width) / 2)}, and the systems are
consequently centered on the page.  If unset,
@code{left-margin-default} (15@dmn{mm}, scaled to paper size) is used
in computations.  Default: unset.

If @code{two-sided} is set to @code{#t}, this value is ignored.  Also
see @code{check-consistency}.")
    (line-thickness ,positive-number? "This value globally defines
the default line thickness LilyPond uses while constructing many
non-glyph elements like lines or boxes.  The actual line thickness is
usually specified as a factor of this base value, either as a default
value, or overridden by the user.  The default depends on the staff
space and is computed as @code{(0.328571 + 0.0342857 *
staff-space)} (see function @code{calc-line-thickness} in file
@file{paper.scm}), which is synchronized with the Emmentaler font.")
    (line-width ,positive-number? "The horizontal extent of the staff
lines in unindented, non-ragged systems, equal to @code{(paper-width -
left-margin - right-margin)} if not set.  If @code{line-width} is set,
and both @code{left-margin} and @code{right-margin} are not set, then
the margins are updated to center the systems on the page
automatically.  Also see @code{check-consistency}.  Default: unset.")

;;
;; m
;;
    (make-footer ,procedure? "Internal.  This is what LilyPond
actually uses for for handling @code{evenFooterMarkup} and
@code{oddFooterMarkup}.")
    (make-header ,procedure? "Internal.  This is what LilyPond
actually uses for for handling @code{evenHeaderMarkup} and
@code{oddHeaderMarkup}.")
    (markup-markup-spacing ,alist? "The distance between two (title
or top-level) markups.  The default value is defined in file
@file{paper-defaults-init.ly}.")
    (markup-system-spacing ,alist? "The distance between a (title or
top-level) markup and the system that follows it.  The default value
is defined in file @file{paper-defaults-init.ly}.")
    (max-systems-per-page ,index? "The maximum number of systems that
are placed on a page.  This is currently supported only by the
@code{ly:optimal-breaking} algorithm.  Default: unset.")
    (min-systems-per-page ,index? "The minimum number of systems that
are placed on a page.  This may cause pages to be overfilled if it is
made too large.  This is currently supported only by the
@code{ly:optimal-breaking} algorithm.  Default: unset.")
    (mm ,positive-number? "The millimeter unit, approx.@:
0.039@dmn{in}.")

;;
;; n
;;
    (number-footnote-table ,list? "Internal.  LilyPond uses this to
manage footnotes and footnote numbers.")

;;
;; o
;;
    (oddFooterMarkup ,markup? "The footer markup used for
odd-numbered pages.  The default value is defined in file
@file{titling-init.ly}.")
    (oddHeaderMarkup ,markup? "The header markup used for
odd-numbered pages.  The default value is defined in file
@file{titling-init.ly}.")
    (orphan-penalty ,number? "To be documented.  See
@file{lily/page-breaking.cc}.  Default is 100000.")
    (outer-margin ,non-negative-number? "The margin all pages have at
the outer side if they are part of a book.  Has only an effect if
@code{two-sided} is set to @code{#t}.  If unset,
@code{outer-margin-default} (15@dmn{mm}, scaled to paper size) is used
in computations.  Default: unset.")
    (output-scale ,positive-number? "Internal.  This value globally
defines the output scale LilyPond uses while creating output.  Use
@code{set-global-staff-size} or @code{layout-set-staff-size} to change
the scaling.  Default: 1.7573, which corresponds to a 20@dmn{pt} staff
size.")

;;
;; p
;;
    (page-breaking ,procedure? "The page-breaking algorithm to use.
Choices are @code{ly:minimal-breaking}, @code{ly:page-turn-breaking},
@code{ly:one-page-breaking}, @code{ly:one-line-breaking},
@code{ly:one-line-auto-height-breaking}, and
@code{ly:optimal-breaking}.  Default: @code{ly:optimal-breaking}.")
    (page-breaking-system-system-spacing ,alist? "This tricks the
page breaker into thinking that @code{system-system-spacing} is set to
something different than it really is.  For example, if
@code{page-breaking-system-system-spacing.padding} is set to something
substantially larger than @code{system-system-spacing.padding}, then
the page breaker puts fewer systems on each page.  Default: unset.")
    (page-count ,index? "The number of pages to be used for a score.
Default: unset.")
    (page-number-type ,symbol? "The type of numerals used for page
numbers.  Choices include @code{arabic}, @code{roman-ij-lower},
@code{roman-ij-upper}, @code{roman-lower}, and @code{roman-upper}.
Default: @code{arabic}.")
    (page-post-process ,procedure? "If this function is defined
(within the @code{\\paper} block), LilyPond uses it to post-process
pages, for example, to extract the table of contents and writing the
information to an auxiliary file.  The call happens after page
breaking has been performed.

The syntax is

@example
#(define (page-post-process layout pages) ... )
@end example
")
    (page-spacing-weight ,number? "When using the
@code{ly:optimal-breaking} algorithm for page breaking, LilyPond has
to make trade-offs between horizontal and vertical stretching so that
the overall spacing is more acceptable.  This parameter controls the
relative importance of (vertical) page spacing and (horizontal) line
spacing.  High values makes page spacing more important.  Default: 10.")
    (paper-height ,positive-number? "The height of the page.
Note that the automatic scaling of some vertical dimensions is not
affected if you set this value directly.  Default: 297@dmn{mm} (A4
paper height).")
    (papersizename ,string? "Internal.  Use
@code{set-default-paper-size} or @code{set-paper-size} to set the
paper size.")
    (paper-width ,positive-number? "The width of the page.  While
setting @code{paper-width} directly has no effect on the automatic
scaling of some horizontal dimensions, it does influence the
@code{line-width} variable.  If both @code{paper-width} and
@code{line-width} are set, then @code{left-margin} and
@code{right-margin} also get updated.  Also see
@code{check-consistency}.  Default: 210@dmn{mm} (A4 paper width).")
    (print-all-headers ,boolean? "If set to @code{#t}, print all
headers for each @code{\\score} in the output.  If set to @code{#f},
only the @code{piece} and @code{opus} header variables are printed.
Default: @code{#f}.")
    (print-first-page-number ,boolean? "If set to @code{#t}, a page
number is printed on the first page.  Default: @code{#f}.")
    (print-page-number ,boolean? "If set to @code{#f}, page numbers
are not printed.  Default: @code{#t}.")
    (property-defaults ,alist? "This variable holds some settings
that are used for top-level markups and as fallback values if nothing
else is specified, for example, the standard fonts or the baseline
skip.  The default value is defined in file
@file{paper-defaults-init.ly}.")
    (pt ,positive-number? "The point unit, equal to approx.@:
0.351@dmn{mm} (0.0139@dmn{in}).")

;;
;; r
;;
    (ragged-bottom ,boolean? "If set to @code{#t}, systems are set at
at their natural spacing, neither compressed nor stretched vertically
to fit the page.  Default: @code{#f}.")
    (ragged-last ,boolean? "If set to @code{#t}, the last system in
the score does not fill the line width.  Instead, the last system ends
at its natural horizontal length.  Default: @code{#f}.")
    (ragged-last-bottom ,boolean? "If set to @code{#f}, then the last
page, and the last page in each section created with a
@code{\\bookpart} block, is vertically justified in the same way as the
earlier pages.  Default: @code{#t}.")
    (ragged-right ,boolean? "If set to @code{#t}, systems don't fill
the line width.  Instead, systems end at their natural horizontal
length.  Default: @code{#t} for scores with only one system, and
@code{#f} for scores with two or more systems.")
    (reset-footnotes-on-new-page ,boolean? "If set to @code{#t},
footnote and in-note numbers are reset on each page break.  For
footnotes and in-notes numbered consecutively across page breaks, set
to @code{#f}.  Default: @code{#t}.")
    (right-margin ,non-negative-number? "The margin between the right
edge of the page and the end of the staff lines in non-ragged systems.
If @code{right-margin} is not set, and both @code{line-width} and
@code{left-margin} are set, then @code{right-margin} is set to
@code{(paper-width - line-width - left-margin)}.  If only
@code{line-width} is set, then both margins are set to
@code{((paper-width - line-width) / 2)}, and the systems are
consequently centered on the page.  If unset,
@code{right-margin-default} (15@dmn{mm}, scaled to paper size) is used
in computations.  Default: unset.

If @code{two-sided} is set to @code{#t}, this value is ignored.  Also
see @code{check-consistency}.")

;;
;; s
;;
    (score-markup-spacing ,alist? "The distance between the last
system of a score and the (title or top-level) markup that follows it.
The default value is defined in file @file{paper-defaults-init.ly}.")
    (score-system-spacing ,alist? "The distance between the last
system of a score and the first system of the score that follows it,
if no (title or top-level) markup exists between them.  The default
value is defined in file @file{paper-defaults-init.ly}.")
    (scoreTitleMarkup ,markup? "The titling markup of a score within
a @code{\\book}, using standard fields from the @code{\\header} block.
The default value is defined in file @file{titling-init.ly}.")
    (score-title ,procedure? "Internal.  This is what LilyPond
actually uses for handling @code{scoreTitleMarkup}.")
    (score-title-properties ,alist? "Internal.")
    (short-indent ,non-negative-number? "The level of indentation for
all systems in a score besides the first system.  The space within the
@code{line-width} available for systems other than the first one is
reduced by this amount.  Default: @code{short-indent-default}
(0@dmn{mm}).")
    (staff-height ,positive-number? "Internal.  Use
@code{set-global-staff-size} or @code{layout-set-staff-size} to set
the staff height.  Default: 20@dmn{pt}.")
    (staff-space ,positive-number? "Internal.  One staff height
contains four staff spaces.  Use @code{set-global-staff-size} or
@code{layout-set-staff-size} to set this value.  Default: 5@dmn{pt}.")
    (system-count ,index? "The number of systems to be used for a
score.  Default: unset.")
    (systems-per-page ,index? "The number of systems that should be
placed on a page.  This is currently supported only by the
@code{ly:optimal-breaking} algorithm.  Default: unset.")
    (system-separator-markup ,markup? "A markup object that is
inserted between systems, often used for orchestral scores.  The
@code{\\slashSeparator} markup, defined in file
@file{titling-init.ly}, is provided as a sensible standard value.
Default: unset.")
    (system-system-spacing ,alist? "The distance between two systems
in the same score.  The default value is defined in file
@file{paper-defaults-init.ly}.")

;;
;; t
;;
    (tagline ,markup? "This markup gets placed at the bottom of the
last page (via @code{oddFooterMarkup}).  The default value is defined
in file @file{titling-init.ly}.")
    (text-font-size ,positive-number? "Internal.  This value globally
defines the standard text size for markups.  The default depends on
the staff height and is computed as @code{(staff-height / 20 * 11)}.")
    (tocFormatMarkup ,procedure? "How the top-level entries of the
table of contents are formatted (if there are several hierarchical
levels).  This variable holds a function like @code{make-bold-markup}.
The default value is defined in file @file{toc-init.ly}.")
    (tocIndentMarkup ,markup? "How the outline’s hierarchy in the
table of contents is made apparent.  This markup is printed zero, one,
or several times depending on the level of each entry.  The default
value is defined in file @file{toc-init.ly}.")
    (tocItemMarkup ,markup? "How an item in the table of contents is
formatted.  The default value of this markup is defined in file
@file{toc-init.ly}.")
    (tocTitleMarkup ,markup? "How the title of the table of contents
is formatted.  The default value of this markup is defined in file
@file{toc-init.ly}.")
    (top-margin ,non-negative-number? "The margin between the top of
the page and the top of the printable area.  Default:
@code{top-margin-default} (10@dmn{mm}, scaled to paper size).")
    (top-markup-spacing ,alist? "The distance from the top of the
printable area (i.e., the bottom of the top margin) to the
first (title or top-level) markup on a page, when there is no system
between the two.  The default value is defined in file
@file{paper-defaults-init.ly}.")
    (top-system-spacing ,alist? "The distance from the top of the
printable area (i.e., the bottom of the top margin) to the first
system on a page, when there is no (title or top-level) markup between
the two.  The default value is defined in file
@file{paper-defaults-init.ly}.")
    (two-sided ,boolean? "If set to @code{#t}, use
@code{inner-margin}, @code{outer-margin} and @code{binding-offset} to
determine margins depending on whether the page number is odd or even.
@code{left-margin} and @code{right-margin} are then ignored.  Default:
@code{#f}.")

     ))
