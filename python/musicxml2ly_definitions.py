# -*- coding: utf-8 -*-
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2016--2024 John Gourlay <john@weathervanefarm.net>
# Copyright (C) 2024--2024 Werner Lemberg <wl@gnu.org>
#
# LilyPond is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# LilyPond is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.


additional_definitions = {
    # TODO: Implement values `both` and `arrow` of `line-end` attribute.
    "make-edge-height": """\
#(define edge-height-alist
   '((up . -0.7)
     (down . 0.7)
     (none . 0)))

#(define (get-height type)
   (or (assv-ref edge-height-alist type)
       (begin
         (ly:warning "bracket edge type '~a' not implemented" type)
         0)))

% Make the `edge-height` property independent of a bracket's direction.
#(define (make-edge-height left right)
   (grob-transformer
    'edge-height
    (lambda (grob orig)
      (let ((dir (ly:grob-property grob 'direction))
            (left-height (get-height left))
            (right-height (get-height right)))
        (cons (* dir left-height) (* dir right-height))))))
""",

    "make-bracketed": """\
#(define make-bracketed
   (grob-transformer
    'stencil
    (lambda (grob orig)
      (let* ((paren-stil (grob-interpret-markup
                          grob
                          (markup #:musicglyph "accidentals.leftparen")))
             (ext (ly:stencil-extent paren-stil Y))
             (stil (ly:accidental-interface::print grob))
             (thick (ly:output-def-lookup (ly:grob-layout grob)
                                          'line-thickness 0.1))
             (padding thick)
             (protrusion (* 2.5 thick))
             (lb (ly:bracket Y ext thick protrusion))
             (rb (ly:bracket Y ext thick (- protrusion))))
        (set! stil (ly:stencil-combine-at-edge stil X 1 rb padding))
        (set! stil (ly:stencil-combine-at-edge stil X -1 lb padding))
        stil))))

bracketAcc =
  \\tweak AccidentalCautionary.parenthesized ##f
  \\tweak AccidentalCautionary.stencil #make-bracketed \\etc
""",

    "hide-note": """\
hideNote =
  \\tweak Dots.transparent ##t
  \\tweak NoteHead.transparent ##t
  \\tweak NoteHead.no-ledgers ##t
  \\tweak Stem.transparent ##t
  \\tweak Accidental.transparent ##t
  \\tweak Rest.transparent ##t
  \\tweak TabNoteHead.transparent ##t \\etc
""",

    "insert-before": """\
#(define (insert-before where what lst)
   (cond ((null? lst)
          (list what))
         ((eq? where (car lst))
          (cons what lst))
         (else
          (cons (car lst) (insert-before where what (cdr lst))))))
""",

    "cancel-before-barline": """\
cancelBeforeBarline = {
  \\once \\set Staff.printKeyCancellation = ##t
  \\once \\override Score.BreakAlignment.break-align-orders =
  #(grob-transformer
    'break-align-orders
    (lambda (grob orig)
      (let* ((middle (vector-ref orig 1))
             (middle (delq 'key-cancellation middle))
             (middle (insert-before 'staff-bar 'key-cancellation middle)))
        (vector
         (vector-ref orig 0)
         middle
         (vector-ref orig 2)))))
}
""",

    "cancel-after-key": """\
cancelAfterKey = {
  \\once \\set Staff.printKeyCancellation = ##t
  \\once \\override Score.BreakAlignment.break-align-orders =
  #(grob-transformer
    'break-align-orders
    (lambda (grob orig)
      (let* ((middle (vector-ref orig 1))
             (middle (delq 'key-signature middle))
             (middle (insert-before 'key-cancellation 'key-signature middle)))
        (vector
         (vector-ref orig 0)
         middle
         (vector-ref orig 2)))))
}
""",

    "staff-lines": """\
% Change the number of staff lines to `num-lines`.
%
% The argument `clef` makes the function emit a clef with the given
% name.  If `clef` contains the substring `"percussion"` or `"tab"`,
% use both even and odd staff line positions.  If set to `""`, do the
% same but don't set `Staff.clefPosition` (which means that no clef
% gets triggered).  If `clef` is set to any other value, use even
% staff line positions only and set `Staff.clefPosition`, which
% triggers a clef if its value changes.
%
% The optional argument `properties` is an alist of properties to
% control the appearance of both the staff and the clef:
%
% * `details` is a list of staff line numbers that should be
%   displayed.  An empty list suppresses any display of staff lines;
%   omitting the argument means to display all lines.
% * `staff-color` holds the color of the staff.
% * `clef-font-size` holds the font size of the clef
% * `clef-color` holds the color of the clef.
%
% \\staffLines [<properties>] <clef> <num-lines>

staffLines =
#(define-music-function (properties clef num-lines)
                        ((alist? '()) string? index?)
   (let* ((details (assoc-get 'details properties #f))
          (staff-color (assoc-get 'staff-color properties #f))
          (clef-color (assoc-get 'clef-color properties #f))
          (clef-font-size (assoc-get 'clef-font-size properties #f))
          (lines (or details (iota num-lines 1)))
          (only-even-pos (not (or (equal? clef "")
                                  (string-contains clef "percussion")
                                  (string-contains clef "tab"))))
          (offset (if only-even-pos
                      (if (even? num-lines) 1 0)
                      0))
          ;; MusicXML counts staff lines from the bottom.
          (delta (- -1 num-lines offset))
          (positions (filter (lambda (x) (<= x num-lines))
                             lines))
          (positions (map (lambda (x) (+ delta (* x 2)))
                      positions)))
     #{
       \\stopStaff

       #(if staff-color
            #{
              \\once \\override Staff.StaffSymbol.color = #staff-color
            #})
       #(if clef-color
            #{
              \\once \\override Staff.Clef.color = #clef-color
            #})
       #(if clef-font-size
            #{
              \\once \\override Staff.Clef.font-size = #clef-font-size
            #})

       #(if (equal? clef "")
            #{
              \\unset Staff.middleCPosition
              \\unset Staff.middleCClefPosition
              % To suppress the clef inserted by default by LilyPond at the
              % beginning of a piece.
              \\once \\omit Staff.Clef
            #}
            #{ \\clef #clef #})

       \\override Staff.StaffSymbol.line-positions = #positions

       \\applyContext
       #(lambda (ctx)
          (let* ((c (ly:context-find ctx 'Staff))
                 (clef-p (ly:context-property c 'clefPosition))
                 (middle-c-p (ly:context-property c 'middleCPosition))
                 (middle-c-clef-p (ly:context-property c 'middleCClefPosition))
                 (use-offset (not (string-contains clef "percussion")))
                 (delta (+ delta (if use-offset 6 0))))
            (when only-even-pos
              (ly:context-set-property! c 'clefPosition (+ clef-p delta))
              (ly:context-set-property! c 'forceClef #t))
            (ly:context-set-property! c 'middleCPosition (+ middle-c-p delta))
            (ly:context-set-property! c 'middleCClefPosition
                                      (+ middle-c-clef-p delta))))
       \\startStaff
     #}))
""",

    "square": """\
#(define-markup-command (square layout props arg)
   (markup?)
   (let* ((stil (interpret-markup layout props arg))
          (x-size (interval-length (ly:stencil-extent stil X)))
          (y-size (interval-length (ly:stencil-extent stil Y)))
          (axis (if (> x-size y-size) Y X)))
     (interpret-markup
      layout props
      #{
        \\markup \\box \\with-dimension-from #axis \\rotate #90 #arg #arg
      #})))
""",

    "accidental-marks": """\
% \\accs-ornament
% --------------
%
% Position the `above` and `below` markup lists as vertical stacks
% above and below markup `script`, respectively.  `above` and `below`
% are by default horizontally centered on `script` (controlled by the
% property `acc-dir`), printed at a size given by the property
% `acc-font-size` (default value -5), and a color given by the
% property `acc-color` (default is black).  Between all markups a
% vertical space given by the property `acc-padding` (default value
% 0.2) is inserted.  The `above` stack grows from bottom to top, while
% the `below` stack grows from top to bottom.
%
% If either `above` or `below` is an empty argument, or if `script` is
% set to `#f`, the corresponding argument is not printed.
%
% If an element of `above`, `script`, or `below` is a plain string,
% check the following.
%
% * If the string consists entirely of one or more characters from the
%   accidental set '♮♭♯𝄪𝄫' (not counting a possible enclosure), add
%   `\\number` internally as a prefix to use the Emmentaler font.
%
% * Otherwise the element is taken as a glyph name (again not counting
%   a possible enclosure), to be internally accessed with the
%   `\\musicglyph` command.
%
% * (Not for `script`.)  If an element of `above` or `below` is a
%   plain string and the first and last character is `(` and `)`,
%   respectively, the element gets enclosed in parentheses.  The same
%   holds for `[` and `]`, enclosing the element in brackets.  The
%   font size of the enclosure characters is controlled by the
%   property `enclosure-font-size` (default value -2).
%
% Example:
%
% ```
% \\markup \\override #'(acc-padding . 0.5)
%         \\accs-ornament { "♭" "(♯)" } "scripts.haydnturn" {}
% ```

#(define-markup-command (accs-ornament layout props
                                       above script below)
   (markup-list? markup? markup-list?)
   #:properties ((acc-font-size -5)
                 (acc-color black)
                 (acc-padding 0.3)
                 (acc-dir CENTER)
	         (enclosure-font-size -2))

   (define enclosure-regex
     ;; `(?(-3) ...)` is a 'conditional subpattern' that is only
     ;; considered for matching if the subpattern three opening
     ;; parentheses earlier (i.e., the first group) matches.
     (ly:make-regex "(?x) ^ ( [([] ) ?
                            ( [^]()[] + )
                            ( (?(-3) [])] ) ) $"))

   (define charset:accidentals (string->char-set "♮♭♯𝄪𝄫"))

   (define (musicglyph-or-number-markup arg)
     (if (string-every charset:accidentals arg)
         (make-number-markup arg)
         (make-musicglyph-markup arg)))

   (define (with-enclosure-markup arg)
     (let ((match (ly:regex-exec enclosure-regex arg)))
       (if match
           (let* (;; `left` is `#f` if there is no enclosure.
                  (left (ly:regex-match-substring match 1))
                  (left (if left
                            (make-normalsize-markup
                             (make-fontsize-markup
                              enclosure-font-size left))
                            #f))
                  (glyph (ly:regex-match-substring match 2))
                  ;; `right` is `""` if there is no enclosure.
                  (right (ly:regex-match-substring match 3))
                  (right (if (string-null? right)
                             #f
                             (make-normalsize-markup
                              (make-fontsize-markup
                               enclosure-font-size right)))))
             (make-concat-markup
              (list
               (or left "")
               (musicglyph-or-number-markup glyph)
               (or right ""))))
           #f)))

   (define (to-markup arg enclosure)
     (if (string? arg)
	 (if (string-null? arg)
             #f
             (if enclosure
                 (with-enclosure-markup arg)
                 (musicglyph-or-number-markup arg)))
         arg))

   (define (make-acc-stencil arg)
     (if arg
         (interpret-markup
          layout props
          (make-halign-markup
           acc-dir
           (make-with-color-markup
            acc-color
            (make-fontsize-markup
             acc-font-size arg))))
         #f))

   (let* ((above (map (lambda (x) (to-markup x #t)) above))
          (above-stils (map (lambda (x)
                              (make-acc-stencil x))
                            above))

          (script (to-markup script #f))
          (script-stil (if script
                           (interpret-markup layout props script)
                           empty-stencil))

          (below (map (lambda (x) (to-markup x #t)) below))
          (below-stils (map (lambda (x)
                              (make-acc-stencil x))
                            below))

          (script-stil (fold (lambda (elem previous)
                               (ly:stencil-combine-at-edge
                                previous Y UP elem acc-padding))
                             script-stil
                             above-stils))
          (script-stil (fold (lambda (elem previous)
                               (ly:stencil-combine-at-edge
                                previous Y DOWN elem acc-padding))
                             script-stil
                             below-stils)))
     script-stil))


% \\acc-ornament
% -------------
%
% This function behaves similar to `\\accs-ornament` with the
% difference that `above` and `below` are single markups, not markup
% lists.  If one of the arguments is an empty string, it is not
% printed.
%
% Example:
%
% ```
% \\markup \\acc-ornament "[♭]" "scripts.turn" ♯
% ```

#(define-markup-command (acc-ornament layout props
                                      above script below)
   (markup? markup? markup?)
   #:properties (accs-ornament-markup)
   (accs-ornament-markup layout props
                         (if (and (string? above) (string-null? above))
                             '()
                             (list above))
                         (if (and (string? script) (string-null? script))
                             #f
                             script)
                         (if (and (string? below) (string-null? below))
                             '()
                             (list below))))


% Some shorthands to make the usage of `\\acc-ornament` more
% comfortable for simple cases.


ornament =
#(define-music-function (above script below)
   (string? string? string?)
   #{
     \\tweak parent-alignment-X #CENTER
     \\tweak self-alignment-X #CENTER
     -\\markup \\acc-ornament #above #script #below
   #})


accTrill =
#(define-music-function (above)
   (string?)
   #{
     \\tweak parent-alignment-X #CENTER
     \\tweak self-alignment-X #CENTER
     -\\markup
        \\override #'((acc-dir . -1.3)
                     (acc-padding . -0.4))
        \\acc-ornament #above "scripts.trill" ""
   #})


#(define-markup-command (trill-acc-tweak layout props above)
   (string?)
   (interpret-markup
    layout props
    #{
      \\markup
        \\override #'((acc-dir . -1.3)
                     (acc-padding . -0.4))
        \\with-dimension-from #X
          \\with-true-dimension #X \\musicglyph "scripts.trill"
            \\acc-ornament
              #above
              \\with-true-dimension #X \\musicglyph "scripts.trill"
          ""
    #}))


% `color` and `size` are optional.  If `size` is present, `color` must
% be present, too.  `size` also sets the `enclosure-font-size`
% property (three magsteps larger).
trillTweak =
#(define-music-function (acc color size music)
   (string? (color? black) (number?) ly:music?)
   (let* ((override (list `(acc-color . ,color)))
          (override (if size
                        (cons `(enclosure-font-size . ,(+ size 3))
                         (cons `(acc-font-size . ,size) override))
                        override)))
     #{
       \\tweak bound-details.left.text
         \\markup \\override #override
                   \\trill-acc-tweak #acc
       #music
     #}))
""",

    "harmonic": """\
harmonicSmall =
  \\tweak NoteHead.font-size #-3
  \\tweak NoteHead.duration-log #4
  \\etc

harmonicParen =
#(define-music-function (size offset music)
   (number? number? ly:music?)
   #{
     \\tweak Parentheses.font-size #size
     \\tweak Parentheses.Y-offset #offset
     \\parenthesize
     #music
   #})
""",

    "fingering-substitution": """\
substFinger =
#(define-music-function (start left right)
   (markup? markup? markup?)
   #{
     \\tweak self-alignment-X #-0.6
     -\\finger \\markup
       \\concat {
         #start
         \\overtie \\concat { #left \\char ##x2009 #right } }
   #})
""",

    "pluck": """\
RH = \\rightHandFinger \\etc
""",

    "crescendo": """\
Cresc = #(make-music 'CrescendoEvent 'span-direction START
                                     'span-type 'text)
""",

    "decrescendo": """\
Decresc = #(make-music 'DecrescendoEvent 'span-direction START
                                         'span-type 'text)
""",

    "for-barline": """\
% This `\\caesura` creates a light-light bar line where there isn't
% already a more significant bar line, and aligns its decorations
% (e.g., fermatas) to the bar line.
forBarLine =
{
  \\once \\set Staff.caesuraType = #'((underlying-bar-line . "||"))
  \\once \\set Staff.caesuraTypeTransform = ##f
  \\caesura
}
""",
}
