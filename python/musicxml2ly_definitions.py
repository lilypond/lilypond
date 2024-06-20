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
    "tuplet-note-wrapper": """\
% A formatter function, which is simply a wrapper around an existing
% tuplet formatter function.  It takes the value returned by the given
% function and appends a note of given length.
#(define-public ((tuplet-number::append-note-wrapper function note) grob)
  (let* ((txt (if function (function grob) #f)))
    (if txt
      (markup txt #:fontsize -5 #:note note UP)
      (markup #:fontsize -5 #:note note UP))))
""",

    "tuplet-non-default-denominator": """\
#(define ((tuplet-number::non-default-tuplet-denominator-text denominator) grob)
  (number->string (if denominator
                      denominator
                      (ly:event-property (event-cause grob) 'denominator))))
""",

    "tuplet-non-default-fraction": """\
#(define ((tuplet-number::non-default-tuplet-fraction-text denominator numerator) grob)
  (let* ((ev (event-cause grob))
         (den (if denominator denominator (ly:event-property ev 'denominator)))
         (num (if numerator numerator (ly:event-property ev 'numerator))))
    (format #f "~a:~a" den num)))
""",

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
   (interpret-markup
    layout props
    #{ \\markup \\box \\with-dimension-from #Y \\rotate #90 #arg #arg #}))
"""
}
