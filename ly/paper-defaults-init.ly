%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2004--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
%%%%                          Jan Nieuwenhuizen <janneke@gnu.org>
%%%%                          Neil Puttock <n.puttock@gmail.com>
%%%%
%%%% LilyPond is free software: you can redistribute it and/or modify
%%%% it under the terms of the GNU General Public License as published by
%%%% the Free Software Foundation, either version 3 of the License, or
%%%% (at your option) any later version.
%%%%
%%%% LilyPond is distributed in the hope that it will be useful,
%%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%% GNU General Public License for more details.
%%%%
%%%% You should have received a copy of the GNU General Public License
%%%% along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.

\version "2.16.0"

\paper {
  #(set-paper-dimension-variables (current-module))

  %%% WARNING
  %%%
  %%% If you add any new dimensions, don't forget to update
  %%% the dimension-variables variable.  See paper.scm.

  unit = #(ly:unit)
  mm = 1.0
  cm = #(* 10 mm)
  in = 25.4
  pt = #(/ in 72.27)
  bp = #(/ in 72)

  % 20pt staff, 5 pt = 1.75 mm
  output-scale = #1.7573

  %% ugh. hard coded?
  #(layout-set-absolute-staff-size (* 20.0 pt))


  %% Automatic scaling to paper size:
  %%
  %% Margins, indents, and offsets marked "scaled to paper size"
  %% below apply to the default paper format given by
  %% (ly:get-option 'paper-size) and are scaled accordingly for
  %% other formats.


  %%
  %% Fixed vertical spacing
  %%
  top-margin-default = 5\mm     % scaled to paper-size
  bottom-margin-default = 6\mm  % scaled to paper-size
  ragged-bottom = ##f
  ragged-last-bottom = ##t  % best for shorter scores

  %%
  %% Flexible vertical spacing
  %%
  %% Note: these are not scaled; they are in staff-spaces.
  system-system-spacing = #'((basic-distance . 12)
                             (minimum-distance . 8)
                             (padding . 1)
                             (stretchability . 60))
  score-system-spacing = #'((basic-distance . 14)
                            (minimum-distance . 8)
                            (padding . 1)
                            (stretchability . 120))
  markup-system-spacing = #'((basic-distance . 5)
                             (padding . 0.5)
                             (stretchability . 30))
  score-markup-spacing = #'((basic-distance . 12)
                            (padding . 0.5)
                            (stretchability . 60))
  markup-markup-spacing = #'((basic-distance . 1)
                             (padding . 0.5))
  top-system-spacing = #'((basic-distance . 1)
                          (minimum-distance . 0)
                          (padding . 1))
  top-markup-spacing = #'((basic-distance . 0)
                          (minimum-distance . 0)
                          (padding . 1))
  last-bottom-spacing = #'((basic-distance . 1)
                           (minimum-distance . 0)
                           (padding . 1)
                           (stretchability . 30))


  %%
  %% Widths and (horizontal) margins
  %%
  left-margin-default = 10\mm   % scaled to paper-size
  right-margin-default = 10\mm  % scaled to paper-size
  check-consistency = ##t


  %%
  %% Two-sided mode
  %%
  two-sided = ##f
  inner-margin-default = 10\mm   % scaled to paper-size
  outer-margin-default = 20\mm   % scaled to paper-size
  binding-offset-default = 0\mm  % scaled to paper-size


  %%
  %% Indents
  %%
  indent-default = 15\mm       % scaled to paper-size
  short-indent-default = 0\mm  % scaled to paper-size


  %%
  %% Page breaking
  %%
  blank-after-score-page-penalty = 2
  blank-last-page-penalty = 0
  blank-page-penalty = 5
  page-breaking = #ly:optimal-breaking


  %%
  %% Footnotes
  %%
  footnote-separator-markup = \markup \fill-line { \override #'(span-factor . 1/2) \draw-hline }
  footnote-padding = 0.5\mm
  footnote-footer-padding = 0.5\mm
  footnote-number-raise = 0.5\mm
  footnote-numbering-function = #numbered-footnotes
  reset-footnotes-on-new-page = ##t

  %%
  %% Page numbering
  %%
  first-page-number = #1
  print-first-page-number = ##f
  print-page-number = ##t
  page-number-type = #'arabic

  %%
  %% Headers, footers, and titles
  %%
  #(define make-header (marked-up-headfoot 'oddHeaderMarkup 'evenHeaderMarkup))
  #(define make-footer (marked-up-headfoot 'oddFooterMarkup 'evenFooterMarkup))

  #(define-public book-title (marked-up-title 'bookTitleMarkup))
  #(define-public score-title (marked-up-title 'scoreTitleMarkup))
  #(define-public score-title-properties
     '((is-title . #t)
       (is-book-title . #f)))
  #(define-public book-title-properties
     '((is-title . #t)
       (is-book-title . #t)))

  \include "titling-init.ly"


  %%
  %% Fonts
  %%
  %% text-font-defaults is typically used as the last component of
  %% the alist chains passed around to markup commands.  When a
  %% grob interprets a markup, the alist chain consists in that
  %% grob's properties, and the defaults.  When interpreting a
  %% standalone markup, there are only the defaults.  Thus,
  %% text-font-defaults defines defaults for markup properties.
  %% font-defaults is used to select the font for grobs printed
  %% in a musical font.  Moreover, grobs are free to look up properties
  %% in these two alists (usually as fallbacks through the use
  %% of alist chains).
  #(define font-defaults
    `((alteration-glyph-name-alist . ,standard-alteration-glyph-name-alist)
      (font-family . feta)
      (font-encoding . fetaMusic)))

  % `latin1' is a dummy value for Pango fonts
  #(define text-font-defaults
     `((font-encoding . latin1)
       (baseline-skip . 3)
       (replacement-alist . ())
       (string-transformers . (,ly:perform-text-replacements))
       (word-space . 0.6)))
  \include "text-replacements.ly"

}
