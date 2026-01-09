%%%% Predefined chord modifiers.
%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 1998--2023 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

\version "2.21.0"

chordmodifiers = #default-chord-modifier-list

whiteTriangleMarkup = \markup {
  \fontsize #-3 \triangle ##f
  %% U+0394 capital delta
  %#(ly:wide-char->utf-8 #x0394)

  %% U+2206: delta from the symbol font.
  %   #(ly:wide-char->utf-8 #x2206)

  %% U+25B3: up pointing triangle
  % #(ly:wide-char->utf-8 #x25B3)
}

blackTriangleMarkup = \markup {
  \fontsize #-3 \triangle ##t

  %% black up pointing triangle
%  #(ly:wide-char->utf-8 #x25B2)
}

whiteCircleMarkup = \markup {
  %% U+00B0 is the degree sign. No need for \super here.
  \fontsize #2 #(ly:wide-char->utf-8 #x00b0)
}

ignatzekExceptionMusic = {
  <c e gis>-\markup { "+" }
  <c es ges>-\markup { \whiteCircleMarkup }
  <c es ges bes>-\markup {
    %% U+00F8 is character 'o' with a slash.
    \super #(ly:wide-char->utf-8 #x00f8) }
  <c es ges beses>-\markup \concat { \whiteCircleMarkup \super "7" }
  <c e g bes des' ees' fis' aes'>-\markup {
    \super "alt" } % altered chord (super-Locrian)
  % Power chords should be printed by default.
  <c g>-\markup { \super "5" }
  <c g c'>-\markup { \super "5" }
}

ignatzekExceptions =
#(sequential-music-to-chord-exceptions ignatzekExceptionMusic #t)
