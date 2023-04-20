%%%% Gregorian notation functions.
%%%%
%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2003--2023 Han-Wen Nienhuys <hanwen@xs4all.nl>,
%%%%                          Jürgen Reuter <reuter_j@web.de>
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

%%
%% Shortcuts common for all styles of Gregorian chant notation.
%%

\version "2.25.3"

%
% Declare memorable shortcuts for special Unicode characters
% that are used in chant notation.
%

% U+0132 (latin capital ligature IJ)
IJ = \lyricmode { Ĳ }
IIJ = \lyricmode { IĲ }

% U+0133 (latin small ligature ij)
ij = \lyricmode { ĳ }
iij = \lyricmode { iĳ }

%
% Given some music that represents lyrics, add a prefix to the first
% lyric event.
%
#(define (add-prefix-to-lyrics prefix music)
   (let ((found? #f))
     (map-some-music
      (lambda (m)
        (if found? m
            (and (music-is-of-type? m 'lyric-event)
                 (begin
                   (set! (ly:music-property m 'text)
                         (string-append prefix (ly:music-property m 'text)))
                   (set! found? #t)
                   m))))
      music)))

% Add U+2123 (versicle) as prefix to lyrics.
versus =
#(define-music-function (music) (ly:music?)
   (add-prefix-to-lyrics "℣" music))

% Add U+211F (response) as prefix to lyrics.
responsum =
#(define-music-function (music) (ly:music?)
   (add-prefix-to-lyrics "℟" music))

%
% Declare head prefix shortcuts.
%
virga =
  \once \override NoteHead.virga = ##t
stropha =
  \once \override NoteHead.stropha = ##t
inclinatum =
  \once \override NoteHead.inclinatum = ##t
auctum =
  \once \override NoteHead.auctum = ##t
descendens =
  \once \override NoteHead.descendens = ##t
ascendens =
  \once \override NoteHead.ascendens = ##t
pes =
  \once \override NoteHead.pes-or-flexa = ##t
flexa =
  \once \override NoteHead.pes-or-flexa = ##t
oriscus =
  \once \override NoteHead.oriscus = ##t
quilisma =
  \once \override NoteHead.quilisma = ##t
deminutum =
  \once \override NoteHead.deminutum = ##t
linea =
  \once \override NoteHead.linea = ##t
cavum =
  \once \override NoteHead.cavum = ##t

%
% Declare divisiones shortcuts.
%
virgula = {
  \once \set Staff.caesuraType = #'((breath . comma))
  \caesura
}
divisioMinima = {
  \once \set Staff.caesuraType = #'((breath . chantquarterbar))
  \caesura
}
divisioMaior = {
  \caesura \shortfermata
}
divisioMaxima = {
  \caesura \fermata
}
finalis = \section

%
% Declare articulation shortcuts.
%
accentus = #(make-articulation 'accentus)
ictus = #(make-articulation 'ictus)
semicirculus = #(make-articulation 'semicirculus)
circulus = #(make-articulation 'circulus)

%
% `\augmentum` increases the dot-count value of all note heads to which
% it is applied by one.
%
augmentum =
#(define-music-function (expr) (ly:music?)
   (shift-duration-log expr 0 1))

%
% Declare shortcut music functions for Liber Hymnarius neumes
% table (experimental).
%
#(define (make-ligature music)
   (make-music
    'SequentialMusic
    'elements (append
               (cons
                (make-music
                 'EventChord
                 'elements (list
                            (make-span-event 'LigatureEvent START)))
                (ly:music-property music 'elements))
               (list
                (make-music
                 'EventChord
                 'elements (list
                            (make-span-event 'LigatureEvent STOP)))))))

ligature =
#(define-music-function
   (location music) (ly:music?)
   (make-ligature music))

%
% Declare default layout; here for Vaticana style.  In case there will
% be additional styles, we may want to create style-specific `.ly` files
% for inclusion (e.g., `vaticana-init.ly`), move the style-dependent stuff
% over there, leave the style-independent Gregorian stuff here, and let
% the style-specific file (`vaticana-init.ly`) include this file.  The
% user then will have to include `vaticana-init.ly` instead of
% `gregorian.ly`.
%
\layout {
  indent = 0.0

  %%% TODO: should ragged-right be the default?
  %ragged-right = ##t
  ragged-last = ##t

  \context {
    \VaticanaStaff

    \override StaffSymbol.color = #red
    \override LedgerLineSpanner.color = #red
  }
  \context {
    \Score
    \remove Bar_number_engraver

    %%% FIXME: Musicologically seen, timing should be set to #f.
    %%% The question is how disruptive this change would be.
%{
    timing = ##f
    forbidBreakBetweenBarLines = ##f
%}

    \override SpacingSpanner.packed-spacing = ##t

    %%%
    %%% TODO: Play around with the following SpacingSpanner
    %%% settings to yield better spacing between ligatures.
    %%%
    %%% FIXME: setting #'spacing-increment to a small value
    %%% causes tons of "programming error: adding reverse spring,
    %%% setting to unit" messages.
    %%%
    % \override SpacingSpanner.base-shortest-duration = \musicLength 4
    % \override SpacingSpanner.shortest-duration-space = #0
    % \override SpacingSpanner.average-spacing-wishes = ##f
    % \override SpacingSpanner.spacing-increment = #0.0
    % \override SpacingSpanner.uniform-stretching = ##t
  }
}

%%% Local Variables:
%%% coding: utf-8
%%% End:
