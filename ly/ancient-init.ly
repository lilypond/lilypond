% ancient-init.ly
%%%% Gregorian and Mensural notation functions.
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

\version "2.25.5"

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

versus =
#(define-music-function (music) (ly:music?)
   (_i "Prepend character U+2123 (@sc{versicle}) to the lyrics represented by
@var{music}.")
   (add-prefix-to-lyrics "℣" music))

responsum =
#(define-music-function (music) (ly:music?)
   (_i "Prepend character U+211F (@sc{response}) to the lyrics represented by
@var{music}.")
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
   (_i "Add augmentum dots (@dfn{morae}) to Gregorian chant @var{expr}.")
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
#(define-music-function (music) (ly:music?)
   (_i "Make a ligature from Gregorian Chant @var{music}.

This is equivalent to enclosing @var{music} with @code{\\[} and @code{\\]}.")
   (make-ligature music))

%%% Local Variables:
%%% coding: utf-8
%%% End:
