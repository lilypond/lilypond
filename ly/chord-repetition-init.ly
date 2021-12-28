%%% -*- Mode: Scheme -*-
%%%% Chord repetition functions.
%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2009--2022 Nicolas Sceaux <nicolas.sceaux@free.fr>
%%%% Copyright (C) 2012--2022 David Kastrup <dak@gnu.org>
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

\version "2.19.22"
%{
  Chord repetition behavior is not customizable in the parser.  That
  is due to it usually being done by the toplevel music handler
  affecting every bit of music at the same time, not closely related
  to music input.  Customized behavior is instead accomplished by
  calling \chordRepeats explicitly on some music list with a list of
  event types you wish to keep by default (if any events of that kind
  are found already on the repeat chord, however, they still get
  removed from the original).

  The default behavior is straightforward: don't keep anything but the
  rhythmic events themselves.
%}

chordRepeats =
#(define-music-function (event-types music)
   ((list? '()) ly:music?)
   "Walk through @var{music} putting the notes of the previous chord
into repeat chords, as well as an optional list of @var{event-types}
such as @code{#'(string-number-event)}."
   (expand-repeat-chords! (cons 'rhythmic-event event-types) music))

tabChordRepeats =
#(define-music-function (event-types music)
   ((list? '()) ly:music?)
   "Walk through @var{music} putting the notes, fingerings and string
numbers of the previous chord into repeat chords, as well as an
optional list of @var{event-types} such as @code{#'(articulation-event)}."
   #{ \chordRepeats
      #(append '(string-number-event fingering-event) event-types)
      #music
   #})

tabChordRepetition =
#(define-void-function () ()
   (_i "Include the string and fingering information in a chord repetition.
This function is deprecated; try using @code{\\tabChordRepeats} instead.")
   (ly:parser-define! '$chord-repeat-events
                      '(string-number-event fingering-event)))
