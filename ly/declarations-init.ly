%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 1996--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>
%%%%                          Jan Nieuwenhuizen <janneke@gnu.org>
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

%% named durations
breve = #(ly:make-duration -1 0)
longa = #(ly:make-duration -2 0)
maxima = #(ly:make-duration -3 0)

\include "music-functions-init.ly"
\include "toc-init.ly"

%% default note names are dutch
#(set! default-language "nederlands")
#(note-names-language default-language)

\include "drumpitch-init.ly"
\include "chord-modifiers-init.ly"
\include "script-init.ly"

\include "chord-repetition-init.ly"

#(define default-fret-table (make-hash-table 101))
#(define chord-shape-table (make-hash-table 29))
#(call-after-session
  (lambda ()
   (hash-clear! default-fret-table)
   (hash-clear! chord-shape-table)))

% declarations for standard directions
left = #-1
right = #1
up = #1
down = #-1
start = #-1
stop = #1
smaller = #-1
bigger = #1
center = #0

%% FIXME
%% should also set \override Beam.breakable, but how to do it "portably"? (ie. also
%% working with lyric sections)
%%
%% try \once \override Score.Beam.breakable = ##t

%% rather name \newline, \pageBreak ?
break = #(make-music 'LineBreakEvent 'break-permission 'force)
noBreak = #(make-music 'LineBreakEvent 'break-permission '())
%% \pageBreak, \noPageBreak, \pageTurn, \noPageTurn, \allowPageTurn are defined
%% as music functions

stopStaff = #(make-span-event 'StaffSpanEvent STOP)
startStaff = #(make-span-event 'StaffSpanEvent START)


%
% Code articulation definitions
%
noBeam = #(make-music 'BeamForbidEvent)
"|" = #(make-music 'BarCheck)
"[" = #(make-span-event 'BeamEvent START)
"]" = #(make-span-event 'BeamEvent STOP)
"~" = #(make-music 'TieEvent)
"(" =  #(make-span-event 'SlurEvent START)
")" = #(make-span-event 'SlurEvent STOP)
"\\!" = #(make-span-event 'CrescendoEvent STOP)
"\\(" = #(make-span-event 'PhrasingSlurEvent START)
"\\)" = #(make-span-event 'PhrasingSlurEvent STOP)
"\\>" = #(make-span-event 'DecrescendoEvent START)
"\\<" = #(make-span-event 'CrescendoEvent START)
"\\[" = #(make-span-event 'LigatureEvent START)
"\\]" = #(make-span-event 'LigatureEvent STOP)
"\\~" = #(make-music 'PesOrFlexaEvent)
"\\\\" = #(make-music 'VoiceSeparator)
"\\-" = #(make-music 'DurationLineEvent)


\include "scale-definitions-init.ly"

melisma = #(context-spec-music (make-property-set 'melismaBusy #t) 'Bottom)
melismaEnd = #(context-spec-music (make-property-unset 'melismaBusy) 'Bottom)

laissezVibrer = #(make-music 'LaissezVibrerEvent)
repeatTie = #(make-music 'RepeatTieEvent)

\include "dynamic-scripts-init.ly"
\include "spanners-init.ly"

%% MAKE-HASH-TABLE in GUILE 1.6 takes mandatory size parameter.
#(define musicQuotes (make-hash-table 29))
#(call-after-session
  (lambda ()
   (hash-clear! musicQuotes)))

#(define toplevel-book-handler print-book-with-defaults)
#(define toplevel-bookpart-handler collect-bookpart-for-book)
#(define toplevel-music-handler collect-music-for-book)
#(define toplevel-score-handler collect-scores-for-book)
#(define toplevel-text-handler collect-scores-for-book)

#(define book-bookpart-handler ly:book-add-bookpart!)
#(define book-music-handler collect-book-music-for-book)
#(define book-score-handler ly:book-add-score!)
#(define book-text-handler ly:book-add-score!)

#(define bookpart-score-handler ly:book-add-score!)
#(define bookpart-text-handler ly:book-add-score!)
#(define bookpart-music-handler collect-book-music-for-book)
#(define output-def-music-handler context-defs-from-music)
#(define context-mod-music-handler context-mod-from-music)

\include "predefined-fretboards-init.ly"
\include "string-tunings-init.ly"
\include "property-init.ly"

\include "grace-init.ly"
\include "midi-init.ly"
\include "paper-defaults-init.ly"
\include "context-mods-init.ly"

\layout {
  mm = #(ly:output-def-lookup $defaultpaper 'mm)
  unit = #(ly:output-def-lookup $defaultpaper 'unit)

  in = #(* 25.4 mm)
  pt = #(/ in 72.27)
  cm = #(* 10 mm)

  \include "engraver-init.ly"

  #(set-paper-dimension-variables (current-module))
}

#(set-default-paper-size (ly:get-option 'paper-size))
partCombineListener = \layout {
  \context {
    \Score
    skipTypesetting = ##t
    ignoreBarChecks = ##t
    measurePosition = #ZERO-MOMENT
    \alias "Timing"
  }
}

setDefaultDurationToQuarter = { c4 }
