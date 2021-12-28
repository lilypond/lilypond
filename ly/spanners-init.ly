%%%% Spanners.
%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 1999--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

\version "2.19.29"

"\\=" =
#(define-event-function (id event) (key? ly:event?)
  (_i "This sets the @code{spanner-id} property of the following
@var{event} to the given @var{id} (non-negative integer or symbol).
This can be used to tell LilyPond how to connect overlapping
or parallel slurs or phrasing slurs within a single @code{Voice}.
@lilypond[quote,verbatim]
\\fixed c' { c\\=1( d\\=2( e\\=1) f\\=2) }
@end lilypond\n")
  (set! (ly:music-property event 'spanner-id) id)
  event)

startGroup = #(make-span-event 'NoteGroupingEvent START)
stopGroup = #(make-span-event 'NoteGroupingEvent STOP)


cr = #(make-span-event 'CrescendoEvent START)
decr = #(make-span-event 'DecrescendoEvent START)
enddecr = #(make-span-event 'DecrescendoEvent STOP)
endcr = #(make-span-event 'CrescendoEvent STOP)


startMeasureSpanner = #(make-span-event 'MeasureSpannerEvent START)
stopMeasureSpanner = #(make-span-event 'MeasureSpannerEvent STOP)


startMeasureCount = #(make-span-event 'MeasureCounterEvent START)
stopMeasureCount = #(make-span-event 'MeasureCounterEvent STOP)


startTextSpan = #(make-span-event 'TextSpanEvent START)
stopTextSpan = #(make-span-event 'TextSpanEvent STOP)


startTrillSpan = #(make-span-event 'TrillSpanEvent START)
stopTrillSpan = #(make-span-event 'TrillSpanEvent STOP)


episemInitium = #(make-span-event 'EpisemaEvent START)
episemFinis = #(make-span-event 'EpisemaEvent STOP)


cresc = #(make-music 'CrescendoEvent 'span-direction START 'span-type 'text 'span-text "cresc.")
endcresc =  #(make-span-event 'CrescendoEvent STOP)
dim = #(make-music 'DecrescendoEvent 'span-direction START 'span-type 'text 'span-text "dim.")
enddim =  #(make-span-event 'DecrescendoEvent STOP)
decresc = #(make-music 'DecrescendoEvent 'span-direction START 'span-type 'text 'span-text "decresc.")
enddecresc =  #(make-span-event 'DecrescendoEvent STOP)

% Deprecated functions:
% TODO: DEPRECATED_2_13_19
deprecatedcresc =  {
  $(make-event-chord (list cr))
  \once \set crescendoText = \markup { \italic "cresc." }
  \once \set crescendoSpanner = #'text
}


deprecateddim =  {
  $(make-event-chord (list decr))
  \once \set decrescendoText = \markup { \italic "dim." }
  \once \set decrescendoSpanner = #'text
}

deprecatedenddim =  {
  $(make-event-chord (list enddecr))
%  \unset decrescendoText
%  \unset decrescendoSpanner
}

deprecatedendcresc =  {
  $(make-event-chord (list endcr))
%  \unset crescendoText
%  \unset crescendoSpanner
}


%%%%%%%%%%%%%%%%

crescTextCresc = {
    \set crescendoText = \markup { \italic "cresc." }
    \set crescendoSpanner = #'text
}

dimTextDecresc = {
    \set decrescendoText = \markup { \italic "decresc." }
    \set decrescendoSpanner = #'text
}

dimTextDecr = {
    \set decrescendoText = \markup { \italic "decr." }
    \set decrescendoSpanner = #'text
}

dimTextDim = {
    \set decrescendoText = \markup { \italic "dim." }
    \set decrescendoSpanner = #'text
}

crescHairpin = {
    \unset crescendoText
    \unset crescendoSpanner
}

dimHairpin = {
    \unset decrescendoText
    \unset decrescendoSpanner
}


sustainOff = #(make-span-event 'SustainEvent STOP)
sustainOn = #(make-span-event 'SustainEvent START)

unaCorda = #(make-span-event 'UnaCordaEvent START)
treCorde = #(make-span-event 'UnaCordaEvent STOP)

sostenutoOn = #(make-span-event 'SostenutoEvent START)
sostenutoOff = #(make-span-event 'SostenutoEvent STOP)

%crescpoco = \set crescendoText = "cresc. poco a poco"
%decresc = \set crescendoText = "decr."
%dim = \set crescendoText = "dim."

newSpacingSection = #(make-event-chord (list (make-music 'SpacingSectionEvent)))

breakDynamicSpan = #(make-music 'BreakDynamicSpanEvent)
