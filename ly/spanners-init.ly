%%%% Spanners.
%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 1999--2026 Han-Wen Nienhuys <hanwen@xs4all.nl>
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


startGroup = #(make-span-event 'NoteGroupingEvent START)
stopGroup = #(make-span-event 'NoteGroupingEvent STOP)


cr = #(make-span-event 'CrescendoEvent START)
decr = #(make-span-event 'DecrescendoEvent START)
enddecr = #(make-span-event 'DecrescendoEvent STOP)
endcr = #(make-span-event 'CrescendoEvent STOP)


startGradualTempoChange =
#(define-music-function (text) ((markup?))
   (_i "Start an accelerando or ritardando.

Begin a gradual departure from the tempo most recently set with @code{\\tempo}.
The gradual change extends to the next @code{\\stopGradualTempoChange} or
@code{\\tempo} command.  When @code{\\stopGradualTempoChange} is used, it
defines the target tempo; otherwise, @code{\\tempo} defines it.

The @var{text} argument is a placeholder.  Only @code{\\default} is accepted.
It is intended eventually to serve the same purpose as the text argument to
@code{\\tempo}.")
   (when text
     (ly:input-warning (*location*) (G_ "text option not implemented")))
   (make-span-event 'TempoGradualChangeEvent START))

stopGradualTempoChange =
#(define-music-function (text tempo-unit metronome-count)
   ((markup?) ly:duration? number?)
   (_i "End an accelerando or ritardando.

End a gradual tempo change at the tempo defined by @var{tempo-unit} and
@var{metronome-count}.  Unless there is a simultaneous @code{\\tempo} command
setting a new tempo, resume the tempo most recently set with @code{\\tempo}.

The @var{text} argument is a placeholder.  Only @code{\\default} is accepted.
It is intended eventually to serve the same purpose as the text argument to
@code{\\tempo}.")
   (when text
     (ly:input-warning (*location*) (G_ "text option not implemented")))
   (make-music 'TempoGradualChangeEvent
               'span-direction STOP
               'tempo-unit tempo-unit
               'metronome-count metronome-count))


startMeasureSpanner = #(make-span-event 'MeasureSpannerEvent START)
stopMeasureSpanner = #(make-span-event 'MeasureSpannerEvent STOP)


startMeasureCount = #(make-span-event 'MeasureCounterEvent START)
stopMeasureCount = #(make-span-event 'MeasureCounterEvent STOP)


startOptionalMaterial = #(make-span-event 'OptionalMaterialEvent START)
stopOptionalMaterial = #(make-span-event 'OptionalMaterialEvent STOP)


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
