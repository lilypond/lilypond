\version "1.9.8"

startGroup = #(make-span-event 'NoteGroupingEvent START)
stopGroup = #(make-span-event 'NoteGroupingEvent STOP)


cr = #(make-span-event 'CrescendoEvent START)
decr = #(make-span-event 'DecrescendoEvent START)
enddecr = #(make-span-event 'DecrescendoEvent STOP)
endcr = #(make-span-event 'CrescendoEvent STOP) 

%% TODO: remove for 2.0
rc = \endcr
rced = \enddecr

startTextSpan = #(make-span-event 'TextSpanEvent START)
stopTextSpan = #(make-span-event 'TextSpanEvent STOP)

cresc = \notes {
  #(ly:export (make-event-chord (list cr)))
  \property Voice.crescendoText = \markup { \italic "cresc." }
  \property Voice.crescendoSpanner = #'dashed-line
}

% ah, this is handy: maybe drop resetting of properties in
% dynamic-engraver ?
endcresc = \notes {
  #(ly:export (make-event-chord (list rc)))
  \property Voice.crescendoText \unset
  \property Voice.crescendoSpanner \unset
}

dim = \notes {
  #(ly:export (make-event-chord (list decr)))

  \property Voice.decrescendoText = \markup { \italic "dim." }
  \property Voice.decrescendoSpanner = #'dashed-line
}

enddim = \notes {
  #(ly:export (make-event-chord (list rced)))
   \property Voice.decrescendoText \unset
 \property Voice.decrescendoSpanner \unset
}

%{

cresc = \spanevent \start "crescendo"
endcresc = \spanevent \stop "crescendo"

%}

% better name sustainstart/stop? 
sustainUp = #(make-span-event 'SustainEvent STOP)
sustainDown = #(make-span-event 'SustainEvent START)

unaCorda = #(make-span-event 'UnaCordaEvent START)
treCorde = #(make-span-event 'UnaCordaEvent STOP)

sostenutoDown = #(make-span-event 'SostenutoEvent START)
sostenutoUp = #(make-span-event 'SostenutoEvent STOP)

%crescpoco = \property Voice.crescendoText = "cresc. poco a poco"
%decresc = \property Voice.crescendoText = "decr."
%dim = \property Voice.crescendoText = "dim."
