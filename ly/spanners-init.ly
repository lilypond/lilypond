\version "2.2.0"

startGroup = #(make-span-event 'NoteGroupingEvent START)
stopGroup = #(make-span-event 'NoteGroupingEvent STOP)


cr = #(make-span-event 'CrescendoEvent START)
decr = #(make-span-event 'DecrescendoEvent START)
enddecr = #(make-span-event 'DecrescendoEvent STOP)
endcr = #(make-span-event 'CrescendoEvent STOP) 

startTextSpan = #(make-span-event 'TextSpanEvent START)
stopTextSpan = #(make-span-event 'TextSpanEvent STOP)


% TODO: junkme!
cresc = \notes {
  #(ly:export (make-event-chord (list cr)))
  \set crescendoText = \markup { \italic "cresc." }
  \set crescendoSpanner = #'dashed-line
}


dim = \notes {
  #(ly:export (make-event-chord (list decr)))

  \set decrescendoText = \markup { \italic "dim." }
  \set decrescendoSpanner = #'dashed-line
}

enddim = \notes {
  #(ly:export (make-event-chord (list enddecr)))
  \unset decrescendoText 
  \unset decrescendoSpanner 
}

% ah, this is handy: maybe drop resetting of properties in
% dynamic-engraver ?
endcresc = \notes {
  #(ly:export (make-event-chord (list endcr)))
  \unset crescendoText 
  \unset crescendoSpanner 
}

%%%%%%%%%%%%%%%%

setTextCresc = {
    \set crescendoText = \markup { \italic "cresc." }
    \set crescendoSpanner = #'dashed-line
}
setTextDecresc = {
    \set crescendoText = \markup { \italic "decr." }
    \set crescendoSpanner = #'dashed-line
}
setTextDim = {
    \set crescendoText = \markup { \italic "dim." }
    \set crescendoSpanner = #'dashed-line
}
setHairpinCresc = {
    \unset crescendoText 
    \unset crescendoSpanner 
}



% better name sustainstart/stop? 
sustainUp = #(make-span-event 'SustainEvent STOP)
sustainDown = #(make-span-event 'SustainEvent START)

unaCorda = #(make-span-event 'UnaCordaEvent START)
treCorde = #(make-span-event 'UnaCordaEvent STOP)

sostenutoDown = #(make-span-event 'SostenutoEvent START)
sostenutoUp = #(make-span-event 'SostenutoEvent STOP)

%crescpoco = \set crescendoText = "cresc. poco a poco"
%decresc = \set crescendoText = "decr."
%dim = \set crescendoText = "dim."
