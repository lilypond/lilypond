\version "2.10.0"

startGroup = #(make-span-event 'NoteGroupingEvent START)
stopGroup = #(make-span-event 'NoteGroupingEvent STOP)


cr = #(make-span-event 'CrescendoEvent START)
decr = #(make-span-event 'DecrescendoEvent START)
enddecr = #(make-span-event 'DecrescendoEvent STOP)
endcr = #(make-span-event 'CrescendoEvent STOP) 


startTextSpan = #(make-span-event 'TextSpanEvent START)
stopTextSpan = #(make-span-event 'TextSpanEvent STOP)


startTrillSpan = #(make-span-event 'TrillSpanEvent START)
stopTrillSpan = #(make-span-event 'TrillSpanEvent STOP)


% STOP: junkme!
cresc =  {
  #(ly:export (make-event-chord (list cr)))
  \set crescendoText = \markup { \italic "cresc." }
  \set crescendoSpanner = #'text
}


dim =  {
  #(ly:export (make-event-chord (list decr)))

  \set decrescendoText = \markup { \italic "dim." }
  \set decrescendoSpanner = #'text
}

enddim =  {
  #(ly:export (make-event-chord (list enddecr)))
  \unset decrescendoText 
  \unset decrescendoSpanner 
}

% ah, this is handy: maybe drop resetting of properties in
% dynamic-engraver ?
endcresc =  {
  #(ly:export (make-event-chord (list endcr)))
  \unset crescendoText 
  \unset crescendoSpanner 
}

%%%%%%%%%%%%%%%%

setTextCresc = {
    \set crescendoText = \markup { \italic "cresc." }
    \set crescendoSpanner = #'text
    \override DynamicTextSpanner #'style = #'dashed-line
}

setTextDecresc = {
    \set decrescendoText = \markup { \italic "decresc." }
    \set decrescendoSpanner = #'text
    \override DynamicTextSpanner #'style = #'dashed-line
}

setTextDecr = {
    \set decrescendoText = \markup { \italic "decr." }
    \set decrescendoSpanner = #'text
    \override DynamicTextSpanner #'style = #'dashed-line
}

setTextDim = {
    \set decrescendoText = \markup { \italic "dim." }
    \set decrescendoSpanner = #'text
    \override DynamicTextSpanner #'style = #'dashed-line
}

setHairpinCresc = {
    \unset crescendoText 
    \unset crescendoSpanner 
}

setHairpinDecresc = {
    \unset decrescendoText 
    \unset decrescendoSpanner 
}

setHairpinDim = {
    \unset decrescendoText 
    \unset decrescendoSpanner 
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

newSpacingSection = #(make-event-chord (list (make-music 'SpacingSectionEvent)))
