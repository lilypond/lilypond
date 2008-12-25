\version "2.12.0"

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
  \once \set crescendoText = \markup { \italic "cresc." }
  \once \set crescendoSpanner = #'text
}


dim =  {
  #(ly:export (make-event-chord (list decr)))
  \once \set decrescendoText = \markup { \italic "dim." }
  \once \set decrescendoSpanner = #'text
}

enddim =  {
  #(ly:export (make-event-chord (list enddecr)))
%  \unset decrescendoText 
%  \unset decrescendoSpanner 
}

endcresc =  {
  #(ly:export (make-event-chord (list endcr)))
%  \unset crescendoText 
%  \unset crescendoSpanner 
}

%%%%%%%%%%%%%%%%

crescTextCresc = {
    \set crescendoText = \markup { \italic "cresc." }
    \set crescendoSpanner = #'text
    \override DynamicTextSpanner #'style = #'dashed-line
}

dimTextDecresc = {
    \set decrescendoText = \markup { \italic "decresc." }
    \set decrescendoSpanner = #'text
    \override DynamicTextSpanner #'style = #'dashed-line
}

dimTextDecr = {
    \set decrescendoText = \markup { \italic "decr." }
    \set decrescendoSpanner = #'text
    \override DynamicTextSpanner #'style = #'dashed-line
}

dimTextDim = {
    \set decrescendoText = \markup { \italic "dim." }
    \set decrescendoSpanner = #'text
    \override DynamicTextSpanner #'style = #'dashed-line
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
