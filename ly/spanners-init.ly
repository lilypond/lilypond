\version "1.7.2"


#(define (make-span-event type spandir)
  (let* ((m (make-music-by-name  type)))
  (ly-set-mus-property! m 'span-direction spandir)
  m
  ))

groupOpen = #(make-span-event 'NoteGroupingEvent START)
groupClose = #(make-span-event 'NoteGroupingEvent STOP)


cr = #(make-span-event 'CrescendoEvent START)
rc = #(make-span-event 'CrescendoEvent STOP)
decr = #(make-span-event 'DecrescendoEvent START)
rced = #(make-span-event 'DecrescendoEvent STOP)

startTextSpan = #(make-span-event 'TextSpanEvent START)
stopTextSpan = #(make-span-event 'TextSpanEvent STOP)

cresc = \notes {
  #(ly-export (make-event-chord (list cr)))
  \property Voice.crescendoText = #'((font-shape . italic) "cresc.")
  \property Voice.crescendoSpanner = #'dashed-line
}

% ah, this is handy: maybe drop resetting of properties in
% dynamic-engraver ?
endcresc = \notes {
  #(ly-export (make-event-chord (list rc)))
  \property Voice.crescendoText \unset
  \property Voice.crescendoSpanner \unset
}

dim = \notes {
  #(ly-export (make-event-chord (list decr)))

  \property Voice.decrescendoText = #"dim."
  \property Voice.decrescendoSpanner = #'dashed-line
}

enddim = \notes {
  #(ly-export (make-event-chord (list rced)))
   \property Voice.decrescendoText \unset
 \property Voice.decrescendoSpanner \unset
}

%{

cresc = \spanevent \start "crescendo"
endcresc = \spanevent \stop "crescendo"

%}

% better name sustainstart/stop? 
sustainUp = #(make-span-event 'SustainPedalEvent STOP)

unaCorda = #(make-span-event 'UnaCordaEvent START)
treCorde = #(make-span-event 'UnaCordaEvent STOP)

sostenutoDown = #(make-span-event 'SostenutoEvent START)
sostenutoUp = #(make-span-event 'SostenutoEvent STOP)

%crescpoco = \property Voice.crescendoText = "cresc. poco a poco"
%decresc = \property Voice.crescendoText = "decr."
%dim = \property Voice.crescendoText = "dim."
