\version "1.5.68"


#(define (make-span-request type spandir)
  (let* ((m (make-music-by-name  type)))
  (ly-set-mus-property! m 'span-direction spandir)
  m
  ))

groupOpen = #(make-span-request 'NoteGroupingEvent START)
groupClose = #(make-span-request 'NoteGroupingEvent STOP)


cr = #(make-span-request 'CrescendoEvent START)
rc = #(make-span-request 'CrescendoEvent STOP)
decr = #(make-span-request 'DecrescendoEvent START)
rced = #(make-span-request 'DecrescendoEvent STOP)

cresc = \notes {
  \commandspanrequest \start "crescendo" 
  \property Voice.crescendoText = #'((font-shape . italic) "cresc.")
  \property Voice.crescendoSpanner = #'dashed-line
}

% ah, this is handy: maybe drop resetting of properties in
% dynamic-engraver ?
endcresc = \notes {
  \commandspanrequest \stop "crescendo" 
  \property Voice.crescendoText \unset
  \property Voice.crescendoSpanner \unset
}

dim = \notes {
  \commandspanrequest \start "decrescendo" 
  \property Voice.decrescendoText = #"dim."
  \property Voice.decrescendoSpanner = #'dashed-line
}

enddim = \notes {
  \commandspanrequest \stop "decrescendo" 
   \property Voice.decrescendoText \unset
 \property Voice.decrescendoSpanner \unset
}

%{

cresc = \spanrequest \start "crescendo"
endcresc = \spanrequest \stop "crescendo"

%}

% better name sustainstart/stop? 
sustainDown = \spanrequest \start "Sustain"
sustainUp = \spanrequest \stop "Sustain"

unaCorda = \spanrequest \start "UnaCorda"
treCorde = \spanrequest \stop "UnaCorda"

sostenutoDown = \spanrequest \start "Sostenuto"
sostenutoUp = \spanrequest \stop "Sostenuto"

%crescpoco = \property Voice.crescendoText = "cresc. poco a poco"
%decresc = \property Voice.crescendoText = "decr."
%dim = \property Voice.crescendoText = "dim."
