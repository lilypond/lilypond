
cr = \spanrequest \start "crescendo"
decr = \spanrequest  \start "decrescendo"
rc = \spanrequest   \stop "crescendo"
rced = \spanrequest \stop "decrescendo"

cresc = \notes {
  \commandspanrequest \start "crescendo" ;
  \property Voice.crescendoText = "cresc."
  \property Voice.crescendoSpanner = "dashed-line"
}

% ah, this is handy: maybe drop resetting of properties in
% dynamic-engraver ?
endcresc = \notes {
  \commandspanrequest \stop "crescendo"; 
  \property Voice.crescendoText = ##f
  \property Voice.crescendoSpanner = ##f
}

dim = \notes {
  \commandspanrequest \start "decrescendo" ;
  \property Voice.decrescendoText = "dim."
  \property Voice.decrescendoSpanner = "dashed-line"
}

enddim = \notes {
  \commandspanrequest \stop "decrescendo"; 
  \property Voice.decrescendoText = ##f
  \property Voice.decrescendoSpanner = ##f
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
