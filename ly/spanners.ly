
cr = \spanrequest \start "crescendo"
decr = \spanrequest  \start "decrescendo"
rc = \spanrequest   \stop "crescendo"
rced = \spanrequest \stop "decrescendo"

cresc = \notes {
  \commandspanrequest \start "crescendo" ;
  \property Voice.crescendoText = "cresc."
  \property Voice.crescendoSpanner = "dashed-line"
}

endcresc = \notes {
\commandspanrequest \stop "crescendo"; 
\property Voice.crescendoText = ##f
\property Voice.crescendoSpanner = ##f
}

%{

cresc = \spanrequest \start "crescendo"
endcresc = \spanrequest \stop "crescendo"

%}

% better name sustainstart/stop? 
sustainDown = \spanrequest \start "Sustain"
sustainUp = \spanrequest \stop "Sustain"

unaChorda = \spanrequest \start "UnaChorda"
treChorde = \spanrequest \stop "UnaChorda"

sostenutoDown = \spanrequest \start "Sostenuto"
sostenutoUp = \spanrequest \stop "Sostenuto"

%crescpoco = \property Voice.crescendoText = "cresc. poco a poco"
%decresc = \property Voice.crescendoText = "decr."
%dim = \property Voice.crescendoText = "dim."
