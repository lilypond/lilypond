

% 1st arg dynamic (louder/softer). 2nd arg spantype
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
sustaindown = \spanrequest \start "Sustain"
sustainup = \spanrequest \stop "Sustain"

unachorda = \spanrequest \start "UnaChorda"
trechorde = \spanrequest \stop "UnaChorda"

sostenutodown = \spanrequest \start "Sostenuto"
sostenutoup = \spanrequest \stop "Sostenuto"

%crescpoco = \property Voice.crescendoText = "cresc. poco a poco"
%decresc = \property Voice.crescendoText = "decr."
%dim = \property Voice.crescendoText = "dim."
