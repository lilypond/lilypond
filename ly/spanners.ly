

% 1st arg dynamic (louder/softer). 2nd arg spantype
cr = \spanrequest \start "crescendo"
decr = \spanrequest  \start "decrescendo"
rc = \spanrequest   \stop "crescendo"
rced = \spanrequest \stop "decrescendo"

%% urg, these don't work yet.
%% must set manually
xcresc = {
%\spanrequest \start "crescendo"
\property Voice.crescendoText = "cresc."
\property Voice.crescendoSpanner = "dashed"
}

xendcresc = {
%\spanrequest \start "crescendo"
\property Voice.crescendoText = ##f
\property Voice.crescendoSpanner = ##f
}

cresc = \spanrequest \start "crescendo"
endcresc = \spanrequest \stop "crescendo"

%crescpoco = \property Voice.crescendoText = "cresc. poco a poco"
%decresc = \property Voice.crescendoText = "decr."
%dim = \property Voice.crescendoText = "dim."
