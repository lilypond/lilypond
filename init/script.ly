
% name = \script {
% indexstring follow_into_staff same_dir_as_stem above_below_staff symbol_can_beinverted priority }
%

accent = \script { "sforzato"  0 -1 0 0 0 }
marcato = \script { "marcato" 0 -1 0 1  0 }
staccatissimo = \script { "staccatissimo" 0  -1 0 1 0 }
portato= \script { "portato" 0 -1 0 1 0 }
fermata = \script { "fermata" 0 1 0 1 0 }
stopped = \script { "stopped" 0 0 1 0 0 }
staccato = \script { "staccato" 1 -1 0 0 0 }
tenuto = \script {"tenuto" 0 -1 0 0 0 }
upbow = \script { "upbow" 0 0 1 0 0 }
downbow = \script { "downbow" 0 0 1 0 0 }
lheel = \script { "upedalheel" 0 0 -1  0 0 }
rheel = \script { "dpedalheel" 0 0 1 0 0 }
ltoe = \script { "upedaltoe" 0 0 -1 0 0 }
rtoe = \script { "dpedaltoe" 0 0 1 0 0 }
turn = \script { "turn" 0 0 1 0 0 }
open = \script { "open" 0 0 1 0 0 }
flageolet = \script { "flageolet"  0 0 1 0 0 }


% could  we do without this bloat?
%
lbheel = \script { "bheel" 0 0 -1  0 0 }
rbheel = \script { "bheel" 0 0 1 0 0 }
lbtoe = \script { "btoe" 0 0 -1 0 0 }
rbtoe = \script { "btoe" 0 0 1 0 0 }
lfheel = \script { "fheel" 0 0 -1  0 0 }
rfheel = \script { "fheel" 0 0 1 0 0 }
lftoe = \script { "ftoe" 0 0 -1 0 0 }
rftoe = \script { "ftoe" 0 0 1 0 0 }

%
% left toe:      right heel:
%
%                    u     -
%                    -     u
%  ---|-----|--   --|x----|x--
%  ---|-----|--   --|-----|---
%  --x|----x|--   --|-----|---
%    ^     -                   
%    -     ^                   
%  back  front     back  front
%
% 
% heu, me thought for a moment that order in table_xxx.ly was
% being translated into priority...
back = \script { "back" 0 -1 -1  0 0 }
front = \script { "front" 0 -1 1 0 0 }

trill =\script { "trill" 0 0 1 0 
	2000   % be above text. 
}
