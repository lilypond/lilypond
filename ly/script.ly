

#(eval-string (ly-gulp-file "script.scm"))

#default-script-alist

"dash-hat" = "marcato"
"dash-plus" = "stopped"
"dash-dash" = "tenuto"
"dash-bar" = "staccatissimo"
"dash-larger" = "accent"
"dash-dot" = "staccato"


thumb = \script "thumb"
accent = \script "accent"
marcato = \script "marcato"
staccatissimo = \script "staccatissimo"

% portato is indicated
% either by
%   *  slurred & dotted notes. 
%or by
%  * slur and dash notes.
% Neither are  really supported, but c4-.-- should work.
% portato = \script "portato"

fermata = \script "fermata"
stopped = \script "stopped"
staccato = \script "staccato"
tenuto = \script "tenuto"
upbow = \script "upbow"
downbow = \script "downbow"
lheel = \script "lheel"
rheel = \script "rheel"
ltoe = \script "ltoe"
rtoe = \script "rtoe"
turn = \script "turn"
open = \script "open"
flageolet = \script "flageolet"
reverseturn = \script "reverseturn"
trill = \script "trill"
prall = \script "prall"
mordent = \script "mordent"
prallprall = \script "prallprall"
prallmordent = \script "prallmordent"
upprall = \script "upprall"
downprall = \script "downprall"
segno = \script "segno"
coda = \script "coda"
