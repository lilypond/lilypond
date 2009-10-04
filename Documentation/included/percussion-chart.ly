\version "2.12.0"

% yes, I know this is a mess.  But I'm not going to fuss with
% it one day before I leave.  -gp

% this chart is used in the manual too.

\header {
    texidoc ="@cindex Percussion notes

This chart shows all percussion and drum notes."
}

myBreak = { \bar " " \break }

\new Score \with {
  \remove "Bar_number_engraver"
} \new DrumStaff \with {
  \remove "Time_signature_engraver"
} \context DrumVoice {

%% this stuff set up nice || bar lines to divide percussion notes
%% into related fields, but it should be placed in the actual
%% music, not as a separate voice.  -gp
%{
barlines = {  
\repeat "unfold" 4 {s 1 \bar" " } s 1 \bar "||" 
\repeat "unfold" 2 { s 1 \bar" "  s 1 \bar" " s 1 \bar "||" }
\repeat "unfold" 4 {s 1 \bar" " } s 1 \bar "||" 
\repeat "unfold" 7  {s 1 \bar" " }  s 1 \bar "||" 
s 1 \bar" "  s 1 \bar "||" 
\repeat "unfold" 2 { \repeat "unfold" 5 {s 1 \bar" " } s 1 \bar "||" }		
\repeat "unfold" 2 { s 1 \bar" "  s 1 \bar "||" }
\repeat "unfold" 2 {s 1 \bar" " } s 1 \bar "||" 
\repeat "unfold" 4 {s 1 \bar" " } s 1 \bar "||" 
s 1 \bar" "  s 1 \bar "||" 
\repeat "unfold" 3 {s 1 \bar" " } s 1 \bar "||" 
\repeat "unfold" 2 {s 1 \bar" " } s 1 \bar "||" 
\repeat "unfold" 3 {\repeat "unfold" 4 {s 1 \bar" " } s 1 \bar "||"  }
}
%}

\drummode { 
\textLengthOn
\cadenzaOn
bda1 ^"acousticbassdrum: bda" bd   ^"bassdrum: bd" sn  ^"snare: sn" sne  ^"electricsnare: sne" sna  ^"acousticsnare: sna" \myBreak
tomfl  ^"lowfloortom: tomfl" tomfh  ^"highfloortom: tomfh" toml  ^"lowtom: toml"  tomh  ^"hightom: tomh" 
tomml  ^"lowmidtom: tomml" tommh  ^"himidtom: tommh" \myBreak
hhc  ^"closedhihat: hhc"  hh  ^"hihat: hh" hhp  ^"pedalhihat: hhp" hho  ^"openhihat: hho" hhho  ^"halfopenhihat: hhho" \myBreak
cymca  ^"crashcymbala: cymca" cymc  ^"crashcymbal: cymc" cymra  ^"ridecymbala: cymra" cymr  ^"ridecymbal: cymr"  \myBreak cymch  ^"chinesecymbal: cymch" cyms  ^"splashcymbal: cyms"
cymcb  ^"crashcymbalb: cymcb"  cymrb  ^"ridecymbalb: cymrb" 
rb  ^"ridebell: rb" cb  ^"cowbell: cb" \myBreak
bohm  ^"mutehibongo: bohm" boh  ^"hibongo: boh" boho  ^"openhibongo: boho" bolm  ^"mutelobongo: bolm" bol  ^"lobongo: bol" bolo  ^"openlobongo: bolo"\myBreak
cghm  ^"mutehiconga: cghm" cglm  ^"muteloconga: cglm" cgho  ^"openhiconga: cgho" cgh  ^"hiconga: cgh" cglo  ^"openloconga: cglo" cgl  ^"loconga: cgl" \myBreak
timh  ^"hitimbale: timh" timl  ^"lotimbale: timl"  
agh  ^"hiagogo: agh" agl  ^"loagogo: agl" \myBreak
ssh ^"hisidestick: ssh" ss  ^"sidestick: ss" ssl  ^"losidestick: ssl" \myBreak
guis  ^"shortguiro: guis" guil  ^"longguiro: guil" gui  ^"guiro: gui" cab  ^"cabasa: cab" mar  ^"maracas: mar" \myBreak
whs  ^"shortwhistle: whs" whl  ^"longwhistle: whl"  \myBreak
hc  ^"handclap: hc" tamb  ^"tambourine: tamb" vibs  ^"vibraslap: vibs" tt ^"tamtam: tt"  \myBreak
cl  ^"claves: cl" wbh  ^"hiwoodblock: wbh" wbl  ^"lowoodblock: wbl"  \myBreak
cuim  ^"mutecuica: cuim" cuio  ^"opencuica: cuio"
trim  ^"mutetriangle: trim" tri  ^"triangle: tri" trio  ^"opentriangle: trio"  \myBreak
ua  ^"oneup: ua" ub  ^"twoup: ub" uc  ^"threeup: uc" ud  ^"fourup: ud" ue  ^"fiveup: ue"  \myBreak
da  ^"onedown: da" db  ^"twodown: db" dc  ^"threedown: dc" dd  ^"fourdown: dd" de ^"fivedown: de"  \myBreak
}
}

