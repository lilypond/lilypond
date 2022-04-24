\version "2.16.0"

% this chart is used in the manual too.

\header {
  texidoc ="@cindex Percussion notes

This chart shows all percussion and drum notes."
}

myBreak = { \bar " " \break }

\score {
  \new DrumStaff \with {
    \remove Time_signature_engraver
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
      \cadenzaOn

      bda1^\markup { \center-align "acousticbassdrum: bda" }
      bd  _\markup { \center-align "bassdrum: bd" }
      sn  ^\markup { \center-align "snare: sn" }
      sne _\markup { \center-align "electricsnare: sne" }
      sna ^\markup { \center-align "acousticsnare: sna" } \myBreak

      tomfl^\markup { \center-align "lowfloortom: tomfl" }
      tomfh_\markup { \center-align "highfloortom: tomfh" }
      toml ^\markup { \center-align "lowtom: toml" }
      tomh _\markup { \center-align "hightom: tomh" }
      tomml^\markup { \center-align "lowmidtom: tomml" }
      tommh_\markup { \center-align "himidtom: tommh" } \myBreak

      hhc ^\markup { \center-align "closedhihat: hhc" }
      hh  _\markup { \center-align "hihat: hh" }
      hhp ^\markup { \center-align "pedalhihat: hhp" }
      hho _\markup { \center-align "openhihat: hho" }
      hhho^\markup { \center-align "halfopenhihat: hhho" } \myBreak

      cymca^\markup { \center-align "crashcymbala: cymca" }
      cymc _\markup { \center-align "crashcymbal: cymc" }
      cymra^\markup { \center-align "ridecymbala: cymra" }
      cymr _\markup { \center-align "ridecymbal: cymr" } \myBreak

      cymch^\markup { \center-align "chinesecymbal: cymch" }
      cyms _\markup { \center-align "splashcymbal: cyms" }
      cymcb^\markup { \center-align "crashcymbalb: cymcb" }
      cymrb_\markup { \center-align "ridecymbalb: cymrb" }
      rb   ^\markup { \center-align "ridebell: rb" }
      cb   _\markup { \center-align "cowbell: cb" } \myBreak

      bohm^\markup { \center-align "mutehibongo: bohm" }
      boh _\markup { \center-align "hibongo: boh" }
      boho^\markup { \center-align "openhibongo: boho" }
      bolm_\markup { \center-align "mutelobongo: bolm" }
      bol ^\markup { \center-align "lobongo: bol" }
      bolo_\markup { \center-align "openlobongo: bolo" } \myBreak

      cghm^\markup { \center-align "mutehiconga: cghm" }
      cglm_\markup { \center-align "muteloconga: cglm" }
      cgho^\markup { \center-align "openhiconga: cgho" }
      cgh _\markup { \center-align "hiconga: cgh" }
      cglo^\markup { \center-align "openloconga: cglo" }
      cgl _\markup { \center-align "loconga: cgl" } \myBreak

      timh^\markup { \center-align "hitimbale: timh" }
      timl_\markup { \center-align "lotimbale: timl" }
      agh ^\markup { \center-align "hiagogo: agh" }
      agl _\markup { \center-align "loagogo: agl" } \myBreak

      ssh^\markup { \center-align "hisidestick: ssh" }
      ss _\markup { \center-align "sidestick: ss" }
      ssl^\markup { \center-align "losidestick: ssl" } \myBreak

      guis^\markup { \center-align "shortguiro: guis" }
      guil_\markup { \center-align "longguiro: guil" }
      gui ^\markup { \center-align "guiro: gui" }
      cab _\markup { \center-align "cabasa: cab" }
      mar ^\markup { \center-align "maracas: mar" } \myBreak

      whs^\markup { \center-align "shortwhistle: whs" }
      whl_\markup { \center-align "longwhistle: whl" } \myBreak

      hc  ^\markup { \center-align "handclap: hc" }
      tamb_\markup { \center-align "tambourine: tamb" }
      vibs^\markup { \center-align "vibraslap: vibs" }
      tt  _\markup { \center-align "tamtam: tt"  } \myBreak

      cl ^\markup { \center-align "claves: cl" }
      wbh_\markup { \center-align "hiwoodblock: wbh" }
      wbl^\markup { \center-align "lowoodblock: wbl" } \myBreak

      cuim^\markup { \center-align "mutecuica: cuim" }
      cuio_\markup { \center-align "opencuica: cuio" }
      trim^\markup { \center-align "mutetriangle: trim" }
      tri _\markup { \center-align "triangle: tri" }
      trio^\markup { \center-align "opentriangle: trio" } \myBreak
    }
  }

  \layout {
    \context {
      \Score
      \remove Bar_number_engraver
    }
  }
}
