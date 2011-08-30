\version "2.14.0"

startGraceSlur = #(make-music 'SlurEvent 'span-direction START 'spanner-id "grace")
stopGraceSlur = #(make-music 'SlurEvent 'span-direction STOP 'spanner-id "grace")


startGraceMusic =  {
}

stopGraceMusic =  { 
}

startAppoggiaturaMusic =
 {
    s1*0\startGraceSlur
}

stopAppoggiaturaMusic =  { 
    s1*0\stopGraceSlur
}

startAcciaccaturaMusic =  {
    s1*0\startGraceSlur
    \override Flag  #'stroke-style = #"grace"
}

stopAcciaccaturaMusic =  {
    \revert Flag #'stroke-style
    s1*0\stopGraceSlur
}

startSlashedGraceMusic =  {
  \override Flag #'stroke-style = #"grace"
}

stopSlashedGraceMusic =  {
  \revert Flag #'stroke-style
}
