\version "2.16.0"

startGraceSlur = #(make-music 'SlurEvent 'span-direction START 'spanner-id "grace")
stopGraceSlur = #(make-music 'SlurEvent 'span-direction STOP 'spanner-id "grace")


startGraceMusic =  {
}

stopGraceMusic =  {
}

startAppoggiaturaMusic =
{
    <>\startGraceSlur
}

stopAppoggiaturaMusic =  {
    <>\stopGraceSlur
}

startAcciaccaturaMusic =  {
    <>\startGraceSlur
    \override Flag  #'stroke-style = #"grace"
}

stopAcciaccaturaMusic =  {
    \revert Flag #'stroke-style
    <>\stopGraceSlur
}

startSlashedGraceMusic =  {
  \override Flag #'stroke-style = #"grace"
}

stopSlashedGraceMusic =  {
  \revert Flag #'stroke-style
}
