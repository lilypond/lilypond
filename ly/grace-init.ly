\version "2.17.6"

startGraceSlur = #(make-music 'SlurEvent 'span-direction START 'spanner-id 'grace)
stopGraceSlur = #(make-music 'SlurEvent 'span-direction STOP 'spanner-id 'grace)


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
    \temporary \override Flag.stroke-style = "grace"
}

stopAcciaccaturaMusic =  {
    \revert Flag.stroke-style
    <>\stopGraceSlur
}

startSlashedGraceMusic =  {
  \temporary \override Flag.stroke-style = "grace"
}

stopSlashedGraceMusic =  {
  \revert Flag.stroke-style
}
