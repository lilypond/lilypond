\version "2.3.8"


startGraceMusic =  {
    \context Voice \applycontext #set-start-grace-properties
}

stopGraceMusic =  { 
    \context Voice \applycontext #set-stop-grace-properties
}

startAppoggiaturaMusic =
 {
    \context Voice \applycontext #set-start-grace-properties
    s1*0(
}

stopAppoggiaturaMusic =  { 
    \context Voice \applycontext #set-stop-grace-properties
    s1*0)
}

startAcciaccaturaMusic =  {
    \context Voice \applycontext #set-start-grace-properties
    s1*0(
    \override Stem  #'stroke-style = #"grace"
}

stopAcciaccaturaMusic =  {
    \revert Stem #'stroke-style
    \context Voice \applycontext #set-stop-grace-properties
    s1*0)
}
