\version "2.3.1"


startGraceMusic = \notes {
    \context Voice \applycontext #set-start-grace-properties
}

stopGraceMusic = \notes { 
    \context Voice \applycontext #set-stop-grace-properties
}

startAppoggiaturaMusic =
\notes {
    \context Voice \applycontext #set-start-grace-properties
    s1*0(
}

stopAppoggiaturaMusic = \notes { 
    \context Voice \applycontext #set-stop-grace-properties
    s1*0)
}

startAcciaccaturaMusic = \notes {
    \context Voice \applycontext #set-start-grace-properties
    s1*0(
    \override Stem  #'stroke-style = #"grace"
}

stopAcciaccaturaMusic = \notes {
    \revert Stem #'stroke-style
    \context Voice \applycontext #set-stop-grace-properties
    s1*0)
}
