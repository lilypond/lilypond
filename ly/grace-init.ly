\version "1.9.1"


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
    \property Voice.Stem \override #'stroke-style = #"grace"
}

stopAcciaccaturaMusic = \notes {
    \property Voice.Stem \revert #'stroke-style
    \context Voice \applycontext #set-stop-grace-properties
    s1*0)
}
