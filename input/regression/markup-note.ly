\header {

    texidoc = "The note markup function is used to make metronome
 markings. It works for a variety of flag dot and duration settings."
}
\version "2.1.7"

\score { \notes { c4^\markup {
    \note #0 #0 #1
    \note #1 #0 #1
    \note #2 #0 #1
    \note #3 #0 #1
    \note #4 #0 #1
    \note #5 #0 #1
    \note #6 #0 #1

    \note #0 #0 #-1
    \note #1 #0 #-1
    \note #2 #0 #-1
    \note #3 #0 #-1
    \note #4 #0 #-1
    \note #5 #0 #-1
    \note #6 #0 #-1

    \note #0 #1 #-1
    \note #1 #1 #-1
    \note #2 #1 #-1
    \note #3 #1 #-1
    \note #4 #1 #-1
    \note #5 #1 #-1
    \note #6 #1 #-1

    \note #0 #1 #1
    \note #1 #1 #1
    \note #2 #1 #1
    \note #3 #1 #1
    \note #4 #1 #1
    \note #5 #1 #1
    \note #6 #1 #1

}

} }
