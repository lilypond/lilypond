\header {

    texidoc = "Slurs avoid scripts with @code{inside-slur} set
    true, while scripts avoid slurs if @code{inside-slur} set to
    false."

}


\version "2.3.22"

\layout { raggedright = ##t }

\relative c''{
    \clef alto
    \slurUp
    \override Script #'inside-slur = ##t 
    c4(^\downbow b) 
    \override Script #'inside-slur = ##f 
    c4(^\downbow b) 
    c4^\downbow b 
}
