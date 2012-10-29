\header {

    texidoc = "Slurs avoid scripts with @code{avoid-slur} set to
@code{inside}, scripts avoid slurs with @code{avoid-slur} set to
@code{around}.  Slurs and scripts keep a distance of
@code{slur-padding}."

}


\version "2.17.6"

\layout { ragged-right = ##t }

\relative c''{
    \clef alto
    \slurUp
    \override Script.slur-padding = #0.2
    \override Script.avoid-slur = #'inside
    c4(^\downbow b) 
    \override Script.avoid-slur = #'around
    c4(^\downbow b) 
    c4^\downbow b 
}
