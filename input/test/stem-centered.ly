\version "1.5.68"
\header {
    texidoc ="mensural note heads."
    }

\score {
\notes {
\relative c'' {
\property Voice . NoteHead \set #'style = #'mensural
c\maxima*1/8
c\longa*1/4 c\breve*1/2 c1 c2 c4 c8 
}
}
}
