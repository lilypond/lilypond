\version "1.5.68"
\header {
	texidoc = "For juxtaposed chords with the same direction, a
slight optical correction is used. It is constant, and only works if
two chords have no common head-positions range."
}

\score { \notes \relative c'' {
\stemDown
\time 12/4
[f8 e f e] [f d f d] [f c f c] [f b, f' b,] [f' a, f' a,]
}
\paper { linewidth = -1. }
}
