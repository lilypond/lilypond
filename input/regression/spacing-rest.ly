\version "1.5.68"
\header {
    texidoc = "Rests get a little less space, since they are narrower.
However, the feta quarter rest is relatively wide, causing this effect to be
very small.
"
    }
\score { \notes \relative c'' \context Staff {
    \time 12/4
    r4 c4 c4 c4 r4 r4 c4
}
\paper {
%stafflinethickness = 0.0
linewidth = -1
    }
     }
