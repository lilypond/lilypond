\version "1.5.68"


\score
{
\context Voice \notes \relative c'' {
\time 2/4
\times 2/3 { c8 c c c c c  }
\property Voice . tupletSpannerDuration = #(make-moment 1 4)
\times 2/3 { c8 c c c c c  }

}

}
