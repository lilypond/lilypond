\version "1.5.68"
\header{
  texidoc="Simple beams on middle line should be allowed to have a slope."
}
\score{
    \notes\relative c'{
	[b8 c] [c b]
	[b''8 a] [a b]
    }
    \paper{
	linewidth = 0.0
    }
}