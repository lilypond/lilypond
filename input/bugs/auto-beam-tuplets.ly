\score { \notes {
	\property Voice.tupletSpannerDuration = #(make-moment 1 4)
  \property Voice.tupletNumberVisibility = #'if-no-beam
  \property Voice.tupletBracketVisibility = #'if-no-beam
  %  Gm7  /  F    |  A7   /  Dm   | 
  \times 2/3 {
  [f,8 bes, d] [g d bes,] [f, a, c] [f c a,] | e, a, cis e cis a, d, f, a, d a, f, |
  %  Db           |
  des, aes, des f des aes, des, aes, des f des aes, | }
}}
