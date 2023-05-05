\version "2.25.5"

divisions = {
  \time 2/4
  s2 % default measure bar here
  s2 % start-repeat bar here
  \repeat volta 2 s2 % double repeat bar here
  \repeat volta 2 s2 % end-repeat bar here
  s2 \caesura
  s2 \virgula
  s2 \divisioMinima
  s2 \divisioMaior
  s2 \divisioMaxima
  s2 \finalis
}

labels = {
  \override Score.TextMark.self-alignment-X = #CENTER
  \time 2/4
  \skip 2 \textMark \markup \rotate #90 \tiny "measure"
  \skip 2
  \repeat volta 2 \skip 2
  \repeat volta 2 \skip 2
  \skip 2 \textMark \markup \rotate #90 \tiny "caesura"
  \skip 2 \textMark \markup \rotate #90 \tiny "virgula"
  \skip 2 % \divisioMinima
  \tweak text "divisio" \startMeasureSpanner
  \skip 2 % \divisioMaior
  \skip 2 % \divisioMaxima
  \stopMeasureSpanner
  \skip 2 \textMark \markup \rotate #90 \tiny "finalis"
}

music = \fixed c' {
  \time 2/4
  c2
  d2
  \repeat volta 2 e2
  \repeat volta 2 f2
  g2
  f2
  e2
  d2
  c2
  b,2
}
