\version "2.25.32"

divisions = {
  s4 % submeasure bar here
  s4 % measure bar here
  s2 % start-repeat bar here
  \repeat volta 2 s2 % double repeat bar here
  \repeat volta 2 s2 % end-repeat bar here
  s2 \caesura
  s2 \caesura \shortfermata
  s2 \caesura \fermata
  s2 \section
  s2 \fine
}

labels = {
  \override Score.TextMark.self-alignment-X = #CENTER
  \skip 4 \textMark \markup \rotate #90 \tiny "submeasure"
  \skip 4 \textMark \markup \rotate #90 \tiny "measure"
  \skip 2
  \repeat volta 2 \skip 2
  \repeat volta 2 \skip 2
  \skip 2 % \caesura
  \tweak text "caesura" \startMeasureSpanner
  \skip 2 % \caesura \shortfermata
  \skip 2 % \caesura \fermata
  \stopMeasureSpanner
  \skip 2 \textMark \markup \rotate #90 \tiny "section"
  \skip 2 \textMark \markup \rotate #90 \tiny "fine"
}

music = \fixed c' {
  %% To demo the submeasure bar line, introduce subdivision into 2/4 time, but
  %% enable printing the bar for one measure only.
  \overrideTimeSignatureSettings 2/4 #1/4 #'((1) (1)) #'()
  \time 2/4
  \submeasureBarsOn
  c4 c4 % room for submeasure bar line
  \submeasureBarsOff
  d2
  \repeat volta 2 e2
  \repeat volta 2 f2
  g2
  f2
  e2
  d2
  c2
}
