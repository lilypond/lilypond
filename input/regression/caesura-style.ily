\version "2.23.13"

\layout {
  \context {
    \Staff
    \omit Clef
    \omit TimeSignature
  }
}

staffMusic = \fixed c' {
  \cadenzaOn

  \sectionLabel \markup \tiny "No Art."
  f2
  \override Score.CaesuraScript.direction = #DOWN
  \caesura
  f
  \caesura \bar "|"

  \sectionLabel \markup \tiny "Art. ↓"
  f
  \caesura_\testArticulation
  f
  \caesura_\testArticulation \bar "|"

  \sectionLabel \markup \tiny "Art. ↑"
  f
  \caesura^\testArticulation
  f
  \caesura^\testArticulation \bar "|"

  \sectionLabel \markup \tiny "Art. Neutral"
  f
  \caesura \testArticulation
  f
  \caesura \testArticulation \bar "|"
}

music = <<
  \new Staff \with {
    instrumentName = \markup \tiny \column { "B.Sign ↑" "C.Script ↑" }
    \override BreathingSign.direction = #UP
    \override CaesuraScript.direction = #UP
  } \staffMusic

  \new Staff \with {
    instrumentName = \markup \tiny \column { "B.Sign ↓" "C.Script ↑" }
    \override BreathingSign.direction = #DOWN
    \override CaesuraScript.direction = #UP
  } \staffMusic

  \new Staff \with {
    instrumentName = \markup \tiny \column { "B.Sign ↑" "C.Script ↓" }
    \override BreathingSign.direction = #UP
    \override CaesuraScript.direction = #DOWN
  } \staffMusic

  \new Staff \with {
    instrumentName = \markup \tiny \column { "B.Sign ↓" "C.Script ↓" }
    \override BreathingSign.direction = #DOWN
    \override CaesuraScript.direction = #DOWN
  } \staffMusic
>>
