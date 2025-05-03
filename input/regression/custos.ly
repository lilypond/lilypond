\version "2.25.27"
\header {
    texidoc = "Custodes may be engraved in various styles."
}

\layout {
  \context {
    \Staff
    \consists "Custos_engraver"
    \omit BarLine
  }
  ragged-right = ##t
}

{
  \override Staff.Custos.neutral-position = #4

  \override Staff.Custos.style = #'hufnagel
  c'1^"hufnagel"
  \break < a d' a' f'' a''>1

  \override Staff.Custos.style = #'medicaea
  c'1^"medicaea"
  \break < a d' a' f'' a''>1

  \override Staff.Custos.style = #'vaticana
  c'1^"vaticana"
  \break < a d' a' f'' a''>1

  \override Staff.Custos.style = #'mensural
  c'1^"mensural"
  \break < a d' a' f'' a''>1
}


\score {
  \new VaticanaScore
    <<
      \new VaticanaVoice = "kyrie" {
        \clef "vaticana-fa2"
        d
        \[ d \melisma \flexa c \melismaEnd \]
        \[ c \melisma \pes d  d \flexa a, \melismaEnd \]
        \break
        \[ g, \melisma \pes a, c c \flexa a, \melismaEnd \] c d \augmentum d
      }
      \new VaticanaLyrics \lyricsto "kyrie" {
        Ký -- ri -- e e -- lé -- i -- son
      }
    >>
  \layout {
    ragged-right = ##t
    indent = 0
    \context {
      \VaticanaLyrics
      \override LyricHyphen.minimum-distance = 1
    }
  }
}
