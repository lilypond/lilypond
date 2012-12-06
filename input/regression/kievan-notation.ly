\version "2.17.4"

\header {
  texidoc = "LilyPond typesets Kievan notation.
"
}

\score {
  <<
    \new KievanVoice = "melody" \transpose c c' {
      \cadenzaOn
        c4 c8 c8[ d8] c4 c2 b,\longa
	\bar "k"
    }
    \new Lyrics \lyricsto "melody" {
      Го -- спо -- ди по -- ми -- луй.
    }
  >>
}
