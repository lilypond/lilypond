\version "2.25.5"

\header {
  texidoc = "LilyPond typesets Kievan notation.
"
}

% Font settings for Cyrillic
% Linux Libertine fonts contain Cyrillic glyphs.
\paper {
  fonts.serif = "Linux Libertine O,serif"
  fonts.sans = "Linux Biolinum O,sans-serif"
  fonts.typewriter = "Linux Libertine Mono O,monospace"
}

\score {
  <<
    \new KievanVoice = "melody" \transpose c c' {
      \cadenzaOn
        cis4 c8 \[ c8( d8) \] c4 c2 bes,\longa
	\bar "k"
    }
    \new Lyrics \lyricsto "melody" {
      Го -- спо -- ди по -- ми -- луй.
    }
  >>
}
