\version "2.19.21"

\header {
  texidoc = "LilyPond typesets Kievan notation.
"
}

% Font settings for Cyrillic
% Linux Libertine fonts contain Cyrillic glyphs.
\paper {
  #(define fonts
    (set-global-fonts
     #:roman "Linux Libertine O,serif"
     #:sans "Linux Biolinum O,sans-serif"
     #:typewriter "Linux Libertine Mono O,monospace"
   ))
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
