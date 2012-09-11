\header {

  texidoc = "No hyphen should be printed under a grace note at the start
of a line if the grace's main note starts a new syllable."
}

\version "2.16.0"
<<
  \new Staff {
    \appoggiatura f'8 g'2 g'( | \break
    \appoggiatura f'8 g'2) \appoggiatura f'8 g'2 | \break
    \appoggiatura f'8 g'2 g' | \break
    g'2 g' |
  }
  \addlyrics {
    \lyricmode {
      bla -- bla -- bla -- bla -- bla -- bla -- bla
    }
  }
  \new Staff {
    g'2 g' |
    g'2 g' |
    g'2 g' |
    g'2 g' |
  }
  \addlyrics {
    \lyricmode {
      bla -- bla -- bla -- bla -- bla -- bla -- bla -- bla
    }
  }
>>
