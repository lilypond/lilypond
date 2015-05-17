
\header {

  texidoc = "If @code{extendersOverRests} is set, an extender
is not terminated upon encountering a rest. "

}

\paper {
  ragged-right = ##T
}

\version "2.19.21"

<<
  \new Voice = "one" \relative {
    c''4\melisma 
    c4 r c\melismaEnd c
  }
  \new Lyrics \lyricsto "one" {
    \set extendersOverRests = ##t 

    Test __ end
  }
>>
