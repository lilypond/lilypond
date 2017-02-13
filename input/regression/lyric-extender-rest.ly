
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
    c4 r c\melismaEnd c1
    c4 \melisma
    c4 r c\melismaEnd c1
    c4 \melisma
    c4 r c\melismaEnd c1
  }
  \new Lyrics \lyricsto "one" {
    Test __ "default"
    \set extendersOverRests = ##t 
    test __ \markup \typewriter "#t"
    \set extendersOverRests = ##f
    test __ \markup \typewriter "#f"
  }
>>
