\version "2.19.2"
\header
{

  texidoc = "Lyric extenders run to the end of the line if it
continues the next line. Otherwise, it should run to the last note
of the melisma."

}

\layout {
  ragged-right = ##t
}

<< 
  \new Voice =A  {
    a1 ( a1 \break
    a) a2( b) \break
    a2 
  }
  \context Lyrics \lyricsto A { a __ a __ ha }
>>

