\version "2.11.51"
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
  \lyricsto A \context Lyrics \lyricmode { a __ a __ ha }
>>

