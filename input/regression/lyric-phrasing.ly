\version "2.19.21"
\header {

  texidoc = "
  Normally, the lyric is centered on the note head. However, on
  melismata, the text is left aligned on the left-side of the note head.

"
}

  \layout { ragged-right = ##t }




<<
  \relative \new Voice = "bla" {
    \autoBeamOff
    c'4( c16 d c b)  c4
    d16[ e f g]
    
  }
  \context Lyrics \lyricsto "bla" {
    alllll __ tijd
    izzz
  }
>>


