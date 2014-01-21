\header
{

  texidoc = "Lyrics are ignored for aftergrace notes."

}

\version "2.19.2"
\paper {
  ragged-right = ##t
}

<<
  \new Staff {
    \new Voice = "myVoice" {
      \afterGrace c''2 { e''32( } c''2)
    }}
  \new Lyrics \lyricsto "myVoice" { foo -- bar }
>>
