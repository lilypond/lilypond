\header
{

  texidoc = "Lyrics are ignored for aftergrace notes."

}

\version "2.11.51"
\paper {
  ragged-right = ##t
}

<<
  \new Staff {
    \new Voice = "myVoice" {
      \afterGrace c''2 { e''32( } c''2)
    }}
  \new Lyrics \lyricsto "myVoice" \lyricmode { foo -- bar }
>>
