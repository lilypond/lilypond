\version "2.11.51"
\header {

  texidoc ="Polyphonic rhythms and rests do not disturb
@code{\lyricsto}."

}

\layout { ragged-right = ##t}

{
  \new ChoirStaff <<
    \context Lyrics = sop { s1 }
    \new Staff {
      \clef violin
      \time 8/8
      \key des \major
      <<
        \new Voice = "one" {
        \voiceOne
        bes'4 bes'4
        bes'4 bes'4
      }
      \lyricsto "one"  \context Lyrics = sop {
        Do mi nus ex
      }
      \new Voice = "two" {
        \voiceTwo
        ees'8 r8 r8 r8 ees' r8 r8 r8 
      }
      \lyricsto "two"  \new Lyrics  {
        Do na
      }
    >>
  }
  >>
}



