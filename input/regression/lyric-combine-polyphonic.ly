\header {

texidoc ="polyphonic rhythms and rests don't disturb
@code{\addlyrics}."

}

\score {
    \notes {
       \clef violin
       \time 8/8
       \key des \major
       < \context Voice = one {
          \voiceOne
          \addlyrics
          \notes {
             bes'4 bes'4
             bes'4 bes'4
          }
          \context Lyrics \lyrics
          {
             Do __ mi __ nus ex
          }
         }
         \context Voice = two {
          \voiceTwo
          \addlyrics
          \notes {
             ees'8 r8 r8 r8 ees' r8 r8 r8 
          }
          \context Lyrics \lyrics
         {
             Do __ na
          }
         }
       >
    }
    \paper { linewidth=-1.0 }
}
