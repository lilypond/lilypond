\version "2.1.7"
\header {

texidoc ="Polyphonic rhythms and rests don't disturb
@code{\newaddlyrics}."

}

\score {
    \notes {
       \clef violin
       \time 8/8
       \key des \major
       <<
	   \context Voice = one {
	       \voiceOne
	       bes'4 bes'4
	       bes'4 bes'4
	   }
	  \context Voice = two {
	      \voiceTwo
	      ees'8 r8 r8 r8 ees' r8 r8 r8 
          }
          \newaddlyrics "two" \lyrics \new LyricsVoice {
             Do na
         }
	 \lyrics  \newaddlyrics "one" \new LyricsVoice
	   {
	       Do mi nus ex
	   }
       >>
    }
    \paper { raggedright = ##t}
}

