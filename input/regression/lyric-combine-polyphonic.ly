\version "2.2.0"
\header {

texidoc ="Polyphonic rhythms and rests do not disturb
@code{\lyricsto}."

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
          \lyricsto "two" \new Lyrics \lyrics {
	      Do na
         }
	 \lyricsto "one" \new Lyrics \lyrics {
	     Do mi nus ex
	 }
       >>
    }
    \paper { raggedright = ##t}
}

