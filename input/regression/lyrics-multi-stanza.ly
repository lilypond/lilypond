\header {

texidoc = "Lyrics syllables are aligned according to
  punctuation. Stanza and stz set stanza numbers. "

}

	
\score {
\addlyrics
  \notes \relative c'' \context Voice = duetto { \time 3/4 g2 e4 a2 f4 g2.  }
  \lyrics \context Lyrics <
  \context LyricsVoice = "duet-1" {
    \property LyricsVoice . stanza = "Bert"
    Hi, my name is bert.    }
  \context LyricsVoice = "duet-2" {
    \property LyricsVoice . stanza = "Ernie" 
    Ooooo, ch\'e -- ri, je t'aime. }
  >
  \paper { linewidth = -1.0}
}

