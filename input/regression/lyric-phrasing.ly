\version "1.3.146"
\header{
  texidoc="Lyric phrasing

  We find start and end of phrases, and align lyrics of multiple stanzas
  accordingly.

  Also, lyrics at start of melismata should be left aligned.
  (is that only lyrics that are followed by `__'?  Because
  that seems to be the case now -- jcn)

@example
	   |        |        |     |      |
	  x|       x|       x|    x|     x|

     1:  Start  sentence  melisma      end.
     2:  x         x         x______      x
@end example

  Only lyrics that are followed by '__' while there's a melisma,
  are left-aligned, in this case the third x."
}

\paper { linewidth = -1. }
\score {
  \addlyrics
    \context Voice = "v" \notes  \relative c'' {
      \property Staff.automaticMelismata = ##t
      \autoBeamOff
      a a a8()a a4
    }
    \context Lyrics <
      \context LyricsVoice = "v-1" \lyrics {
        \property LyricsVoice . stanza = "1:"
        Start sentence melisma end.
      }
      \context LyricsVoice = "v-2" \lyrics {
        \property LyricsVoice . stanza = "2:"
        x x x __ x
      }
   >
}
