
\version "2.1.22"
\header{
  texidoc="Lyric phrasing

  The multiple stanzas are aligned according to the start and end of a phrase.

  Also, lyrics at the beginning of a melismata should be left aligned.
  (is that only lyrics that are followed by `__'?  Because
  that seems to be the case now -- jcn)

@example
	   |        |        |     |      |
	  x|       x|       x|    x|     x|

     1:  Start  sentence  melisma      end.
     2:  x         x         x______      x
@end example

  Only lyrics that are followed by '__' while there is a melisma,
  are left-aligned, in this case the third x."
}

\paper { raggedright = ##t}
\score {
  \addlyrics
    \context Voice = "v" \notes  \relative c'' {
      \autoBeamOff
      a a a8 ( a) a4
    }
  <<
      \new Lyrics \lyricsto "v"  \lyrics {
        \set stanza = "1:"
        Start sentence melisma end.
      }
      \new Lyrics \lyricsto "v" \lyrics {
        \set stanza = "2:"
        x x x __ x.
      }
   >>
}

