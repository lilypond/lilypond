
\version "2.1.22"
\header{
  texidoc="
  The multiple stanzas of lyric phrasing are aligned according to the start 
  and end of a phrase.

  By default, lyrics are centered with respect to the corresponding notes.

@example
           |        |        |     |      |
          x|       x|       x|    x|     x|

     1:  Start  sentence  melisma      end.
     2:  x         x         x______      x
@end example

  While there is a melisma, lyrics are followed by '__' and they
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

