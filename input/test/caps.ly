\version "1.3.146"
\header {
texidoc =  " Small caps is available as font-shape caps.

	" }
shapeSC   = \property Lyrics.LyricText \override #'font-shape = #'caps
 shapeNorm = \property Lyrics.LyricText \revert   #'font-shape

\score { <
 \notes \relative c'' { c4 c4 c8 c8 }
\lyrics \context Lyrics { 
  what4 is4 \shapeSC Bud8 -- dha?8 }>
\paper { linewidth = -1.}
  }
