\header {

  texidoc = "The minimum distance between lyrics are determined by the
@code{minimum-distance} of @code{LyricHyphen} and
@code{LyricSpace}. The hyphen is omitted when it would be shorter than
its @code{minimum-length} setting."

}

\version "2.7.22"

\layout {
  raggedright = ##t
}


\relative c'' {
  \time 2/4
  c32 c c c 
  c32 c c c 
  c32 c c c 
  c32 c c c 
  c32 c c c 
}
\addlyrics
{
   syl -- lab word
   \once \override LyricHyphen #'minimum-distance = #0.0
   syl -- lab word
   \override LyricSpace #'minimum-distance = #0.0
   syl -- lab word
}
