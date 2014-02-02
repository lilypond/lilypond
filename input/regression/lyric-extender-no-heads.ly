\header
{
  texidoc = "Extender engraver also notices the lack of note heads.
Here the extender ends on the 2nd quarter note, despite the grace note
without a lyric attached." 
  
}

\version "2.19.2"

\layout {
  ragged-right = ##t
}

\relative c'' {
  \time 3/4
  d4~ 4 r4
  \grace es8

  d4
  
}
\addlyrics { x __ x  }

