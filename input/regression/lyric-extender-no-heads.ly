\header
{
  texidoc = "Extender engraver also notices the lack of note heads.
Here the extender ends on the 2nd quarter note, despite the grace note
without a lyric attached." 
  
}

\version "2.19.21"

\layout {
  ragged-right = ##t
}

\relative {
  \time 3/4
  d''4~ 4 r4
  \grace es8

  d4
  
}
\addlyrics { x __ x  }

