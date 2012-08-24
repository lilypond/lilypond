\version "2.16.0"
\header {
  texidoc = "Oppositely stemmed chords,  meshing into each other,
are resolved."
}

\layout { ragged-right= ##t }


\context Staff  \transpose c c' {
  \time 3/4
  %% Here's what I was trying to do:
  <<
    {
      d'4
      d'4.	 
      <a c'>4
      <g b g'>4
      <g b g'>4.	 	 
    } \\ {
      <g b g'>4
      <g b g'>4.       
      <g b d'>4
      d'4
      d'4.       
    }
  >>
}




