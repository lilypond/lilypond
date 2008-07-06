
\header {

  texidoc = "stanza numbers remain, even on otherwise empty lyrics
lines."
	  
}

\paper {
  indent = #0.0
  ragged-right = ##T
}

\version "2.11.51"

<<
  \new Voice = melody \relative c'{
    r2 r4 r8 e8 |\break
    e16 e8. 
  }
  \lyricsto "melody" \lyrics { 
    \set stanza = "Verse 2."
    _ bla bla  }
>>
