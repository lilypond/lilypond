\version "2.27.5"

\header
{
  texidoc = "Quotes may contain grace notes.  Where structural elements like
time signatures, key signatures, clefs and so on occur in the main music or
in parallel, they may move before the grace notes in the quoted music."
}

quoted = \fixed c' { r4
  c4 \acciaccatura dis8 e4 \acciaccatura fis8 g4 \acciaccatura b8 c'\breve
}

\addQuote quoted \quoted

global = { \time 4/4 s1 \time 2/1 }

\new Staff \with { instrumentName = "original" }
<< \quoted \global >>

\new Staff \with { instrumentName = "quoted" }
<< \cueDuring "quoted" #UP
   \new Voice { \voiceOne c''1 \bar "||" c'''\breve }
   \global
>>
