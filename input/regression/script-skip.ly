\version "2.23.6"

\header {
  texidoc = "Scripts on skips are supported."
}

\new PianoStaff <<
  \new Staff \relative { e'4. f8 g4 a }
  \new Dynamics { \after 4. -> <> }
  \new Staff \relative { e'4. f8 g4 a }
>>

{ s^>^^ }
