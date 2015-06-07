\header {

  texidoc = "Midi also handles crescendo and decrescendo, either
  starting and ending from specified or unspecified sound level."

}

\version "2.19.21"

\score {
  \relative {

    \set midiMinimumVolume = #0.0
    \set midiMaximumVolume = #1.0
    c'\ff c\pppp
    c\ff\> c c c c\!\pppp

    c\< c c c c\! \ff

    c\> c c c \!  
  } 
  \midi {}
  \layout{}
}
