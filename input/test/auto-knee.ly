\header{
texidoc="One automatic knee";
}

\score {
  \context Staff \notes\relative c''{ 
    [c'8 c,,] [c8 e']
  }
  \paper{
    linewidth = 40*\staffspace;
    \translator {
      \VoiceContext
      Beam \override #'auto-knee-gap = #13
    }
  }
}
