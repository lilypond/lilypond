\version "2.23.1"

\header{
  texidoc =
"The palm mute technique for stringed instruments
is supported by triangle-shaped note heads."
}

palmmute = \relative c, {
    \time 4/4
    \palmMuteOn
    e8^\markup { \musicglyph "noteheads.s2do"  = palm mute }
    e e
    \palmMuteOff  e e  \palmMute e e e |
    e8 \palmMute { e e e } e e e e |
    \palmMuteOn < e b' e >8 e e e < e b' e >2 \palmMuteOff |
    < \palmMute e b' e >8 \palmMute { e e e } < \palmMute e b' e >2
    \bar "|."
}

\context StaffGroup <<
  \context Staff {
    \context Voice {  % Warning: explicit Voice instantiation is
                      %    required to have palmMuteOff work properly
                      %    when palmMuteOn comes at the beginning of
                      %    the piece.
      \clef "G_8"
      \palmmute
    }
  }
  \context TabStaff <<
    \palmmute
  >>
>>


