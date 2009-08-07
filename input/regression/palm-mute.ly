\version "2.13.4"

\header{ texidoc = "The palm mute technique for stringed instruments
                    is supported by triangle-shaped note heads."
       }

palmmute = \relative c, {
    \time 4/4
    e8^\markup { \musicglyph #"noteheads.u2do"  = palm mute }
    \palmMuteOn e e \palmMuteOff  e e  \palmMute e e e |
    e8 \palmMute { e e e } e e e e |
    \palmMuteOn < e b' e >8 e e e < e b' e >2 \palmMuteOff |
    < \palmMute e b' e >8 \palmMute { e e e } < \palmMute e b' e >2
    \bar "|."
}

\context StaffGroup <<
  \context Staff <<
    \clef "G_8"
    \palmmute
  >>
  \context TabStaff <<
    \palmmute
  >>
>>


