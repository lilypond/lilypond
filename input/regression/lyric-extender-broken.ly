\version "2.1.7"
\header{
    texidoc =

    "Extenders that end a staff should not extend past the staff."

}

sopran = \notes \relative c'' {
\time 3/4 a2.( | \break
 g2) g4
}

text = \lyrics {
Aaaaa __ aaaaaah
}

\score {
<<
    
  \context Voice = "foo" \sopran
  \context LyricsVoice \newaddlyrics foo \text
>>
\paper { linewidth = 5.0\cm
}
}

