\version "1.5.68"
\header {
texidoc = "Stanza numbers may differ for the first and following systems."
}

\score {

< \context LyricsVoice \lyrics {

\property LyricsVoice . stanza =  "first"
\property LyricsVoice . stz =  #'("2" (super "nd"))
Foo1 Bar1   }

\notes { c''1 \break c''1 }>} 
