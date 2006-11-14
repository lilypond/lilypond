
\version "2.10.0"
\header {
    texidoc = "@cindex Dynamic Absolute Volume
Absolute dynamics have an effect on MIDI files.
"
}


\score{
\relative c''{
%segfault in engraver
a1\ppp 
a1\pp
a\p
a\mp
a\mf
a\f
a\ff
a\fff
a\sf
}
\layout{ ragged-right = ##t }

  \midi {
    \context {
      \Score
      tempoWholesPerMinute = #(ly:make-moment 60 1)
      }
    }


}

