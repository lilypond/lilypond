\version "2.19.21"
\header {
texidoc = "

Midi2ly tuplet test.

@example
  python scripts/midi2ly.py --duration-quant=32 \
      --allow-tuplet=4*2/3 \
      --allow-tuplet=8*2/3 \
      --allow-tuplet=4*3/5 \
      --allow-tuplet=8*3/5 \
      tu.midi 
@end example
"
}


\score { 
  \context Voice  \relative {

    a1 a2 a2. a4 a4. a8 a8. a16 a16. a32 a32. a64

    \tuplet 3/2 { b4 b4 b4 }
    \tuplet 5/3 { b4 b4 b4 b4 b4 }

    \tuplet 3/2 { c8 c8 c8 }
    \tuplet 5/3 { c8 c8 c8 c8 c8 }

  }
  \layout { }  
  \midi { }
}


