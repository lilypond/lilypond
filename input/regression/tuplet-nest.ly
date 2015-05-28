
\version "2.19.21"

\header {
    texidoc=" Tuplets may be nested."
}

\paper {
  ragged-right = ##t  
  indent = 0.0
}

\relative {
  \tuplet 3/4 {
    \tuplet 3/2 { c''8[ c c] }
    \tuplet 3/2 { c8[ c c] }
    \tuplet 3/2 { c8[ c c] }
  }

  \tuplet 3/4 {
    \tuplet 3/2 { a8[ a a] }
    \tuplet 3/2 { a8[ a a] }
    \tuplet 3/2 { a8[ a a] }
  }

  \override TupletNumber.text = #tuplet-number::calc-fraction-text
  \tuplet 6/4 {
    \tuplet 3/2 {
      a4 a a
    }
    \tuplet 5/3 {
      a4 a a a a
    }
  }

  s1*1/6
  
  \stemUp
  \tuplet 6/4 {
    \tuplet 3/2 {
      a1*1/6 a f'
    }
    \tuplet 5/3 {
      c f g a b 
    }
  }
  
}

