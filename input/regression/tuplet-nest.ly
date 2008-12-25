
\version "2.12.0"

\header {
    texidoc=" Tuplets may be nested."
}

\paper {
  ragged-right = ##t  
  indent = 0.0
}

\relative c'' {
  \override TupletNumber #'text = #tuplet-number::calc-fraction-text
  \times 4/6 {
    \times 2/3 {
      a a a
    }
    \times 3/5 {
      a a a a a
    }
  }

  s1*1/6
  
  \stemUp
  \times 4/6 {
    \times 2/3 {
      a1*1/6 a f'
    }
    \times 3/5 {
      c f g a b 
    }
  }
  
}

