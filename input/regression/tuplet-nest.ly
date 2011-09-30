
\version "2.14.0"

\header {
    texidoc=" Tuplets may be nested."
}

\paper {
  ragged-right = ##t  
  indent = 0.0
}

\relative c'' {
  \times 4/3 {
    \times 2/3 { c8[ c c] }
    \times 2/3 { c8[ c c] }
    \times 2/3 { c8[ c c] }
  }

  \times 4/3 {
    \times 2/3 { a8[ a a] }
    \times 2/3 { a8[ a a] }
    \times 2/3 { a8[ a a] }
  }

  \override TupletNumber #'text = #tuplet-number::calc-fraction-text
  \times 4/6 {
    \times 2/3 {
      a4 a a
    }
    \times 3/5 {
      a4 a a a a
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

