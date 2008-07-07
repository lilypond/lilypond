\header {

    texidoc = "Tuplet brackets stay clear of the staff. The
slope is determined by the graphical characteristic of the notes, but
if the musical pattern does not follow graphical slope, then the
bracket is horizontal

The bracket direction is determined by the dominating stem direction.
 
"

}

\layout {
  ragged-right = ##t 
}

\version "2.11.51"

\new Voice {
  \relative c'' {
    \times 2/3 { c4 d e}
    \times 2/3 { c4 d e}
  }
  
  \relative c' {
    \times 4/5 { a'4 as g fis f }
    \times 4/5 { fis4 e es d des }
    \times 4/5 { fis,4 e es d des }
    \times 4/5 { bes'''4 bes,, b c cis }
    \times 4/5 { a''4 b b c cis }
  }
}

