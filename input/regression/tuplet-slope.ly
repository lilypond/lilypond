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

\version "2.17.11"

\new Voice {
  \relative c'' {
    \tuplet 3/2 { c4 d e}
    \tuplet 3/2 { c4 d e}
  }
  
  \relative c' {
    \tuplet 5/4 { a'4 as g fis f }
    \tuplet 5/4 { fis4 e es d des }
    \tuplet 5/4 { fis,4 e es d des }
    \tuplet 5/4 { bes'''4 bes,, b c cis }
    \tuplet 5/4 { a''4 b b c cis }
  }
}

