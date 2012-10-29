
\version "2.17.6"

\header {

  texidoc = "In cue notes, Tuplet stops are handled before new tuplets
  start."

}

foo = \relative c' {
  \times 2/3 { c4 c c } \times 2/3 { c4 c c }
}

\addQuote "foo" { \foo }

\paper {
  ragged-right = ##t
}

\new Staff
<<
  \new Voice \transpose c c' {
    \override Stem.direction = #UP
    \foo
  }
  \new Voice {
    \cueDuring #"foo" #DOWN { s1 }
  }
>>
  
