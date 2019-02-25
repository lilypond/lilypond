
\version "2.21.0"

\header {

  texidoc = "In cue notes, Tuplet stops are handled before new tuplets
  start."

}

foo = \relative {
  \tuplet 3/2 { c'4 c c } \tuplet 3/2 { c4 c c }
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
    \cueDuring "foo" #DOWN { s1 }
  }
>>
  
