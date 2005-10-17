\header {

  texidoc = "Pairs of congruent figured bass extender lines are vertically centered.
 "

}

\version "2.7.13"
\paper {
  raggedright = ##t
}

<<
  \relative \new Voice {
    c8 c  b b  a a  b b  
    c c  b b
  
  }
 \figures {
    \set useBassFigureExtenders = ##t
    <6+ 4 3>4 <6 4 3> r
    <6+ 4 3>4 <6 4 3> <4 3+> r
  } 
>>
