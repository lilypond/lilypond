\header {

  texidoc = "Figured bass extender lines run between repeated bass
figures. They are switched on with @code{useBassFigureExtenders}"

}

\version "2.19.21"
\paper {
  ragged-right = ##t
}

<<
  \relative \new Voice {
    c'8 c  b b  a a  b b  
    c^"the same with extenders" c  b b  a a  b b  
  
  }
 \figures {
    <6+ 4 3>4 <6 4 3> <4 3+> r4
    \set useBassFigureExtenders = ##t
    <6+ 4 3> <6 4 3> <4 3+> r4
  } 
>>
