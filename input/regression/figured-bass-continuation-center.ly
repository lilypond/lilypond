\header {

  texidoc = "Pairs of congruent figured bass extender lines are
vertically centered if @code{figuredBassCenterContinuations} is set to
true."

}

\version "2.12.0"
\paper {
  ragged-right = ##t
}

<<
  \relative \new Voice {
    c8 c  b b  a a  b b  
    c c  b b
  
  }
 \figures {
    \set useBassFigureExtenders = ##t
    \set figuredBassCenterContinuations = ##t
    <6+ 4 3>4 <6 4 3> r
    <6+ 4 3>4 <6 4 3> <4 3+> r
  } 
>>
