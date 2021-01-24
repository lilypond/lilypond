\header {

  texidoc = "Pairs of congruent figured bass extender lines are
vertically centered if @code{figuredBassCenterContinuations} is set to
true."

}

\version "2.19.21"
\paper {
  ragged-right = ##t
}

<<
  \relative \new Voice {
    c'8 c  b b  a a  b b
    c c  b b  a a  b b \break
    c c  b b  a a  b b
    c c  b b  a a  b b \break
    c c  b b  a a  b b
    c c  b b  a a  b b \break
    c c  b b  a a  b b
    c c  b b  a a  b b
  
  }
  \figures {
    \set useBassFigureExtenders = ##t
    \set figuredBassCenterContinuations = ##t
    <6+ 4 3>4 <6 4 3> r
    <6+ 4 3>4 <6 4 3> <4 3+> r r
    % FIXME: This looks bad, the reader cannot differentiate between only
    % continuation or three...
    <6+ 4 3 2>4 <6 4 3 2> r
    <6+ 4 3 2>4 <6 4 3 2> <4 3+> r r
    <6 4 3 2>4 <6 4 3 2> <4 3> r
    <6 4 3 2>4 <6 4 3 2> <6 4> r r
    <6 5 4 3 2 1>4 <6 5 3 2 1> <4 3 2> r
    <6 5 4 3 2 1>4 <6 5 3 2 1> <6 5 4 3 2> r
  } 
>>
