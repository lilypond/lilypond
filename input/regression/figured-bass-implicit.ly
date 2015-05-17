
\header
{
  
  texidoc = "Implicit bass figures are not printed, but they do get extenders."
}


\version "2.19.21"
\paper
{
  ragged-right = ##t
}

<<
  \relative \new Voice {
    c''^"normal" c c c^"extenders" c c c_"implicit" c
    }
  \figures {
    <3 6!>
    <3 4+>
    r
    \set useBassFigureExtenders = ##t
    <3 6!>
    <3 4+>
    r
    \set useBassFigureExtenders = ##t
    \set implicitBassFigures = #'(3)
    <3 6!>
    <3 4+>
  }  
>>  
  
