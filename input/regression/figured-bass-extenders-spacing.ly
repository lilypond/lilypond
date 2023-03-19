\version "2.25.3"

\header {
  texidoc = "Figured bass extenders do not distort vertical spacing."
}

<<
  { c'4 d' e' f' }
  \new FiguredBass \figuremode { <5>2 <5> }
>>

% spacing should be comparable to the previous score
<<
  { c'4 d' e' f' }
  \new FiguredBass \figuremode { \bassFigureExtendersOn
                                 <5>4 4 4 4 }
>>
