\version "2.18.0"

\header {
  texidoc = "Easy noteheads should be scalable in size, like in grace notes."
}

\layout { ragged-right = ##t }

mus = {
  \acciaccatura g'8 c'4 d' e' f' c'2 g' c'1
}

<<
  \new Staff { \mus \huge \mus \tiny \mus \bar "|." }
  \new Staff \with \easyHeadsOn { \mus \huge \mus \tiny \mus }
>>
