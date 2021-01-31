\version "2.23.1"

\header {
  texidoc = "The @code{FingerGlideSpanner} grob prints nicely for all styles if
there are line breaks.  For the styles @code{stub-right}, @code{stub-left} and
@code{stub-right} the printed line is intentionally shorter."
}

mus = {
  <g''\glide-4>1 <f''-4> <g''\glide-4> \break <f''-4>
}

<<
  \new Voice
    \mus
  \new Voice
    \with { \override FingerGlideSpanner.style = #'line }
    \mus
  \new Voice
    \with { \override FingerGlideSpanner.style = #'stub-right }
    \mus
  \new Voice \with
    { \override FingerGlideSpanner.style = #'stub-left }
    \mus
  \new Voice
    \with { \override FingerGlideSpanner.style = #'stub-both }
    \mus
  \new Voice
    \with { \override FingerGlideSpanner.style = #'dashed-line }
    \mus
  \new Voice
    \with { \override FingerGlideSpanner.style = #'dotted-line }
    \mus
  \new Voice
    \with { \override FingerGlideSpanner.style = #'zigzag }
    \mus
  \new Voice
    \with { \override FingerGlideSpanner.style = #'trill }
    \mus
  \new Voice
    \with { \override FingerGlideSpanner.style = #'bow }
    \mus
  \new Voice
    \with { \override FingerGlideSpanner.style = #'none }
    \mus
>>
