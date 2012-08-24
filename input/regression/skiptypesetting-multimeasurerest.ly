\version "2.16.0"

\header {
  texidoc = "
When @code{skipTypesetting} is set during a @code{skipBars}-induced
@code{MultiMeasureRest} spanner, no segfault occurs.
"
}

<<
  {
    \time 3/4
    \set Score.skipBars = ##t
    a4 a a
    R2.*2
  }
  \\
  {
    \set Score.skipTypesetting = ##t
    s2. s4
    \set Score.skipTypesetting = ##f
  }
>>
