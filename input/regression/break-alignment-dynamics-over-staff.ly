\version "2.23.3"

\header {
  texidoc = "A @code{Dynamics} context over a @code{Staff} does not
impact the spacing of bar numbers relative to the staff at a line
break.  Bar number 2 should appear in its usual spot."
}

\paper { ragged-right = ##t }

\new Score \fixed c'' <<
  \new Dynamics { \tempo "Tempo 1" s1 \break \tempo "Tempo 2" s1 }
  \new Staff { c1 c1 }
>>
