\version "2.19.19"
\header {
    texidoc = "Test some transitions that might be found in string parts produced with \\partcombine."
}

vone =  \relative a' { a2 <a e> | r2 <a e> | r     r | r2 a4 r4 | g2 r | <b g> }
vtwo =  \relative a' { e2 <e a> | r2 r     | <d g> r | r2 f4 r4 | g2 r | <g d> }
combined = \partcombine \vone \vtwo

% The part combiner does not yet support all of these labels.
expectedText = \relative c' {
  s2_"div." s2_"unis." | s2 s2_"solo" | s2_"solo 2" s2 |
  s2 s4_\markup \column { tutti, div. } s4 | s2_"unis." s | s_"div."
}

\layout { ragged-right = ##t }

\new Staff \with {
     aDueText = "unis."
     soloText = "solo"
     soloIIText = "solo 2"
} <<
   \set Score.skipBars = ##t
   \combined
   \expectedText
>>
