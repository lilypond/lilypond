\version "2.21.0"
\header {
    texidoc = "Test some transitions that might be found in string parts produced with \\partCombine."
}

vone =  \relative { a'2 <a e> | r2 <a e> | r     r | r2 a4 r4 | g2 r | <b g> }
vtwo =  \relative { e'2 <e a> | r2 r     | <d g> r | r2 f4 r4 | g2 r | <g d> }
combined = \partCombine \vone \vtwo

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
