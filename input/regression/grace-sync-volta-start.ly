\version "2.27.5"

\header {
  texidoc = "A volta repeat must not begin after grace notes in parallel music."
}

\layout { ragged-right = ##t }

\new Staff \with
{ instrumentName = \markup \center-column { Graces after volta repeat bars } }
{
    \repeat volta 2 { \grace a'16 g'8 f' e' d' c'2 }
    \grace g'8 c'1
    \repeat volta 2 { \grace a'16 g'8 f' e' d' c'2 }
  }

\new StaffGroup \with
{ instrumentName = \markup \center-column { Graces after volta repeat bars } }
<<
  \new Staff {
    \repeat volta 2 { \grace a'16 g'8 f' e' d' c'2 }
    \grace g'8 c'1
    \repeat volta 2 { \grace a'16 g'8 f' e' d' c'2 }
  }
  \new Staff {
    \repeat volta 2 { c'2 d' }
    c'1
   \repeat volta 2 { c'2 d' }
  }
>>
