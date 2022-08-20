\version "2.23.2"

\header {
  lsrtags = "keyboards, specific-notation, symbols-and-glyphs"

  texidoc = "
Accordion register symbols are available as @code{\\markup} as well as
as standalone music events (as register changes tend to occur between
actual music events).  Bass registers are not overly standardized.  The
available commands can be found in 'Discant symbols' in the Notation
Reference.
"

  doctitle = "Accordion register symbols"
}


#(use-modules (lily accreg))

\new PianoStaff
<<
  \new Staff \relative {
    \clef treble
    \discant "10"
    r8 s32 f'[ bes f] s e[ a e] s d[ g d] s16 e32[ a]
    <<
      { r16 <f bes> r <e a> r <d g> }
      \\
      { d r a r bes r }
    >> |
    <cis e a>1
  }

  \new Staff \relative {
    \clef treble
    \freeBass "1"
    r8 d'32 s16. c32 s16. bes32 s16. a32[ cis] s16
    \clef bass \stdBass "Master"
    <<
      { r16 <f, bes d>^"b" r <e a c>^"am" r <d g bes>^"gm" |
      <e a cis>1^"a" }
      \\
      { d8_"D" c_"C" bes_"B" | a1_"A" }
    >>
  }
>>
