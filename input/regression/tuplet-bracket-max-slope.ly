\version "2.25.0"

\header{
  texidoc="
Tuplet brackets are not too steep.  In groups without beams, their
angle should match an equivalent beam slant when possible.
"
}

\layout {
  \override TupletBracket.bracket-visibility = ##t
}

{
  \once \tupletUp \tuplet 3/2 { d'4 a'''8 }
  \tuplet 5/4 { d'4 <f'' e'''>16 }
  \tuplet 5/4 { <f'' f'''>16 d'4 }
  \tuplet 5/4 { <f'' g'''>16 d'4 }
  \tuplet 5/4 { <f'' a'''>16 d'4 }
  \tuplet 5/4 { <f'' b'''>16 d'4 }
  \tuplet 5/4 { <f'' c'''''>16 d'4 }
}

{
  \tupletUp
  \tuplet 2/2 { d''8 d''}
  \tuplet 2/2 { d''8 e''}
  \tuplet 2/2 { d''8 f''}
  \tuplet 2/2 { e''8 g''}
  \tuplet 2/2 { d''8 a''}
  \tuplet 2/2 { e''8 b''}
  \tuplet 2/2 { d''8 c'''}
  \tuplet 2/2 { e''8 d'''}
  \tuplet 2/2 { d''8 e'''}
  \tuplet 2/2 { e''8 f'''}
  \tuplet 2/2 { d''8 g'''}
  \tuplet 2/2 { e''8 a'''}
  \tuplet 2/2 { d''8 b'''}
}

{
  \tupletUp
  \tuplet 2/2 { d''4 d''}
  \tuplet 2/2 { d''4 e''}
  \tuplet 2/2 { d''4 f''}
  \tuplet 2/2 { e''4 g''}
  \tuplet 2/2 { d''4 a''}
  \tuplet 2/2 { e''4 b''}
  \tuplet 2/2 { d''4 c'''}
  \tuplet 2/2 { e''4 d'''}
  \tuplet 2/2 { d''4 e'''}
  \tuplet 2/2 { e''4 f'''}
  \tuplet 2/2 { d''4 g'''}
  \tuplet 2/2 { e''4 a'''}
  \tuplet 2/2 { d''4 b'''}
}

\relative {
  \tupletUp \tuplet 5/4 { bes'''16 bes,, b16 c cis16 }
  \tupletUp \tuplet 5/4 { bes''16 bes,, b16 c8 }
  \tupletUp \tuplet 5/4 { bes''16 bes,, b8. }
  \tupletUp \tuplet 5/4 { bes''16 [bes,,] b8. }
}

\new PianoStaff <<
  \new Staff = "rh" \relative {
    \time 3/4
    s2.
  }
  \new Staff = "lh" \relative {
    \clef bass
    \time 3/4
    \tuplet 6/4 {
      a,16 e' c'
      \change Staff = "rh"
      a' e' c'
    }
    \once \tupletUp
    \tuplet 9/8 {
      \change Staff = "lh"
      a,,,16 \tuplet 3/2 { c'32
      \change Staff = "rh"
      a' e'} \tuplet 4/3 {
        c'32 b c d
      }
      e32 c'
    }
    \once \tupletUp
    \tuplet 9/8 {
      \change Staff = "lh"
      a,,,,16 \tuplet 3/2 { c'32 [
      \change Staff = "rh"
      a' e'} \tuplet 4/3 {
        c'32 b c d
      }
      e32 c']
    }
  }
>>


aigues = \relative c' {
  \time 6/8
  s4.
  \stemDown
  c16[ bes' e]
  \stemUp
  g c e
  \stemDown
  g8
}

basses = \relative c {
  \clef F
  \tuplet 7/6 {
    c8[ e'16]
    \change Staff = "md"
    \stemUp
    g[ c e8]
  }
  s4. s8
}

\new PianoStaff
<<
  \new Staff = "md" \aigues
  \new Staff = "mg" \basses
>>

\context PianoStaff <<
  \new Staff = "up"
  \relative <<
    {
      \tuplet 7/6 {
      \stemDown
      f''16 d b \change Staff = down \stemUp
      \clef treble g ~ < g e>8.
      }

    } \\
  >>
  \new Staff = "down" {
    \time 3/8 \clef bass s4. }
>>
