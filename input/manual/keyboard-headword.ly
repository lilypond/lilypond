% Sergei Rachmaninoff, Prelude Op. 3 No. 5, mm. 44–47

\version "2.11.49"
\include "english.ly"

#(set-global-staff-size 15)
\paper{
  ragged-right=##t
  line-width=17\cm
  indent=0\cm
}

rh = { \change Staff = RH \voiceFour }
lh = { \change Staff = LH \voiceOne }

\new PianoStaff <<
  \set Score.currentBarNumber = #44
  \new Staff = "RH" {
    <<
      \override Staff.NoteCollision #'merge-differently-dotted = ##t
      \relative c''' {
        \key g \minor
        r8 <a fs>--( <a fs>-- <bf fs d bf>-- <c g ef c>4-- <bf bf,>8 <a
          a,>8)
        <g g,>8( <a a,> <bf bf,>4)-- <fs d c fs,>8(-- <g g,> <a d, c a>4)
        r8 <bf bf,>(_\markup \italic cresc. <bf g d> <c c,>
          <d bf af d,>4) <c c,>8( <bf bf,>)
        <c ef, c>\mf( <d d,> <ef ef,> <f f,>) <g g,>( <a a,>4\> <bf
          bf,>8)\!
        <fs, fs,>8\p
      }
      \\
      \relative c'' {
        s8 a16( c d4)-- s4 <ef c>
        <e cs>4 <e cs> s2
        s1
        s4 <bf' g> <d bf>2
      }
    >>
  }
  \new Staff = "LH" <<
    \override Staff.NoteCollision #'merge-differently-dotted = ##t
    \clef bass
    \key g \minor
    \new Voice \relative c' {
      \rh <c~ fs,>2-- c8 \lh d ef f
      g4 fs8-- \rh g^- a4.-- a16( c
      \showStaffSwitch
      <d bf d,>2--) r8 d-- ef-- f--
      g4-- \lh g,~-- g8 \clef treble a-- bf-- c--
      \rh d2*1/4
    }
    \new Voice \relative c, {
      \times 4/6 {d16[( a' d fs c' d]}
      \override TupletNumber #'transparent = ##t
      \times 4/6 {fs d c fs, d a)}
      \voiceTwo
      \times 4/6 {d,[ a' g' d'( c g)]}
      \times 4/6 {ef'( c g) f'( c g)}
      
      \times 4/6 {d,[( a' g' a bf cs])}
      \times 4/6 {fs( cs bf g a, d,~)}
      \oneVoice
      \times 4/6 {d[( a' d d a' d]}
      \times 4/6 {c a d, d d, d')}
      
      \times 4/6 {g,[( d' g bf \clef treble d g]}
      \times 4/6 {bf g d \clef bass bf g d)}
      \times 4/6 {f,([ d' g af bf \clef treble d]}
      \times 4/6 {af' ef bf d \clef bass af bf,)}
      
      \times 4/6 {ef,([ bf' g' bf ef f])}
      \voiceTwo
      \times 4/6 {g( ef bf g bf, ef,)}
      \times 4/6 {e[( g' <d' bf>) a'( g d)]}
      \times 4/6 {bf'( g d) c'( g d)}
      
      \oneVoice
      \clef bass a,,16
    }
  >>
>>
