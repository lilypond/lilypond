
\include "paper16.ly"

stemdown = \property Voice.Stem \override #'direction = #-1
stemup = \property Voice.Stem \override #'direction = #1
stemboth = \property Voice.Stem \revert #'direction  

viola = \notes \relative c' \context Voice = viola {
    <c4-\f-\arpeggio g' c>
    \stemDown g'8. b,16
    s1 s2. r4
    g
}

oboes = \notes \relative c'' \context Voice = oboe {
    \stemUp s4  g8. b,16 c8 r <e'8.^\p g> <f16 a>
    \grace <e8( g> <d4 f> <c2 e>
    \times 2/3 { <d8 \< f> <e g> <f a> }
    <
        { \times 2/3 { a8 g c } \! c2 }
        \context Voice = oboeTwo {
            \stemDown
            \grace {
                \property Grace.Stem \override #'direction = #-1
                [f,16 g] }
                f8 e e2
            }
    >
    \stemBoth
    \grace <c,8( e> <)b8. d8.-\trill> <c16 e> | 
    [<d ( f> < )f8. a>] <)b,8 d> r [<d16( f> <f8. )a>] <b,8 d> r  |
    [<c16( e>  < )e8. g>] <c8 e,>
}

hoomPah = \repeat unfold 8 \notes
    \transpose c' { c8 \stemDown c'8 \stemUp }

bassvoices = \notes \relative c' {
    c4 g8. b,16
    \autochange Staff \hoomPah
    \translator Staff = down
    \stemDown [c8 c'8] r4
    <g d'> r4
    < {\stemUp r2 <e4 c'> <c8 g'> }
        \context Voice = reallyLow  {\stemDown g2 ~ | g4 c8 } >
}

\score {
    \context PianoStaff \notes <
        \context Staff = up < \time 2/2
             \viola
             \oboes
         >
         \context Staff = down < \time 2/2 \clef bass
             \bassvoices
         >
    >
    \midi { }
    \paper {
        indent = 0.0
        linewidth = 15.0 \cm }
}
