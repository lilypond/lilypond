\version "1.7.6"

\include "paper16.ly"
 
viola = \notes \relative c'  \context Voice = viola {
    <<c g' c>>4-\arpeggio
    \voiceTwo
    g'8. b,16
    s1 s2. r4
    g
}

oboes = \notes \relative c'' \context Voice = oboes {
    \voiceOne
    s4  g8. b,16 c8 r <<e' g>>8. <<f a>>16
    \grace <<e g>>8-( <<d f>>4-) <<c e>>2
    \times 2/3 { <<d  f>>8 <<e g>> <<f a>> }
    <
        { \times 2/3 { a8 g c }  c2 }
	\\
        { f,8 e e2 }  
    >

    \grace <<c, e>>8-( <<b d>>8.-)-\trill <<c e>>16 | 
    [<<d  f>>-( <<f a>>8.-)] <<b, d>>8 r [<<d f>>16-( <<f a>>8.-)] <<b, d>>8 r  |
    [<<c e>>16-(  <<e g>>8.-)] <<c e,>>8
}

hoomPah = \repeat unfold 8 \notes
    \transpose c c {
	\translator Staff = down
	\stemUp
	c8
	\translator Staff = up
	\stemDown
	c'8 }

bassvoices = \notes \relative c' {
    c4 g8. b,16
    \context Voice \hoomPah
    \translator Staff = down
    \stemBoth 
    
    [c8 c'8] r4
    <<g d'>> r4
    < { r2 <<e c'>>4  <<c g'>>8 } \\
      { g2-~ | g4 c8 } >
}

\score {
    \context PianoStaff \notes <
        \context Staff = up < 
             \oboes
             \viola
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
%% new-chords-done %%


