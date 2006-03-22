
\version "2.8.0"
\header {
    texidoc = "@cindex Rests

Rests may be used in various styles.

"
}

\layout {
    indent = 0.0
    ragged-right = ##t
}

\context Staff \relative c {
    \set Score.timing = ##f
    \override Staff.Rest  #'style = #'mensural
    r\maxima^\markup \typewriter { mensural }
    r\longa r\breve r1 r2 r4 r8 r16 r32 r64 r128 r128
    \bar "" 

    \override Staff.Rest  #'style = #'neomensural
    r\maxima^\markup \typewriter { neomensural }
    r\longa r\breve r1 r2 r4 r8 r16 r32 r64 r128 r128
    \bar "" 

    \override Staff.Rest  #'style = #'classical
    r\maxima^\markup \typewriter { classical }
    r\longa r\breve r1 r2 r4 r8 r16 r32 r64 r128 r128
    \bar ""
    
    \override Staff.Rest  #'style = #'default
    r\maxima^\markup \typewriter { default }
    r\longa r\breve r1 r2 r4 r8 r16 r32 r64 r128 r128
    
}

