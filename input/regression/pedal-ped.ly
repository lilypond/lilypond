\header
{
    texidoc = "Piano pedals: the standard style is with Ped symbols.
The string can be tuned. This example shows the shorter tilde/P variant
at the end of the melody."

}


\score{
    \context Staff \notes\relative c'{

        c4 d e f g
        \sustainDown b c
        c, d16[  c  c c]  e[ e \sustainUp \sustainDown e e ] f4 \sustainUp 
        g\sustainDown  b \sustainUp c 
        \property Staff.pedalSustainStrings = #'("-" "-P" "P")
        \property Staff.SustainPedal \override #'padding = #-2
        c, \sustainDown d e \sustainUp \sustainDown f
        \sustainUp g b c



    }
}
