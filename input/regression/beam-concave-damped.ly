\header {
    texidoc = "Beams that are not strictly concave are damped
 according to their concaveness. "
}
\version "2.19.21"
\layout {
    ragged-right = ##t
}

\relative {
    \time 2/4 
    g''=''8[ d a' b]
   \time 3/4
    f=''8[ e d c g b]
    b,16[ f' g a]
    r2
   \time 3/8
    c=''16[ b c e g <e b'>] |
   \stemUp b,8[  \stemDown d'8 bes8]
}
