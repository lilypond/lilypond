\version "2.21.0"

\header {

    texidoc= "The length of the transition between one syllable
and the next is indicated by the length of the arrow, which may
not start immediately after a new syllable."

}

\layout { ragged-right = ##t }

\relative c''{
    \set melismaBusyProperties = #'()
    c2( ~ c ~
    c4 d8 e f2 ~
    f1)
}
\addlyrics {
    oo "" \vowelTransition
    ah _ _
    "" \vowelTransition oh
}
