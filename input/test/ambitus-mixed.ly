
\header {

    texidoc = "Ambits can be added per voice. In that case, the
ambitus must be moved manually to prevent collisions."

}

\version "2.7.32"

\layout {
    ragged-right = ##t
}

\new Staff <<
    \new Voice \with {
	\consists "Ambitus_engraver"
    } \relative c'' {
	\override Ambitus #'X-offset = #-1.0
	\voiceOne
	c4 a d e f2
    }
    \new Voice \with {
	\consists "Ambitus_engraver"
    } \relative c' {
	\voiceTwo
	es4 f g as b2
    }
>>
