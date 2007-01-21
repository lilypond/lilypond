\version "2.10.12"

\header { texidoc = "
<p>Ambits can be added per voice. In that case, the
ambitus must be moved manually to prevent collisions.
" }

\new Staff <<
    \new Voice \with {
	\consists "Ambitus_engraver"
    } \relative c'' {
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

