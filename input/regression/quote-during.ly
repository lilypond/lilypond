\header
{

    texidoc = "With @code{\\cueDuring} and @code{\\quoteDuring},
fragments of previously entered music may be
quoted. @code{quotedEventTypes} will determines what things are
quoted. In this example, a 16th rest is not quoted, since
@code{rest-event} is not in @code{quotedEventTypes}."

}
\version "2.21.0"
\layout {
    ragged-right = ##t
}


quoteMe = \relative { fis'4 r16  a8.-> b4-\ff c }

\addQuote quoteMe \quoteMe 
original = \relative { c''8 d s2 es8 gis8 }

<<
    \new Staff {
	\set Staff.instrumentName = "quoteMe"
	\quoteMe
    }
    \new Staff {
	\set Staff.instrumentName = "orig"
	\original
    }
    \new Staff \relative c'' <<
	
	\set Staff.instrumentName = "orig+quote"	
	\set Staff.quotedEventTypes = #'(note-event articulation-event)
	\new Voice {\voiceOne
	    \override Voice.Beam.collision-voice-only = ##t
	    \original }
	\new Voice {\voiceTwo s4 \quoteDuring "quoteMe" { s2. } }
    >>
>>
