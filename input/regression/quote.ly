\header
{

    texidoc = "With @code{\\quote}, fragments of previously entered
music may be quoted. @code{quotedEventTypes} will determines what
things are quoted. In this example, a 16th rests is not quoted, since
@code{rest-event} is not in @code{quotedEventTypes}."

}
\version "2.3.17"
\paper {
    raggedright = ##t
}

\addquote bla \relative c' {
    fis4 r16  a8.-> b-\ff }

\relative c'' {

    \set Staff.quotedEventTypes = #'(note-event articulation-event)
    c8 d8  <<
	s2 
	\new Voice {
	    \set fontSize = #-2
	    \quote bla 2

	} >>
    es8 gis
	
    }

