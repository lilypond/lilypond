
\header {

    texidoc = "Piano pedal brackets.  Brackets should start and end at
 the left side of the note. If a note is shared between two brackets,
 the ends are flared.

There are no vertical endings at a line-break.  " }

\score {
    \notes \relative c'' {
        \property Staff.pedalSustainStyle = #'bracket

        c4 d e \sustainDown b c c, \sustainUp \sustainDown  d8[ c]  e8[ 
	    e \sustainUp \sustainDown] f4 d
        \sustainUp g \sustainDown b b, \sustainUp c'

        \property Staff.pedalUnaCordaStyle = #'mixed

        c4 d \unaCorda e f g
        b  | \break c b c \treCorde c
    }
    \paper { raggedright = ##t }
}
