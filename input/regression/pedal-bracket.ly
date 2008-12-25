\version "2.12.0"

\header {

    texidoc = "The brackets of a piano pedal should start and end at
 the left side of the note. If a note is shared between two brackets,
 these ends are flared.

At a line-break, there are no vertical endings.  " }

\score {
     \relative c'' {
        \set Staff.pedalSustainStyle = #'bracket

        c4 d e \sustainOn b c c, \sustainOff \sustainOn  d8[ c]  e8[ 
	    e \sustainOff \sustainOn]  f4 d
        \sustainOff g \sustainOn b b, \sustainOff c'

        \set Staff.pedalUnaCordaStyle = #'mixed

        c4 d \unaCorda e f g
        b  | \break c b c \treCorde c
    }
    \layout { ragged-right = ##t }
}
