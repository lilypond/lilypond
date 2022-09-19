\version "2.23.14"

\header {

    texidoc = "The brackets of a piano pedal should start and end at
 the left side of the main note-column. If a note is shared between
 two brackets, these ends are flared.

At a line-break, there are no vertical endings.  Pedal changes can
be placed at spacer rests." }

\score {
     \relative {
        \set Staff.pedalSustainStyle = #'bracket

        c''4 d <e f b,> \sustainOn b c c, \sustainOff \sustainOn  d8[ c]  e8[
            e \sustainOff \sustainOn]  f4 r \sustainOff
            g \sustainOn bes bes, \sustainOff c'

        \set Staff.pedalUnaCordaStyle = #'mixed

        c4 d \unaCorda e f g b \break
        \tweak self-alignment-X #CENTER \textMark "long mark"
        c b <c e,>\arpeggio \treCorde c |
        b8\sustainOn g d b <<c2 {s4 s4\sustainOff\sustainOn }>> |
        g1
    }
    \layout { ragged-right = ##t }
}
