\header
{
  texidoc = "Frets are supported as markup commands."
}

\version "2.3.1"

\score {
    <<
       \new ChordNames \chords {c2. f d s bes}
       
    \notes \context Voice=mel {
%    c'2. c' c' c' c'
        c'2.^\markup \override #'(staff-padding . 4.0) {\fret-diagram #1 #"f:2;6-x;5-3-3;4-2-2;3-o;2-1-1;1-o;"}
        f'^\markup {\fret-diagram #1 #"c:6-1-1;p:0.5;6-1;5-3;4-3;3-2;2-1;1-1;"}
        d' ^\markup \fret-diagram #1 #"f:1;6-x;5-x;4-o;3-2-1;2-3-3;1-2-2;"
        d' ^\markup \fret-diagram #.75 #"f:1;6-x;5-x;4-o;3-2-1;2-3-3;1-2-2;"
        bes' ^\markup \fret-diagram #1.5 #"6-1;5-1;4-3;3-3;2-3;1-1;c:6-1-1;c:2-4-3;"
        bes'
        a'2.^\markup \fret-diagram #1 #"6-x;5-x;4-o;3-14;2-13;1-12;"
        c''
        bes'2.^\markup \fret-diagram #1 #"6-1;5-1;4-3;3-3;2-3;1-1;c:6-1-1;c:2-4-3;"
    }
    >>
  \paper{ raggedright = ##t }
}
