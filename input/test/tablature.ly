\version "2.3.8"

\header{ texidoc = "@cindex Tabulature
Tablature is internally done by overriding the note-head formatting function
and let it act on a 6-line staff. A special engraver takes then care of 
choosing the fret and converting the pitch to a number. 

Thus, by providing the fret numbers, the same music can be generated both 
for a normal and tabulature staffs. By default, the fret is the smallest 
possible, according to @code{minimumFret}.

"
}

partition =  {
    \key e \major
    e8\5 fis\5 gis\5 a\5 b\5 cis'\5 dis'\5 e'\5
    e8\4 fis\4 gis\4 a\4 b\4 cis'\4 dis'\4 e'\4
    e8 fis gis a b cis' dis' e'
    \set Score.minimumFret = #5
    e8 fis gis a b cis' dis' e'
}

\score {
  \context StaffGroup <<
    \context Staff <<
	\clef "G_8"
	\partition
    >>
    \context TabStaff <<
	\partition
    >>
  >>
}

