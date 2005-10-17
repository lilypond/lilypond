\version "2.7.13"
\header
{
  texidoc = "Melismata are triggered by manual beams." 
}

  \layout { raggedright = ##t }




<<
  \new Staff
  \relative c'' {
    \set Staff.autoBeaming = ##f
    c8 c8[ c8 c8]  c8    }
  
  \lyricsto "" \new Lyrics \lyricmode { bla bla bla }
>>


