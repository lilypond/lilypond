flauti = \notes \relative c' {
  c1 
  \break c
}


timpani = \notes \relative c' {
  c1 c
}



\score {
  < 
    \context StaffGroup ="legni" < 
      \context Staff ="flauti" \flauti
    >
    \context StaffGroup ="timpani" <
    \notes { 
       c1
    }
    >
  >

}

