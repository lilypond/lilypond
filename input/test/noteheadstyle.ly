
\score { \notes \relative c{
% anyone wanna pop?
c''4 c2 c8  c16 c16 c1 c\breve
\property Voice.NoteHead \push #'style = #'diamond
c4 c2 c8  c16 c16  c1 c\breve
\property Voice.NoteHead \push #'style = #'transparent
c4 c2 c8  c16 c16  c1 c\breve
\property Voice.NoteHead \push #'style = #'cross
c4 c2 c8  c16 c16  c1 c\breve
\property Voice.NoteHead \push #'style = #'mensural
c4 c2 c8  c16 c16  c1 c\breve c\longa
\property Voice.NoteHead \push #'style = #'harmonic
c4 c2 c8  c16 c16  c1 c\breve
\property Voice.NoteHead \push #'style = #'baroque
c4 c2 c8  c16 c16  c1 c\breve c\longa


   \context Voice <
    \context Thread = TA
      {
        \property Thread.NoteHead \push #'style = #'cross
        \property Voice.Stem \push #'direction = #1
        c16
       }
    \context Thread = TB
      { \property Thread.NoteHead \push #'style = #'default a16  }

    \context Thread = TC
      { \property Thread.NoteHead \push #'style = #'mensural d16 }

  >


   \context Voice <\context Thread = TA {
   \property Thread.NoteHead \push #'style = #'default
   c4 c4 }
\context Thread = TB {
   \property Thread.NoteHead \push #'style = #'mensural
  c'4 \stemDown c
} >

}

    \paper {


}
}
