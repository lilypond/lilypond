\header{
texidoc="
Note head shapes are settable.  The stem endings should be adjusted
per note head.  If you want different note head styles on one stem,
you must create a special context called Thread.

Harmonic notes have a different shape and different
dimensions. Nevertheless, noteheads in both styles can be combined, on
either up or down stems.
";
}




\score { \notes \relative c{

c''4 c2 c8  c16 c16 c1 c\breve
\property Voice.NoteHead \set #'style = #'diamond
c4 c2 c8  c16 c16  c1 c\breve
\property Voice.NoteHead \set #'style = #'transparent
c4 c2 c8  c16 c16  c1 c\breve
\property Voice.NoteHead \set #'style = #'cross
c4 c2 c8  c16 c16  c1 c\breve
\property Voice.NoteHead \set #'style = #'mensural
c4 c2 c8  c16 c16  c1 c\breve c\longa
\property Voice.NoteHead \set #'style = #'harmonic
c4 c2 c8  c16 c16  c1 c\breve
\property Voice.NoteHead \set #'style = #'baroque
c4 c2 c8  c16 c16  c1 c\breve c\longa


   \context Voice <
    \context Thread = TA
      {
        \property Thread.NoteHead \set #'style = #'cross
        \property Voice.Stem \set #'direction = #1
        c16
       }
    \context Thread = TB
      { \property Thread.NoteHead \set #'style = #'default a16  }

    \context Thread = TC
      { \property Thread.NoteHead \set #'style = #'mensural d16 }

  >


   \context Voice <\context Thread = TA {
   \property Thread.NoteHead \set #'style = #'default
   c4 c4 }
\context Thread = TB {
   \property Thread.NoteHead \set #'style = #'mensural
  c'4 \stemDown c
} >

}

    \paper {


}
}
