\version "1.3.146"
\header{
texidoc="
Note head shapes are settable.  The stem endings should be adjusted
per note head.  If you want different note head styles on one stem,
you must create a special context called Thread.

Harmonic notes have a different shape and different
dimensions. Nevertheless, noteheads in both styles can be combined, on
either up or down stems.
"
}


\include "paper23.ly"

\score { \notes \relative c''{
% \property Voice.Stem \override #'thickness = #5.0
\property Voice.NoteHead \set #'style = #'default
c4 c2 c8  c16 c16 c1 c\breve b4 b2 b8  b16 b16 b1 b\breve \break
\property Voice.NoteHead \set #'style = #'diamond
c4 c2 c8  c16 c16 c1 c\breve b4 b2 b8  b16 b16 b1 b\breve \break
\property Voice.NoteHead \set #'style = #'transparent
c4 c2 c8  c16 c16 c1 c\breve b4 b2 b8  b16 b16 b1 b\breve \break
\property Voice.NoteHead \set #'style = #'cross
c4 c2 c8  c16 c16 c1 c\breve b4 b2 b8  b16 b16 b1 b\breve \break
\property Voice.NoteHead \set #'style = #'xcircle
c4 c2 c8  c16 c16 c1 c\breve b4 b2 b8  b16 b16 b1 b\breve \break
\property Voice.NoteHead \set #'style = #'slash
c4 c2 c8  c16 c16 c1 c\breve b4 b2 b8  b16 b16 b1 b\breve \break
\property Voice.NoteHead \set #'style = #'mensural
c4 c2 c8  c16 c16  c1 c\breve c\longa b4 b2 b8  b16 b16 b1 b\breve b\longa \break
\property Voice.NoteHead \set #'style = #'harmonic
c4 c2 c8  c16 c16 c1 c\breve b4 b2 b8  b16 b16 b1 b\breve \break
\property Voice.NoteHead \set #'style = #'baroque
c4 c2 c8  c16 c16  c1 c\breve c\longa b4 b2 b8  b16 b16 b1 b\breve b\longa \break


   \context Voice = another <
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


   \context Voice <
     \context Thread = TA {
       \property Thread.NoteHead \set #'style = #'cross
       c4 c4 c4 c4 }
     \context Thread = TB {
       \property Thread.NoteHead \set #'style = #'mensural
       c'4 \stemDown c
       \property Thread.NoteHead \set #'style = #'slash
       \stemUp c4 \stemDown c
} >

}

    \paper {}
}
