\version "2.1.7"
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

pattern = \notes <<
      \new Voice {
        \property Voice.Stem \set #'direction = #UP
        e'4
         e'2. e'1 e'\breve*1/2 e'\longa*1/4
      }
      \new Voice {
        \property Voice.Stem \set #'direction = #DOWN
         a4 a2. a1 a\breve*1/2 a\longa*1/4
      }
    >>

\score {
  \notes \transpose c c {
    \clef C

    \property Staff.NoteHead \set #'style = #'default
    s1*0^\markup { "default" }
    \pattern

    \property Staff.NoteHead \set #'style = #'baroque
    s1*0^\markup { "baroque" }
    \pattern
        \break

    \property Staff.NoteHead \set #'style = #'neo_mensural
    s1*0^\markup { "neomensural" }
    \pattern

    \property Staff.NoteHead \set #'style = #'mensural
    s1*0^\markup { "mensural" }
    \pattern
    
    \break

    \property Staff.NoteHead \set #'style = #'harmonic
    s1*0^\markup { "harmonic" }
    \pattern

    \property Staff.NoteHead \set #'style = #'diamond
    s1*0^\markup { "diamond" }
    \pattern
    \break

    \property Staff.NoteHead \set #'style = #'cross
    s1*0^\markup { "cross" }
    \pattern

    \property Staff.NoteHead \set #'style = #'xcircle
    s1*0^\markup { "xcircle" }
\pattern
    
    \break

    \property Staff.NoteHead \set #'style = #'triangle
    s1*0^\markup { "triangle" }
    \pattern
    

    \property Staff.NoteHead \set #'style = #'slash
    s1*0^\markup { "slash" }
    \pattern
    \break

    \new Voice <<
      \new Thread {
        \property Thread.NoteHead \set #'style = #'cross
        \property Voice.Stem \set #'direction = #1
        c'16
      }
      \new Thread {
        \property Thread.NoteHead \set #'style = #'default a16
      }
      \new Thread {
        \property Thread.NoteHead \set #'style = #'mensural d'16
      }
    >>
    \context Voice <<
      \new Thread {
        \property Thread.NoteHead \set #'style = #'cross
        c'4 c'4 c'4 c'4
      }
      \new Thread {
        \property Thread.NoteHead \set #'style = #'mensural
        c''4 \stemDown c''
        \property Thread.NoteHead \set #'style = #'slash
        \stemUp c''4 \stemDown c''
      }
    >>
  }

  \paper {
    indent = 0.0
    raggedright = ##t
  }
}
