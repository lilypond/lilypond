\version "1.9.0"
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

\score {
  \notes \transpose c c {
    \clef C

    \property Staff.NoteHead \set #'style = #'default
    <
      \context Voice = up {
        \property Voice.Stem \set #'direction = #1 %up
        e'16^\markup { "NoteHead \#'style = \#'default" }
        e'16 e'8 e'4 e'2 e'1 e'\breve e'\longa
      }
      \context Voice = down {
        \property Voice.Stem \set #'direction = #-1 %down
        a16 a16 a8 a4 a2 a1 a\breve a\longa
      }
    >
    \break

    \property Staff.NoteHead \set #'style = #'baroque
    <
      \context Voice = up {
        \property Voice.Stem \set #'direction = #1 %up
        e'16^\markup { "NoteHead \#'style = \#'baroque" }
        e'16 e'8 e'4 e'2 e'1 e'\breve e'\longa
      }
      \context Voice = down {
        \property Voice.Stem \set #'direction = #-1 %down
        a16 a16 a8 a4 a2 a1 a\breve a\longa
      }
    >
    \break

    \property Staff.NoteHead \set #'style = #'neo_mensural
    <
      \context Voice = up {
        \property Voice.Stem \set #'direction = #1 %up
        e'16^\markup { "NoteHead \#'style = \#'neo\_mensural" }
        e'16 e'8 e'4 e'2 e'1 e'\breve e'\longa
      }
      \context Voice = down {
        \property Voice.Stem \set #'direction = #-1 %down
        a16 a16 a8 a4 a2 a1 a\breve a\longa
      }
    >
    \break

    \property Staff.NoteHead \set #'style = #'mensural
    <
      \context Voice = up {
        \property Voice.Stem \set #'direction = #1 %up
        e'16^\markup { "NoteHead \#'style = \#'mensural" }
        e'16 e'8 e'4 e'2 e'1 e'\breve e'\longa
      }
      \context Voice = down {
        \property Voice.Stem \set #'direction = #-1 %down
        a16 a16 a8 a4 a2 a1 a\breve a\longa
      }
    >
    \break

    \property Staff.NoteHead \set #'style = #'harmonic
    <
      \context Voice = up {
        \property Voice.Stem \set #'direction = #1 %up
        e'16^\markup { "NoteHead \#'style = \#'harmonic" }
        e'16 e'8 e'4 e'2 e'1 e'\breve e'\longa
      }
      \context Voice = down {
        \property Voice.Stem \set #'direction = #-1 %down
        a16 a16 a8 a4 a2 a1 a\breve a\longa
      }
    >
    \break

    \property Staff.NoteHead \set #'style = #'diamond
    <
      \context Voice = up {
        \property Voice.Stem \set #'direction = #1 %up
        e'16^\markup { "NoteHead \#'style = \#'diamond" }
        e'16 e'8 e'4 e'2 e'1 e'\breve e'\longa
      }
      \context Voice = down {
        \property Voice.Stem \set #'direction = #-1 %down
        a16 a16 a8 a4 a2 a1 a\breve a\longa
      }
    >
    \break

    \property Staff.NoteHead \set #'style = #'cross
    <
      \context Voice = up {
        \property Voice.Stem \set #'direction = #1 %up
        e'16^\markup { "NoteHead \#'style = \#'cross" }
        e'16 e'8 e'4 e'2 e'1 e'\breve e'\longa
      }
      \context Voice = down {
        \property Voice.Stem \set #'direction = #-1 %down
        a16 a16 a8 a4 a2 a1 a\breve a\longa
      }
    >
    \break

    \property Staff.NoteHead \set #'style = #'xcircle
    <
      \context Voice = up {
        \property Voice.Stem \set #'direction = #1 %up
        e'16^\markup { "NoteHead \#'style = \#'xcircle" }
        e'16 e'8 e'4 e'2 e'1 e'\breve e'\longa
      }
      \context Voice = down {
        \property Voice.Stem \set #'direction = #-1 %down
        a16 a16 a8 a4 a2 a1 a\breve a\longa
      }
    >
    \break

    \property Staff.NoteHead \set #'style = #'triangle
    <
      \context Voice = up {
        \property Voice.Stem \set #'direction = #1 %up
        e'16^\markup { "NoteHead \#'style = \#'triangle" }
        e'16 e'8 e'4 e'2 e'1 e'\breve e'\longa
      }
      \context Voice = down {
        \property Voice.Stem \set #'direction = #-1 %down
        a16 a16 a8 a4 a2 a1 a\breve a\longa
      }
    >
    \break

    \property Staff.NoteHead \set #'style = #'slash
    <
      \context Voice = up {
        \property Voice.Stem \set #'direction = #1 %up
        e'16^\markup { "NoteHead \#'style = \#'slash" }
        e'16 e'8 e'4 e'2 e'1 e'\breve e'\longa
      }
      \context Voice = down {
        \property Voice.Stem \set #'direction = #-1 %down
        a16 a16 a8 a4 a2 a1 a\breve a\longa
      }
    >
    \break

    \property Staff.NoteHead \override #'transparent = ##t
    <
      \context Voice = up {
        \property Voice.Stem \set #'direction = #1 %up
        e'16^\markup { "NoteHead \#'transparent = \#\#t" }
        e'16 e'8 e'4 e'2 e'1 e'\breve e'\longa
      }
      \context Voice = down {
        \property Voice.Stem \set #'direction = #-1 %down
        a16 a16 a8 a4 a2 a1 a\breve a\longa
      }
    >
    \property Staff.NoteHead \revert #'transparent
    \break

    \context Voice = another <
      \context Thread = TA {
        \property Thread.NoteHead \set #'style = #'cross
        \property Voice.Stem \set #'direction = #1
        c'16
      }
      \context Thread = TB {
        \property Thread.NoteHead \set #'style = #'default a16
      }
      \context Thread = TC {
        \property Thread.NoteHead \set #'style = #'mensural d'16
      }
    >

    \context Voice <
      \context Thread = TA {
        \property Thread.NoteHead \set #'style = #'cross
        c'4 c'4 c'4 c'4
      }
      \context Thread = TB {
        \property Thread.NoteHead \set #'style = #'mensural
        c''4 \stemDown c''
        \property Thread.NoteHead \set #'style = #'slash
        \stemUp c''4 \stemDown c''
      }
    >
  }

  \paper {
    indent = 0.0
    raggedright = ##t
  }
}
