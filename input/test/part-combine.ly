\version "1.3.146"

\header{
texidoc="
In orchestral scores and hymns, voices are traditionally combined onto
one staff.  LilyPond has a part combiner, that combines or separates two
voices according to actual rhythm and pitch.  User-defined texts such as
``solo'' and ``@`a2'' are typeset automagically, as appropriate.
"
}

\score{
  \context Staff = flauti <
    \time 4/4
    \context Voice=one \partcombine Voice
    \context Thread=one \notes\relative c'' {
      c4 d e f | b,4 d c d | r2 e4 f | c4 d e f |
      c4 r e f | c4 r e f | c4 r a r | a a r a |
      a2 \property Voice.soloADue = ##f a |
    }
    \context Thread=two \notes\relative c'' {
      g4 b d f | r2 c4 d | a c c d | a4. b8 c4 d
      c r e r | r2 s2 | a,4 r a r | a r r a |
      a2 \property Voice.soloADue = ##f a |
    }
  >
  \paper{
    linewidth = 80 * \staffspace
    \translator{
      \ThreadContext
      \consists Rest_engraver
    }
    \translator{
      \VoiceContext
      \remove Rest_engraver
    }
  }
}

