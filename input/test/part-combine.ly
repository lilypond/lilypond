
\version "2.3.8"

\header{ texidoc="@cindex Part Combine
In orchestral scores and hymns, voices are traditionally combined into
one staff.  LilyPond has a part combiner that combines, or separates, two
voices according to the actual rhythm and pitch.  Configurable texts, such 
as ``solo'' and ``@`a2'', are typeset automatically in appropriate
places. "
}

\score{
    \context Staff = flauti <<
    \time 4/4
    \partcombine
    \relative c'' {
      c4 d e f | b,4 d c d | r2 e4 f | c4 d e f |
      c4 r e f | c4 r e f | c4 r a r | a a r a |
      a2 \set soloADue = ##f a |
    }
    \relative c'' {
      g4 b d f | r2 c4 d | a c c d | a4. b8 c4 d
      c r e r | r2 s2 | a,4 r a r | a r r a |
      a2 \set soloADue = ##f a |
    }
  >>
  \paper{
	raggedright = ##t
  }
}


