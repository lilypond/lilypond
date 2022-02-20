\version "2.16.0"
\header
{
  texidoc = "Symmetric figures should lead to symmetric slurs."

}

\layout{
  ragged-right = ##t
  indent = #0
}

\relative c' <<
  \time 6/8
  \context Staff = "s1" {
    e8(e e) e(d e) e(c e) e(b e)
    \time 4/4
    e(e e e) e(d d e) e(c c e) e(b b e)
  }
  \context Staff = "s2" {
    f'8(f f) f(g f) f(a f) f(b f)
    f8(f f f) f(g g f) f(a a f) f(b b f)
  }
>>

